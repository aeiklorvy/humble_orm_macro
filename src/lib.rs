use convert_case::{Case, Casing};
use quote::quote;
use std::{collections::BTreeMap, error::Error};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Driver {
    MySql,
    Postgre,
    Sqlite,
}

impl Driver {
    fn as_str(&self) -> &'static str {
        match self {
            Driver::MySql => "MySqlPool",
            Driver::Postgre => "PgPool",
            Driver::Sqlite => "SqlitePool",
        }
    }
}

#[proc_macro]
pub fn generate_structs_mysql(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_string = input.to_string();
    match parse_sql(&sqlparser::dialect::MySqlDialect {}, &input_string) {
        Ok(tables) => write_structs(tables, Driver::MySql),
        Err(err) => {
            let e = err.to_string();
            quote! { compile_error!(#e) }
        }
    }
    .into()
}

#[proc_macro]
pub fn generate_structs_psql(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_string = input.to_string();
    match parse_sql(&sqlparser::dialect::PostgreSqlDialect {}, &input_string) {
        Ok(tables) => write_structs(tables, Driver::Postgre),
        Err(err) => {
            let e = err.to_string();
            quote! { compile_error!(#e) }
        }
    }
    .into()
}

#[proc_macro]
pub fn generate_structs_sqlite(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_string = input.to_string();
    match parse_sql(&sqlparser::dialect::SQLiteDialect {}, &input_string) {
        Ok(tables) => write_structs(tables, Driver::Sqlite),
        Err(err) => {
            let e = err.to_string();
            quote! { compile_error!(#e) }
        }
    }
    .into()
}

#[derive(Default)]
struct TableColumn {
    name: String,
    type_: proc_macro2::TokenStream,
    is_primary: bool,
}

struct TableRelation {
    column: String,
    rel_table: String,
    rel_column: String,
}

#[derive(Default)]
struct TableDef {
    cols: Vec<TableColumn>,
    rels: Vec<TableRelation>,
}

// use BTreeMap to keep insertion order
type TableMap = BTreeMap<String, TableDef>;

fn parse_sql(
    dialect: &dyn sqlparser::dialect::Dialect,
    sql: &str,
) -> Result<TableMap, Box<dyn Error>> {
    let stmts = sqlparser::parser::Parser::parse_sql(dialect, sql)?;
    let mut tables = TableMap::new();

    for stmt in stmts {
        if let sqlparser::ast::Statement::CreateTable(sqlparser::ast::CreateTable {
            name,
            columns,
            constraints,
            ..
        }) = stmt
        {
            let table_name = name
                .to_string()
                .replace('`', "")
                .replace('\"', "")
                .replace('\'', "");

            let table_def = tables.entry(table_name).or_insert(TableDef::default());
            for col in columns {
                let is_primary = is_column_primary(&col);
                let is_nullable = is_column_nullable(&col);

                let mut column_type = convert_sql_type(col.data_type);
                if is_nullable {
                    column_type = quote! { Option<#column_type> };
                }
                table_def.cols.push(TableColumn {
                    name: col.name.value,
                    type_: column_type,
                    is_primary,
                });
            }

            for c in constraints {
                if let sqlparser::ast::TableConstraint::ForeignKey {
                    columns,
                    foreign_table,
                    referred_columns,
                    ..
                } = c
                {
                    let rel_table = foreign_table
                        .to_string()
                        .replace('`', "")
                        .replace('\"', "")
                        .replace('\'', "");
                    table_def.rels.push(TableRelation {
                        column: columns[0].value.clone(),
                        rel_table,
                        rel_column: referred_columns[0].value.clone(),
                    })
                }
            }
        }
    }

    Ok(tables)
}

fn write_structs(tables: TableMap, driver: Driver) -> proc_macro2::TokenStream {
    let mut structs = vec![];

    for (table, def) in tables {
        let table_escaped = format!("{table:?}");
        let t = quote::format_ident!("{}", table.to_case(Case::Pascal));
        let primary_col: Option<&TableColumn> = def.cols.iter().find(|c| c.is_primary);

        let mut struct_fields = vec![];
        let mut default_values = vec![];
        for column in &def.cols {
            let field_name = quote::format_ident!("{}", column.name);
            let col_type = &column.type_;

            struct_fields.push(quote! {
                pub #field_name: #col_type,
            });

            // can't write #[derive(Default)] because of the `time` crate,
            // which does not implement a Default trait for its types
            if col_type.to_string().ends_with("Date") {
                default_values.push(quote! {
                    #field_name: sqlx::types::time::Date::MIN,
                });
            } else if col_type.to_string().ends_with("PrimitiveDateTime") {
                default_values.push(quote! {
                    #field_name: sqlx::types::time::PrimitiveDateTime::MIN,
                });
            } else if col_type.to_string().ends_with("Time") {
                default_values.push(quote! {
                    #field_name: sqlx::types::time::Time::MIDNIGHT,
                });
            } else {
                default_values.push(quote! {
                    #field_name: Default::default(),
                });
            }
        }

        let mut meta_columns = vec![];
        for column in &def.cols {
            let field_name = quote::format_ident!("{}", column.name.to_case(Case::UpperSnake));
            // use debug trait to escape
            let col_name = format!("{:?}", column.name);
            meta_columns.push(quote! {
                pub const #field_name: SqlColumn = SqlColumn::new(#col_name, #table_escaped);
            });
        }
        meta_columns.push(quote! {
            pub const ALL: SqlColumn = SqlColumn::new("*", #table_escaped);
        });

        let related_funcs: Vec<_> = def
            .rels
            .iter()
            .map(|rel| gen_get_related_by(driver, &rel))
            .collect();
        let get_by_primary_key = match primary_col {
            Some(id_col) => gen_get_by_primary_key(driver, &table, &id_col.name, &id_col.type_),
            None => quote! {},
        };
        let update_fn = match primary_col {
            Some(id_col) => gen_update(driver, &table, &def.cols, &id_col.name),
            None => quote! {},
        };
        let delete_fn = match primary_col {
            Some(id_col) => gen_delete(driver, &table, &id_col.name),
            None => quote! {},
        };
        let insert_without_pk_fn = match primary_col {
            Some(id_col) => gen_insert_without_pk(driver, &table, &def.cols, id_col),
            None => quote! {},
        };
        let insert_fn = gen_insert(driver, &table, &def.cols);
        let select_func = gen_select_all(driver, &table);
        let cols_from_row = gen_column_from_row(driver, &def.cols);

        let primary_col_name: &str = primary_col.map(|col| col.name.as_ref()).unwrap_or_default();
        let primary_col_name_escaped = format!("{primary_col_name:?}");
        structs.push(quote! {
            #[derive(Clone, Debug, sqlx::FromRow)]
            pub struct #t {
                #(#struct_fields)*
            }

            impl SqlTable for #t {
                fn table_name() -> &'static str {
                    #table_escaped
                }
                fn id_column_name() -> &'static str {
                    #primary_col_name_escaped
                }
            }

            impl Default for #t {
                fn default() -> Self {
                    Self {
                        #(#default_values)*
                    }
                }
            }

            impl #t {
                #(#meta_columns)*
                #(#related_funcs)*
                #get_by_primary_key
                #select_func
                #update_fn
                #insert_fn
                #insert_without_pk_fn
                #delete_fn
                #cols_from_row
            }
        });
    }

    quote! { #(#structs)* }
}

fn convert_sql_type(dt: sqlparser::ast::DataType) -> proc_macro2::TokenStream {
    match dt {
        sqlparser::ast::DataType::Character(_)
        | sqlparser::ast::DataType::Char(_)
        | sqlparser::ast::DataType::CharacterVarying(_)
        | sqlparser::ast::DataType::CharVarying(_)
        | sqlparser::ast::DataType::Varchar(_)
        | sqlparser::ast::DataType::Nvarchar(_)
        | sqlparser::ast::DataType::CharacterLargeObject(_)
        | sqlparser::ast::DataType::CharLargeObject(_)
        | sqlparser::ast::DataType::Clob(_)
        | sqlparser::ast::DataType::Text
        | sqlparser::ast::DataType::TinyText
        | sqlparser::ast::DataType::MediumText
        | sqlparser::ast::DataType::LongText
        | sqlparser::ast::DataType::String(_)
        | sqlparser::ast::DataType::FixedString(_) => quote! { String },

        sqlparser::ast::DataType::Binary(_)
        | sqlparser::ast::DataType::Varbinary(_)
        | sqlparser::ast::DataType::Blob(_)
        | sqlparser::ast::DataType::TinyBlob
        | sqlparser::ast::DataType::MediumBlob
        | sqlparser::ast::DataType::LongBlob
        | sqlparser::ast::DataType::Bytes(_) => quote! { Vec<u8> },

        sqlparser::ast::DataType::Numeric(_)
        | sqlparser::ast::DataType::Decimal(_)
        | sqlparser::ast::DataType::DecimalUnsigned(_)
        | sqlparser::ast::DataType::BigNumeric(_)
        | sqlparser::ast::DataType::BigDecimal(_)
        | sqlparser::ast::DataType::Dec(_)
        | sqlparser::ast::DataType::DecUnsigned(_)
        | sqlparser::ast::DataType::Float(_)
        | sqlparser::ast::DataType::FloatUnsigned(_)
        | sqlparser::ast::DataType::Real
        | sqlparser::ast::DataType::RealUnsigned
        | sqlparser::ast::DataType::Float8
        | sqlparser::ast::DataType::Double(_)
        | sqlparser::ast::DataType::DoubleUnsigned(_)
        | sqlparser::ast::DataType::DoublePrecision
        | sqlparser::ast::DataType::DoublePrecisionUnsigned => quote! { f64 },

        sqlparser::ast::DataType::USmallInt
        | sqlparser::ast::DataType::IntUnsigned(_)
        | sqlparser::ast::DataType::Int4Unsigned(_)
        | sqlparser::ast::DataType::MediumIntUnsigned(_)
        | sqlparser::ast::DataType::Int2Unsigned(_)
        | sqlparser::ast::DataType::UTinyInt
        | sqlparser::ast::DataType::TinyIntUnsigned(_)
        | sqlparser::ast::DataType::SmallIntUnsigned(_)
        | sqlparser::ast::DataType::IntegerUnsigned(_) => quote! { u64 },

        sqlparser::ast::DataType::TinyInt(_)
        | sqlparser::ast::DataType::Int2(_)
        | sqlparser::ast::DataType::SmallInt(_)
        | sqlparser::ast::DataType::MediumInt(_)
        | sqlparser::ast::DataType::Signed
        | sqlparser::ast::DataType::SignedInteger
        | sqlparser::ast::DataType::Int(_)
        | sqlparser::ast::DataType::Integer(_) => quote! { i64 },

        sqlparser::ast::DataType::Bool | sqlparser::ast::DataType::Boolean => {
            quote! { bool }
        }

        sqlparser::ast::DataType::BigInt(_)
        | sqlparser::ast::DataType::BigIntUnsigned(_)
        | sqlparser::ast::DataType::UBigInt
        | sqlparser::ast::DataType::Unsigned
        | sqlparser::ast::DataType::UnsignedInteger => quote! { u64 },

        sqlparser::ast::DataType::Date => quote! { sqlx::types::time::Date },
        sqlparser::ast::DataType::Time(_, _) => quote! { sqlx::types::time::Time },
        sqlparser::ast::DataType::Datetime(_) => {
            quote! { sqlx::types::time::PrimitiveDateTime }
        }
        sqlparser::ast::DataType::Timestamp(_, _) => {
            quote! { sqlx::types::time::PrimitiveDateTime }
        }

        sqlparser::ast::DataType::Custom(name, _) => {
            let type_name = name.to_string().to_uppercase();
            match type_name.as_str() {
                "SMALLSERIAL" | "SERIAL" => quote! { i32 },
                "BIGSERIAL" => quote! { i64 },
                _ => quote! { String },
            }
        }

        _ => quote! { String },
    }
}

fn is_column_primary(col: &sqlparser::ast::ColumnDef) -> bool {
    for opt in &col.options {
        if let sqlparser::ast::ColumnOption::Unique { is_primary, .. } = opt.option {
            return is_primary;
        }
    }
    false
}

fn is_column_nullable(col: &sqlparser::ast::ColumnDef) -> bool {
    for opt in &col.options {
        if matches!(opt.option, sqlparser::ast::ColumnOption::NotNull) {
            return false;
        }
    }
    true
}

fn gen_get_by_primary_key(
    driver: Driver,
    table: &str,
    primary_col_name: &str,
    key_type: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let pool = quote::format_ident!("{}", driver.as_str());
    let sql = format!("SELECT * FROM {table:?} WHERE {primary_col_name:?} = ?");

    quote! {
        /// Selects a row from the database where the primary key corresponds
        /// to the specified value
        pub async fn get_by_primary_key(pool: &sqlx::#pool, key: #key_type) -> Option<Self> {
            sqlx::query_as(#sql)
                .bind(&key)
                .fetch_one(pool)
                .await
                .ok()
        }
    }
}

fn gen_get_related_by(driver: Driver, rel: &TableRelation) -> proc_macro2::TokenStream {
    let pool = quote::format_ident!("{}", driver.as_str());
    let fn_name = quote::format_ident!("get_related_by_{}", rel.column.to_case(Case::Snake));
    let fn_name_all =
        quote::format_ident!("get_all_related_by_{}", rel.column.to_case(Case::Snake));
    let model = quote::format_ident!("{}", rel.rel_table.to_case(Case::Pascal));
    let ref_field = quote::format_ident!("{}", rel.column.to_case(Case::Snake));

    let sql_one = format!(
        "SELECT * FROM {:?} WHERE {:?} = ? LIMIT 1",
        rel.rel_table, rel.rel_column
    );
    let sql_many = format!(
        "SELECT * FROM {:?} WHERE {:?} = ?",
        rel.rel_table, rel.rel_column
    );

    quote! {
        /// Selects a row of the related table according to the foreign key
        /// and field value in the model
        pub async fn #fn_name(&self, pool: &sqlx::#pool) -> Option<#model> {
            sqlx::query_as(#sql_one)
                .bind(&self.#ref_field)
                .fetch_one(pool)
                .await
                .ok()
        }
        /// Selects all rows of the related table according to the foreign key
        /// and field value in the model
        pub async fn #fn_name_all(&self, pool: &sqlx::#pool) -> Vec<#model> {
            sqlx::query_as(#sql_many)
                .bind(&self.#ref_field)
                .fetch_all(pool)
                .await
                .unwrap_or_default()
        }
    }
}

fn gen_select_all(driver: Driver, table: &str) -> proc_macro2::TokenStream {
    let pool = quote::format_ident!("{}", driver.as_str());
    let sql = format!("SELECT * FROM {table:?}");

    quote! {
        /// Selects all table rows from the database
        pub async fn select_all(pool: &sqlx::#pool) -> Vec<Self> {
            sqlx::query_as(#sql)
                .fetch_all(pool)
                .await
                .unwrap_or_default()
        }
    }
}

fn gen_update(
    driver: Driver,
    table: &str,
    table_cols: &[TableColumn],
    primary_col_name: &str,
) -> proc_macro2::TokenStream {
    let update_cols = table_cols
        .iter()
        .filter(|col| !col.is_primary)
        .map(|col| format!("{:?} = ?", col.name))
        .collect::<Vec<_>>()
        .join(",");

    let sql = format!("UPDATE {table:?} SET {update_cols} WHERE {primary_col_name:?} = ?");

    let mut bind_values: Vec<proc_macro2::TokenStream> = vec![];
    for col in table_cols {
        if col.is_primary {
            continue;
        }
        let field_name = quote::format_ident!("{}", col.name.to_case(Case::Snake));
        bind_values.push(quote! { self.#field_name });
    }
    let id_field = quote::format_ident!("{}", primary_col_name);
    bind_values.push(quote! { self.#id_field });

    let pool = quote::format_ident!("{}", driver.as_str());

    quote! {
        /// Updates a row in the database that corresponds to the value of the
        /// primary key field.
        pub async fn update(&self, pool: &sqlx::#pool) {
            sqlx::query(#sql)
                #( .bind(&#bind_values) )*
                .execute(pool)
                .await
                .unwrap();
        }
    }
}

fn gen_insert(driver: Driver, table: &str, table_cols: &[TableColumn]) -> proc_macro2::TokenStream {
    let fields = table_cols
        .iter()
        .map(|col| format!("{:?}", col.name))
        .collect::<Vec<_>>()
        .join(",");
    let placeholders = std::iter::repeat_n("?", table_cols.len())
        .collect::<Vec<_>>()
        .join(",");

    let sql = format!("INSERT INTO {table:?} ({fields}) VALUES ({placeholders})");

    let mut binds = vec![];
    for col in table_cols {
        let field_name = quote::format_ident!("{}", col.name);
        binds.push(quote! { self.#field_name });
    }

    let pool = quote::format_ident!("{}", driver.as_str());

    quote! {
        /// Inserts a record via `INSERT` by sending all columns of the model
        pub async fn insert(&self, pool: &sqlx::#pool) {
            sqlx::query(#sql)
                #( .bind(&#binds) )*
                .execute(pool)
                .await
                .unwrap();
        }
    }
}

fn gen_insert_without_pk(
    driver: Driver,
    table: &str,
    table_cols: &[TableColumn],
    primary_col: &TableColumn,
) -> proc_macro2::TokenStream {
    if matches!(driver, Driver::MySql | Driver::Sqlite)
        && !matches!(primary_col.type_.to_string().as_str(), "i64" | "u64")
    {
        // non-integer primary keys not supported, becasue of
        // `last_return_id` and `last_return_rowid` functions
        return quote! {};
    }

    let fields = table_cols
        .iter()
        .filter(|col| !col.is_primary)
        .map(|col| format!("{:?}", col.name))
        .collect::<Vec<_>>()
        .join(",");

    let placeholders = std::iter::repeat_n("?", table_cols.len() - 1)
        .collect::<Vec<_>>()
        .join(",");

    let mut sql = format!("INSERT INTO {table:?} ({fields}) VALUES ({placeholders})");

    let mut binds = vec![];
    for col in table_cols {
        if col.is_primary {
            continue;
        }
        let field_name = quote::format_ident!("{}", col.name);
        binds.push(quote! { self.#field_name });
    }

    let pool = quote::format_ident!("{}", driver.as_str());
    let id_field = quote::format_ident!("{}", primary_col.name.to_case(Case::Snake));

    let fn_body: proc_macro2::TokenStream = match driver {
        Driver::MySql => {
            quote! {
                let result: sqlx::mysql::MySqlQueryResult = sqlx::query(#sql)
                    #( .bind(&#binds) )*
                    .execute(pool)
                    .await
                    .unwrap();
                self.#id_field = result.last_insert_id().into();
            }
        }
        Driver::Postgre => {
            sql += &format!(" RETURNING {:?}", primary_col.name);
            quote! {
                use sqlx::Row;
                let row = sqlx::query(#sql)
                    #( .bind(&#binds) )*
                    .fetch_one(pool)
                    .await
                    .unwrap();
                self.#id_field = row.get(0);
            }
        }
        Driver::Sqlite => {
            quote! {
                let result: sqlx::sqlite::SqliteQueryResult = sqlx::query(#sql)
                    #( .bind(&#binds) )*
                    .execute(pool)
                    .await
                    .unwrap();
                self.#id_field = result.last_insert_rowid().into();
            }
        }
    };

    quote! {
        /// Inserts a record via `INSERT`, skipping the primary key field, and
        /// after insertion sets the primary key value from the DBMS to the
        /// model.
        pub async fn insert_generating_primary_key(&mut self, pool: &sqlx::#pool) {
            #fn_body
        }
    }
}

fn gen_delete(driver: Driver, table: &str, primary_col_name: &str) -> proc_macro2::TokenStream {
    let id_field = quote::format_ident!("{}", primary_col_name);
    let pool = quote::format_ident!("{}", driver.as_str());

    let sql = format!("DELETE FROM {table:?} WHERE {primary_col_name:?} = ?");

    quote! {
        pub async fn delete(self, pool: &sqlx::#pool) {
            sqlx::query(#sql)
                .bind(&self.#id_field)
                .execute(pool)
                .await
                .unwrap();
        }
    }
}

fn gen_column_from_row(driver: Driver, table_cols: &[TableColumn]) -> proc_macro2::TokenStream {
    let row_type = match driver {
        Driver::MySql => quote! { sqlx::mysql::MySqlRow },
        Driver::Postgre => quote! { sqlx::postgres::PgRow },
        Driver::Sqlite => quote! { sqlx::sqlite::SqliteRow },
    };

    let mut funcs = vec![];
    for col in table_cols {
        let get_fn_name = quote::format_ident!("{}_from_row", col.name.to_case(Case::Snake));
        let try_get_fn_name =
            quote::format_ident!("{}_try_from_row", col.name.to_case(Case::Snake));
        let col_type = &col.type_;
        let col_name = col.name.as_str();

        funcs.push(quote! {
            pub fn #get_fn_name(row: &#row_type) -> #col_type {
                use sqlx::Row;
                row.get(#col_name)
            }
            pub fn #try_get_fn_name(row: &#row_type) -> Result<#col_type, sqlx::Error> {
                use sqlx::Row;
                row.try_get(#col_name)
            }
        });
    }

    quote! { #(#funcs)* }
}
