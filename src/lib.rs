use convert_case::{Case, Casing};
use quote::quote;
use std::{collections::BTreeMap, error::Error};

#[cfg(test)]
mod tests;

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

    fn as_dialect<'a>(&self) -> &'a dyn sqlparser::dialect::Dialect {
        match self {
            Driver::MySql => &sqlparser::dialect::MySqlDialect {},
            Driver::Postgre => &sqlparser::dialect::PostgreSqlDialect {},
            Driver::Sqlite => &sqlparser::dialect::SQLiteDialect {},
        }
    }
}

#[proc_macro]
pub fn generate_structs_mysql(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_string = input.to_string();
    generate_structs_impl(Driver::MySql, &input_string).into()
}

#[proc_macro]
pub fn generate_structs_psql(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_string = input.to_string();
    generate_structs_impl(Driver::Postgre, &input_string).into()
}

#[proc_macro]
pub fn generate_structs_sqlite(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input_string = input.to_string();
    generate_structs_impl(Driver::Sqlite, &input_string).into()
}

fn generate_structs_impl(driver: Driver, sql: &str) -> proc_macro2::TokenStream {
    match parse_sql(driver.as_dialect(), sql) {
        Ok(tables) => write_structs(tables, driver),
        Err(err) => {
            let e = err.to_string();
            quote! { compile_error!(#e) }
        }
    }
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

struct TableDef {
    cols: Vec<TableColumn>,
    rels: Vec<TableRelation>,
    create_stmt: sqlparser::ast::Statement,
}

impl TableDef {
    fn primary_col(&self) -> Option<&TableColumn> {
        self.cols.iter().find(|col| col.is_primary)
    }

    fn primary_col_name(&self) -> Option<&str> {
        self.primary_col().map(|col| col.name.as_str())
    }
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
        }) = &stmt
        {
            let table_name = name
                .to_string()
                .replace('`', "")
                .replace('\"', "")
                .replace('\'', "");

            let table_def = tables.entry(table_name).or_insert(TableDef {
                cols: vec![],
                rels: vec![],
                create_stmt: stmt.clone(),
            });
            for col in columns {
                let is_primary = is_column_primary(col);
                let is_nullable = is_column_nullable(col);

                let mut column_type = convert_sql_type(&col.data_type);
                if is_nullable {
                    column_type = quote! { Option<#column_type> };
                }
                table_def.cols.push(TableColumn {
                    name: col.name.value.clone(),
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

        let fields = gen_model_fields(&def);
        let default_impl = gen_default_impl(&table, &def);
        let column_entities = gen_sql_columns(&table, &def);

        let mut all_columns = vec![];
        for column in &def.cols {
            // it represents information about the column's entity, so it's
            // more logical to present it in PascalCase (as with data types)
            let field_name = quote::format_ident!("{}", column.name.to_case(Case::Pascal));
            all_columns.push(quote! { #field_name });
        }

        let fns_related: Vec<_> = def
            .rels
            .iter()
            .map(|rel| gen_get_related_by(driver, &rel))
            .collect();
        let fn_get_by_primary_key = gen_get_by_primary_key(driver, &table, &def);
        let fn_update = gen_update(driver, &table, &def);
        let fn_delete = gen_delete(driver, &table, &def);
        let fn_insert_without_pk = gen_insert_without_pk(driver, &table, &def);
        let fn_insert = gen_insert(driver, &table, &def);
        let fn_select = gen_select_all(driver, &table);
        let fn_create = gen_create_table(driver, &def);
        let fn_drop = gen_drop_table(driver, &table);

        let type_mod = gen_column_type_mod(&table, &def);

        let t = quote::format_ident!("{}", table.to_case(Case::Pascal));
        structs.push(quote! {
            #[derive(Clone, Debug, sqlx::FromRow)]
            pub struct #t {
                #(#fields)*
            }

            #default_impl

            impl SqlTable for #t {
                const TABLE_NAME: &'static str = #table_escaped;
                const COLUMNS: &'static [SqlColumn] = &[#( Self:: #all_columns , )*];
            }

            #[allow(non_upper_case_globals)]
            impl #t {
                #(#column_entities)*
                #(#fns_related)*
                #fn_get_by_primary_key
                #fn_select
                #fn_update
                #fn_insert
                #fn_insert_without_pk
                #fn_delete
                #fn_create
                #fn_drop
            }

            #type_mod
        });
    }

    quote! { #(#structs)* }
}

fn convert_sql_type(dt: &sqlparser::ast::DataType) -> proc_macro2::TokenStream {
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

fn gen_model_fields(table_def: &TableDef) -> Vec<proc_macro2::TokenStream> {
    let mut struct_fields = vec![];
    for column in &table_def.cols {
        let cased_name = column.name.to_case(Case::Snake);
        let field_name = quote::format_ident!("{}", cased_name);
        let col_type = &column.type_;

        if cased_name == column.name {
            struct_fields.push(quote! {
                pub #field_name: #col_type,
            });
        } else {
            let sql_name = column.name.as_str();
            struct_fields.push(quote! {
                #[sqlx(rename = #sql_name)]
                pub #field_name: #col_type,
            });
        }
    }

    struct_fields
}

fn gen_default_impl(table: &str, table_def: &TableDef) -> proc_macro2::TokenStream {
    // can't write #[derive(Default)] because of the `time` crate,
    // which does not implement a Default trait for its types

    let mut default_values = vec![];
    for column in &table_def.cols {
        let field_name = quote::format_ident!("{}", column.name.to_case(Case::Snake));
        let col_type = column.type_.to_string();

        if col_type == quote! {sqlx::types::time::Date}.to_string() {
            // default is "0000-01-01"
            default_values.push(quote! {
                    #field_name: unsafe { sqlx::types::time::Date::__from_ordinal_date_unchecked(0, 1) },
                });
        } else if col_type == quote! {sqlx::types::time::PrimitiveDateTime}.to_string() {
            // default is "0000-01-01 00:00:00"
            default_values.push(quote! {
                #field_name: sqlx::types::time::PrimitiveDateTime::new(
                    unsafe { sqlx::types::time::Date::__from_ordinal_date_unchecked(0, 1) },
                    sqlx::types::time::Time::MIDNIGHT
                ),
            });
        } else if col_type == quote! {sqlx::types::time::Time}.to_string() {
            // default is "00:00:00"
            default_values.push(quote! {
                #field_name: sqlx::types::time::Time::MIDNIGHT,
            });
        } else {
            default_values.push(quote! {
                #field_name: Default::default(),
            });
        }
    }

    let t = quote::format_ident!("{}", table.to_case(Case::Pascal));
    quote! {
        impl Default for #t {
            fn default() -> Self {
                Self {
                    #(#default_values)*
                }
            }
        }
    }
}

fn gen_sql_columns(table: &str, table_def: &TableDef) -> Vec<proc_macro2::TokenStream> {
    let table_escaped = format!("{table:?}");
    let mut consts = vec![];
    for column in &table_def.cols {
        // it represents information about the column's entity, so it's
        // more logical to present it in PascalCase (as with data types)
        let field_name = quote::format_ident!("{}", column.name.to_case(Case::Pascal));
        // use debug trait to escape
        let col_name = format!("{:?}", column.name);
        let is_primary = match column.is_primary {
            true => quote! { true },
            false => quote! { false },
        };
        consts.push(quote! {
            /// Information about the table column
            pub const #field_name: SqlColumn = unsafe {
                SqlColumn::new(#col_name, #table_escaped, #is_primary)
            };
        });
    }

    let docs = format!("A special column representing the entire table: `{table:?}.*`");
    consts.push(quote! {
        #[doc = #docs]
        pub const ALL: SqlColumn = unsafe {
            SqlColumn::new("*", #table_escaped, false)
        };
    });

    consts
}

fn gen_get_by_primary_key(
    driver: Driver,
    table: &str,
    table_def: &TableDef,
) -> proc_macro2::TokenStream {
    if let Some(id_col) = table_def.primary_col() {
        let primary_col_name = id_col.name.as_str();
        let key_type = &id_col.type_;
        let pool = quote::format_ident!("{}", driver.as_str());
        let sql = format!("SELECT * FROM {table:?} WHERE {primary_col_name:?} = ?");

        quote! {
            /// Selects a row from the database where the primary key corresponds
            /// to the specified value
            pub async fn get_by_primary_key(pool: &sqlx::#pool, key: #key_type) -> std::result::Result<Self, sqlx::Error> {
                sqlx::query_as(#sql)
                    .bind(&key)
                    .fetch_one(pool)
                    .await
            }
        }
    } else {
        quote! {}
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
        pub async fn #fn_name(&self, pool: &sqlx::#pool) -> std::result::Result<#model, sqlx::Error> {
            sqlx::query_as(#sql_one)
                .bind(&self.#ref_field)
                .fetch_one(pool)
                .await
        }
        /// Selects all rows of the related table according to the foreign key
        /// and field value in the model
        pub async fn #fn_name_all(&self, pool: &sqlx::#pool) -> std::result::Result<std::vec::Vec<#model>, sqlx::Error> {
            sqlx::query_as(#sql_many)
                .bind(&self.#ref_field)
                .fetch_all(pool)
                .await
        }
    }
}

fn gen_select_all(driver: Driver, table: &str) -> proc_macro2::TokenStream {
    let pool = quote::format_ident!("{}", driver.as_str());
    let sql = format!("SELECT * FROM {table:?}");

    quote! {
        /// Selects all table rows from the database
        pub async fn select_all(pool: &sqlx::#pool) -> std::result::Result<std::vec::Vec<Self>, sqlx::Error> {
            sqlx::query_as(#sql)
                .fetch_all(pool)
                .await
        }
    }
}

fn gen_update(driver: Driver, table: &str, table_def: &TableDef) -> proc_macro2::TokenStream {
    if table_def.primary_col().is_none() {
        return quote! {};
    }

    let primary_col_name = table_def.primary_col_name().unwrap_or_default();
    let update_cols = table_def
        .cols
        .iter()
        .filter(|col| !col.is_primary)
        .map(|col| format!("{:?} = ?", col.name))
        .collect::<Vec<_>>()
        .join(",");

    let sql = format!("UPDATE {table:?} SET {update_cols} WHERE {primary_col_name:?} = ?");

    let mut bind_values: Vec<proc_macro2::TokenStream> = vec![];
    for col in &table_def.cols {
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
        /// primary key field
        pub async fn update(&self, pool: &sqlx::#pool) -> std::result::Result<(), sqlx::Error> {
            sqlx::query(#sql)
                #( .bind(&#bind_values) )*
                .execute(pool)
                .await?;
            Ok(())
        }
    }
}

fn gen_insert(driver: Driver, table: &str, table_def: &TableDef) -> proc_macro2::TokenStream {
    let fields = table_def
        .cols
        .iter()
        .map(|col| format!("{:?}", col.name))
        .collect::<Vec<_>>()
        .join(",");
    let placeholders = std::iter::repeat_n("?", table_def.cols.len())
        .collect::<Vec<_>>()
        .join(",");

    let sql = format!("INSERT INTO {table:?} ({fields}) VALUES ({placeholders})");

    let mut binds = vec![];
    for col in &table_def.cols {
        let field_name = quote::format_ident!("{}", col.name.to_case(Case::Snake));
        binds.push(quote! { self.#field_name });
    }

    let pool = quote::format_ident!("{}", driver.as_str());

    quote! {
        /// Inserts a record via `INSERT` by sending all columns of the model
        pub async fn insert(&self, pool: &sqlx::#pool) -> std::result::Result<(), sqlx::Error> {
            sqlx::query(#sql)
                #( .bind(&#binds) )*
                .execute(pool)
                .await?;
            Ok(())
        }
    }
}

fn gen_insert_without_pk(
    driver: Driver,
    table: &str,
    table_def: &TableDef,
) -> proc_macro2::TokenStream {
    if table_def.primary_col().is_none() {
        return quote! {};
    }
    // SAFETY: this has already been checked in the line above
    let primary_col = unsafe { table_def.primary_col().unwrap_unchecked() };

    if matches!(driver, Driver::MySql | Driver::Sqlite)
        && primary_col.type_.to_string() != quote! {i64}.to_string()
        && primary_col.type_.to_string() != quote! {u64}.to_string()
        && primary_col.type_.to_string() != quote! {Option<i64>}.to_string()
        && primary_col.type_.to_string() != quote! {Option<u64>}.to_string()
    {
        // non-integer primary keys not supported, becasue of
        // `last_return_id` and `last_return_rowid` functions
        return quote! {};
    }

    let fields = table_def
        .cols
        .iter()
        .filter(|col| !col.is_primary)
        .map(|col| format!("{:?}", col.name))
        .collect::<Vec<_>>()
        .join(",");

    // `len - 1` because it is known for sure that the primary key is there
    let placeholders = std::iter::repeat_n("?", table_def.cols.len() - 1)
        .collect::<Vec<_>>()
        .join(",");

    let mut sql = format!("INSERT INTO {table:?} ({fields}) VALUES ({placeholders})");

    let mut binds = vec![];
    for col in &table_def.cols {
        if col.is_primary {
            continue;
        }
        let field_name = quote::format_ident!("{}", col.name.to_case(Case::Snake));
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
                    .await?;
                self.#id_field = result.last_insert_id().into();
                Ok(())
            }
        }
        Driver::Postgre => {
            sql += &format!(" RETURNING {:?}", primary_col.name);
            quote! {
                use sqlx::Row;
                let row = sqlx::query(#sql)
                    #( .bind(&#binds) )*
                    .fetch_one(pool)
                    .await?;
                self.#id_field = row.get(0);
                Ok(())
            }
        }
        Driver::Sqlite => {
            quote! {
                let result: sqlx::sqlite::SqliteQueryResult = sqlx::query(#sql)
                    #( .bind(&#binds) )*
                    .execute(pool)
                    .await?;
                self.#id_field = result.last_insert_rowid().into();
                Ok(())
            }
        }
    };

    quote! {
        /// Inserts a record via `INSERT`, skipping the primary key field, and
        /// after insertion sets the primary key value from the DBMS to the
        /// model
        pub async fn insert_generating_primary_key(&mut self, pool: &sqlx::#pool) -> std::result::Result<(), sqlx::Error> {
            #fn_body
        }
    }
}

fn gen_delete(driver: Driver, table: &str, table_def: &TableDef) -> proc_macro2::TokenStream {
    if let Some(id_col) = table_def.primary_col() {
        let id_field = quote::format_ident!("{}", id_col.name);
        let pool = quote::format_ident!("{}", driver.as_str());

        let sql = format!("DELETE FROM {table:?} WHERE {:?} = ?", id_col.name);

        quote! {
            /// Deletes a row in the database that corresponds to the value of
            /// the primary key field, and the model will be consumed
            pub async fn delete(self, pool: &sqlx::#pool) -> std::result::Result<(), sqlx::Error> {
                sqlx::query(#sql)
                    .bind(&self.#id_field)
                    .execute(pool)
                    .await?;
                Ok(())
            }
        }
    } else {
        quote! {}
    }
}

fn gen_column_type_mod(table: &str, table_def: &TableDef) -> proc_macro2::TokenStream {
    // Right now, I can't just add type alias to the implementation,
    // because inherent associated types are unstable:
    // impl #table {
    //     pub type #col = #type;
    // }
    //
    // Therefore, I am generating a separate module that contains
    // type aliases for each column

    let mut items = vec![];
    for column in &table_def.cols {
        let name = quote::format_ident!("{}Type", column.name.to_case(Case::Pascal));
        let col_type = &column.type_;
        items.push(quote! {
            pub type #name = #col_type;
        });
    }

    let mod_name = quote::format_ident!("{}ColumnTypes", table.to_case(Case::Pascal));
    quote! {
        pub mod #mod_name {
            #( #items )*
        }
    }
}

fn gen_create_table(driver: Driver, table_def: &TableDef) -> proc_macro2::TokenStream {
    let pool = quote::format_ident!("{}", driver.as_str());
    let sql = table_def.create_stmt.to_string();

    quote! {
        /// Creates a new table in the database
        pub async fn create_table(pool: &sqlx::#pool) -> std::result::Result<(), sqlx::Error> {
            sqlx::query(#sql).execute(pool).await?;
            Ok(())
        }
    }
}

fn gen_drop_table(driver: Driver, table: &str) -> proc_macro2::TokenStream {
    let pool = quote::format_ident!("{}", driver.as_str());
    let sql = format!("DROP TABLE IF EXISTS {table:?}");

    quote! {
        /// Deletes the entire table from the database, does nothing if there
        /// is no such table
        pub async fn drop_table(pool: &sqlx::#pool) -> std::result::Result<(), sqlx::Error> {
            sqlx::query(#sql).execute(pool).await?;
            Ok(())
        }
    }
}
