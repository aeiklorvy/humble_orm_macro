//! the test how it handles different column types and names

use crate::{Driver, generate_structs_impl};

const SQL: &str = r#"
CREATE TABLE example (
    int_nullable INTEGER,
    INT_nonnull INTEGER NOT NULL,
    textNullable varchar(128),
    textNotNullable varchar(64) NOT NULL,
    price REAL NOT NULL,
    DATE_OR_NULL DATE,
    "DATE" DATE NOT NULL,
    datetimeOrNull DATETIME,
    date_time DATETIME NOT NULL,
    NullableTime TIME DEFAULT NULL,
    the_time TIME NOT NULL
);
"#;

#[test]
fn test_model_fields() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut struct_exists = false;
    for item in &generated.items {
        if let syn::Item::Struct(_) = item {
            struct_exists = true;

            let s: syn::Item = syn::parse2(quote::quote! {
                #[derive(Clone, Debug, PartialEq, sqlx::FromRow,)]
                pub struct Example {
                    pub int_nullable: Option<i64>,
                    #[sqlx(rename = "INT_nonnull")]
                    pub int_nonnull: i64,
                    #[sqlx(rename = "textNullable")]
                    pub text_nullable: Option<String>,
                    #[sqlx(rename = "textNotNullable")]
                    pub text_not_nullable: String,
                    pub price: f64,
                    #[sqlx(rename = "DATE_OR_NULL")]
                    pub date_or_null: Option<sqlx::types::time::Date>,
                    #[sqlx(rename = "DATE")]
                    pub date: sqlx::types::time::Date,
                    #[sqlx(rename = "datetimeOrNull")]
                    pub datetime_or_null: Option<sqlx::types::time::PrimitiveDateTime>,
                    pub date_time: sqlx::types::time::PrimitiveDateTime,
                    #[sqlx(rename = "NullableTime")]
                    pub nullable_time: Option<sqlx::types::time::Time>,
                    pub the_time: sqlx::types::time::Time,
                }
            })
            .unwrap();
            assert_eq!(item, &s);
        }
    }
    assert_eq!(struct_exists, true);
}

#[test]
fn test_default_impl() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut exists = false;
    for item in &generated.items {
        if let syn::Item::Impl(_) = item {
            let default_impl: syn::Item = syn::parse2(quote::quote! {
                impl Default for Example {
                    fn default() -> Self {
                        Self {
                            int_nullable: Default::default(),
                            int_nonnull: Default::default(),
                            text_nullable: Default::default(),
                            text_not_nullable: Default::default(),
                            price: Default::default(),
                            date_or_null: Default::default(),
                            date: unsafe { sqlx::types::time::Date::__from_ordinal_date_unchecked(0, 1) },
                            datetime_or_null: Default::default(),
                            date_time: sqlx::types::time::PrimitiveDateTime::new(
                                        unsafe { sqlx::types::time::Date::__from_ordinal_date_unchecked(0, 1) },
                                        sqlx::types::time::Time::MIDNIGHT
                                    ),
                            nullable_time: Default::default(),
                            the_time: sqlx::types::time::Time::MIDNIGHT,
                        }
                    }
                }
            }).unwrap();

            // do not assert, there may be other impls
            if &default_impl == item {
                exists = true;
            }
        }
    }
    assert_eq!(exists, true);
}

#[test]
fn test_sqltable_impl() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut exists = false;
    for item in &generated.items {
        if let syn::Item::Impl(_) = item {
            let sqltable_impl: syn::Item = syn::parse2(quote::quote! {
                impl SqlTable for Example {
                    const TABLE_NAME: &'static str = "\"example\"";
                    const COLUMNS: &'static [SqlColumn] = &[
                        Self::IntNullable,
                        Self::IntNonnull,
                        Self::TextNullable,
                        Self::TextNotNullable,
                        Self::Price,
                        Self::DateOrNull,
                        Self::Date,
                        Self::DatetimeOrNull,
                        Self::DateTime,
                        Self::NullableTime,
                        Self::TheTime,
                    ];
                }
            })
            .unwrap();

            // do not assert, there may be other impls
            if &sqltable_impl == item {
                exists = true;
            }
        }
    }
    assert_eq!(exists, true);
}

#[test]
fn test_model_impl() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut exists = false;
    for item in &generated.items {
        if let syn::Item::Impl(item_impl) = item {
            if item_impl.trait_.is_none() {
                exists = true;

                let impl_ = syn::parse2(quote::quote!{
                    #[allow(non_upper_case_globals)]
                    impl Example {
                        /// Information about the table column
                        pub const IntNullable: SqlColumn =
                            unsafe { SqlColumn::new("\"int_nullable\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const IntNonnull: SqlColumn =
                            unsafe { SqlColumn::new("\"INT_nonnull\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const TextNullable: SqlColumn =
                            unsafe { SqlColumn::new("\"textNullable\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const TextNotNullable: SqlColumn =
                            unsafe { SqlColumn::new("\"textNotNullable\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const Price: SqlColumn = unsafe { SqlColumn::new("\"price\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const DateOrNull: SqlColumn =
                            unsafe { SqlColumn::new("\"DATE_OR_NULL\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const Date: SqlColumn = unsafe { SqlColumn::new("\"DATE\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const DatetimeOrNull: SqlColumn =
                            unsafe { SqlColumn::new("\"datetimeOrNull\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const DateTime: SqlColumn =
                            unsafe { SqlColumn::new("\"date_time\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const NullableTime: SqlColumn =
                            unsafe { SqlColumn::new("\"NullableTime\"", "\"example\"", false) };
                        /// Information about the table column
                        pub const TheTime: SqlColumn = unsafe { SqlColumn::new("\"the_time\"", "\"example\"", false) };
                        #[doc = "A special column representing the entire table: `\"example\".*`"]
                        pub const ALL: SqlColumn = unsafe { SqlColumn::new("*", "\"example\"", false) };
                        /// Selects all table rows from the database
                        pub async fn select_all(
                            pool: &sqlx::SqlitePool
                        ) -> std::result::Result<std::vec::Vec<Self>, sqlx::Error> {
                            sqlx::query_as("SELECT * FROM \"example\"")
                                .fetch_all(pool)
                                .await
                        }
                        /// Inserts a record via `INSERT` by sending all columns of the model
                        pub async fn insert(&self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query ("INSERT INTO \"example\" (\"int_nullable\",\"INT_nonnull\",\"textNullable\",\"textNotNullable\",\"price\",\"DATE_OR_NULL\",\"DATE\",\"datetimeOrNull\",\"date_time\",\"NullableTime\",\"the_time\") VALUES (?,?,?,?,?,?,?,?,?,?,?)")
                                .bind(&self.int_nullable)
                                .bind(&self.int_nonnull)
                                .bind(&self.text_nullable)
                                .bind(&self.text_not_nullable)
                                .bind(&self.price)
                                .bind(&self.date_or_null)
                                .bind(&self.date)
                                .bind(&self.datetime_or_null)
                                .bind(&self.date_time)
                                .bind(&self.nullable_time)
                                .bind(&self.the_time)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Creates a new table in the database
                        pub async fn create_table(pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("CREATE TABLE example (int_nullable INTEGER, INT_nonnull INTEGER NOT NULL, textNullable VARCHAR(128), textNotNullable VARCHAR(64) NOT NULL, price REAL NOT NULL, DATE_OR_NULL DATE, \"DATE\" DATE NOT NULL, datetimeOrNull DATETIME, date_time DATETIME NOT NULL, NullableTime TIME DEFAULT NULL, the_time TIME NOT NULL)").execute(pool).await?;
                            Ok(())
                        }
                        /// Deletes the entire table from the database, does nothing if there
                        /// is no such table
                        pub async fn drop_table(pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("DROP TABLE IF EXISTS \"example\"")
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                    }
                }).unwrap();

                assert_eq!(item_impl, &impl_);
            }
        }
    }
    assert_eq!(exists, true);
}

#[test]
fn test_types_mod() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut mod_exists = false;
    for item in &generated.items {
        if let syn::Item::Mod(_) = item {
            mod_exists = true;

            let mod_: syn::Item = syn::parse2(quote::quote! {
                pub mod ExampleColumnTypes {
                    pub type IntNullableType = Option<i64>;
                    pub type IntNonnullType = i64;
                    pub type TextNullableType = Option<String>;
                    pub type TextNotNullableType = String;
                    pub type PriceType = f64;
                    pub type DateOrNullType = Option<sqlx::types::time::Date>;
                    pub type DateType = sqlx::types::time::Date;
                    pub type DatetimeOrNullType = Option<sqlx::types::time::PrimitiveDateTime>;
                    pub type DateTimeType = sqlx::types::time::PrimitiveDateTime;
                    pub type NullableTimeType = Option<sqlx::types::time::Time>;
                    pub type TheTimeType = sqlx::types::time::Time;
                }
            })
            .unwrap();
            assert_eq!(item, &mod_);
        }
    }
    assert_eq!(mod_exists, true);
}
