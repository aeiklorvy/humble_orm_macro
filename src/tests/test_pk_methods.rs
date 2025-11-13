//! the test how it handles primary key functions

use crate::{Driver, generate_structs_impl};

const SQL_1: &str = r#"
CREATE TABLE Customer (
    id integer primary key
);
"#;

const SQL_2: &str = r#"
CREATE TABLE Customer (
    customer_id integer primary key
);
"#;

#[test]
fn test_model_impl_1() {
    let x = generate_structs_impl(Driver::Sqlite, SQL_1);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut exists = false;
    for item in &generated.items {
        if let syn::Item::Impl(item_impl) = item {
            if item_impl.trait_.is_none() {
                exists = true;

                let impl_ = syn::parse2(quote::quote! {
                    #[allow(non_upper_case_globals)]
                    impl Customer {
                        /// Information about the table column
                        pub const Id: SqlColumn = unsafe { SqlColumn::new("\"id\"", "\"Customer\"", true) };
                        #[doc = "A special column representing the entire table: `\"Customer\".*`"]
                        pub const ALL: SqlColumn = unsafe { SqlColumn::new("*", "\"Customer\"", false) };
                        /// Selects a row from the database where the primary key corresponds
                        /// to the specified value
                        ///
                        /// Internally, it uses prepared statements, so this is the most
                        /// preferred way to get a single row of the table using the
                        /// primary key
                        pub async fn get_by_id(
                            pool: &sqlx::SqlitePool,
                            value: i64
                        ) -> std::result::Result<Self, sqlx::Error> {
                            sqlx::query_as("SELECT * FROM \"Customer\" WHERE \"id\" = ? LIMIT 1")
                                .bind(&value)
                                .fetch_one(pool)
                                .await
                        }
                        /// Selects all table rows from the database
                        pub async fn select_all(
                            pool: &sqlx::SqlitePool
                        ) -> std::result::Result<std::vec::Vec<Self>, sqlx::Error> {
                            sqlx::query_as("SELECT * FROM \"Customer\"")
                                .fetch_all(pool)
                                .await
                        }
                        /// Updates a row in the database that corresponds to the value of the
                        /// primary key field
                        pub async fn update(&self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("UPDATE \"Customer\" SET  WHERE \"id\" = ?")
                                .bind(&self.id)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Inserts a record via `INSERT` by sending all columns of the model
                        pub async fn insert(&self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("INSERT INTO \"Customer\" (\"id\") VALUES (?)")
                                .bind(&self.id)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Inserts a record via `INSERT`, skipping the primary key field, and
                        /// after insertion sets the primary key value from the DBMS to the
                        /// model
                        pub async fn insert_generating_id(
                            &mut self,
                            pool: &sqlx::SqlitePool
                        ) -> std::result::Result<(), sqlx::Error> {
                            let result: sqlx::sqlite::SqliteQueryResult =
                                sqlx::query("INSERT INTO \"Customer\" () VALUES ()")
                                    .execute(pool)
                                    .await?;
                            self.id = result.last_insert_rowid().into();
                            Ok(())
                        }
                        /// Deletes a row in the database that corresponds to the value of
                        /// the primary key field, and the model will be consumed
                        pub async fn delete(self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("DELETE FROM \"Customer\" WHERE \"id\" = ?")
                                .bind(&self.id)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Creates a new table in the database
                        pub async fn create_table(pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("CREATE TABLE Customer (id INTEGER PRIMARY KEY)")
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Deletes the entire table from the database, does nothing if there
                        /// is no such table
                        pub async fn drop_table(pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("DROP TABLE IF EXISTS \"Customer\"")
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
fn test_model_impl_2() {
    let x = generate_structs_impl(Driver::Sqlite, SQL_2);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut exists = false;
    for item in &generated.items {
        if let syn::Item::Impl(item_impl) = item {
            if item_impl.trait_.is_none() {
                exists = true;

                let impl_ = syn::parse2(quote::quote! {
                    #[allow(non_upper_case_globals)]
                    impl Customer {
                        /// Information about the table column
                        pub const CustomerId: SqlColumn = unsafe { SqlColumn::new("\"customer_id\"", "\"Customer\"", true) };
                        #[doc = "A special column representing the entire table: `\"Customer\".*`"]
                        pub const ALL: SqlColumn = unsafe { SqlColumn::new("*", "\"Customer\"", false) };
                        /// Selects a row from the database where the primary key corresponds
                        /// to the specified value
                        ///
                        /// Internally, it uses prepared statements, so this is the most
                        /// preferred way to get a single row of the table using the
                        /// primary key
                        pub async fn get_by_customer_id(
                            pool: &sqlx::SqlitePool,
                            value: i64
                        ) -> std::result::Result<Self, sqlx::Error> {
                            sqlx::query_as("SELECT * FROM \"Customer\" WHERE \"customer_id\" = ? LIMIT 1")
                                .bind(&value)
                                .fetch_one(pool)
                                .await
                        }
                        /// Selects all table rows from the database
                        pub async fn select_all(
                            pool: &sqlx::SqlitePool
                        ) -> std::result::Result<std::vec::Vec<Self>, sqlx::Error> {
                            sqlx::query_as("SELECT * FROM \"Customer\"")
                                .fetch_all(pool)
                                .await
                        }
                        /// Updates a row in the database that corresponds to the value of the
                        /// primary key field
                        pub async fn update(&self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("UPDATE \"Customer\" SET  WHERE \"customer_id\" = ?")
                                .bind(&self.customer_id)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Inserts a record via `INSERT` by sending all columns of the model
                        pub async fn insert(&self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("INSERT INTO \"Customer\" (\"customer_id\") VALUES (?)")
                                .bind(&self.customer_id)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Inserts a record via `INSERT`, skipping the primary key field, and
                        /// after insertion sets the primary key value from the DBMS to the
                        /// model
                        pub async fn insert_generating_customer_id(
                            &mut self,
                            pool: &sqlx::SqlitePool
                        ) -> std::result::Result<(), sqlx::Error> {
                            let result: sqlx::sqlite::SqliteQueryResult =
                                sqlx::query("INSERT INTO \"Customer\" () VALUES ()")
                                    .execute(pool)
                                    .await?;
                            self.customer_id = result.last_insert_rowid().into();
                            Ok(())
                        }
                        /// Deletes a row in the database that corresponds to the value of
                        /// the primary key field, and the model will be consumed
                        pub async fn delete(self, pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("DELETE FROM \"Customer\" WHERE \"customer_id\" = ?")
                                .bind(&self.customer_id)
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Creates a new table in the database
                        pub async fn create_table(pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("CREATE TABLE Customer (customer_id INTEGER PRIMARY KEY)")
                                .execute(pool)
                                .await?;
                            Ok(())
                        }
                        /// Deletes the entire table from the database, does nothing if there
                        /// is no such table
                        pub async fn drop_table(pool: &sqlx::SqlitePool) -> std::result::Result<(), sqlx::Error> {
                            sqlx::query("DROP TABLE IF EXISTS \"Customer\"")
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
