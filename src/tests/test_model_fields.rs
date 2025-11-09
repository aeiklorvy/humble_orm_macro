use super::SQL;
use crate::{Driver, generate_structs_impl};

#[test]
fn test_model_fields() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut struct_exists = false;
    for item in &generated.items {
        if let syn::Item::Struct(_) = item {
            struct_exists = true;
            let s: syn::Item = syn::parse2(quote::quote! {
                #[derive(Clone, Debug, sqlx::FromRow)]
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
