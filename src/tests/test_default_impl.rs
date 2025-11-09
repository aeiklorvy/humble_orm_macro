use super::SQL;
use crate::{Driver, generate_structs_impl};

#[test]
fn test_default_impl() {
    let x = generate_structs_impl(Driver::Sqlite, SQL);
    let generated: syn::File = syn::parse2(x).unwrap();

    let mut struct_exists = true;
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

            if &default_impl == item {
                struct_exists = true;
            }
        }
    }
    assert_eq!(struct_exists, true);
}
