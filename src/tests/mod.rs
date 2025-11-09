mod test_default_impl;
mod test_model_fields;

pub(crate) const SQL: &str = r#"
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
