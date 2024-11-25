CREATE TABLE IF NOT EXISTS covid(
    id	TEXT,
    date DATE,	
    confirmed DECIMAL,
    deaths	DECIMAL,
    recovered DECIMAL,
    tests DECIMAL,
    vaccines DECIMAL,
    people_vaccinated DECIMAL,
    people_fully_vaccinated	DECIMAL,
    hosp DECIMAL,
    icu DECIMAL,
    vent DECIMAL,
    school_closing DECIMAL,
    workplace_closing DECIMAL,
    cancel_events DECIMAL,
    gatherings_restrictions DECIMAL,
    transport_closing DECIMAL,
    stay_home_restrictions DECIMAL,
    internal_movement_restrictions DECIMAL,
    international_movement_restrictions DECIMAL,
    information_campaigns DECIMAL,
    testing_policy DECIMAL,
    contact_tracing DECIMAL,
    facial_coverings DECIMAL,
    vaccination_policy DECIMAL,
    elderly_people_protection DECIMAL,
    government_response_index DECIMAL,
    stringency_index DECIMAL,
    containment_health_index DECIMAL,
    economic_support_index DECIMAL,
    administrative_area_level DECIMAL,
    administrative_area_level_1 TEXT,
    administrative_area_level_2 TEXT,
    administrative_area_level_3 TEXT,
    latitude DECIMAL,
    longitude DECIMAL,
    population DECIMAL,
    iso_alpha_3 TEXT,
    iso_alpha_2 TEXT,
    iso_numeric DECIMAL,
    iso_currency TEXT,
    key_local DECIMAL,
    key_google_mobility TEXT,
    key_apple_mobility TEXT,
    key_jhu_csse TEXT,
    key_nuts TEXT,
    key_gadm TEXT
);

\COPY covid FROM './data/raw.csv' DELIMITER ',' CSV HEADER;