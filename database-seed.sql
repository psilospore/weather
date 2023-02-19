CREATE TABLE cities (
  id serial PRIMARY KEY,
  name text NOT NULL,
  state text NOT NULL,
  latitude decimal NOT NULL,
  longitude decimal NOT NULL
);

-- Frontend passes city directly so this is fine as an index.
CREATE INDEX cities_name ON cities (name);

CREATE TABLE weather (
  id serial PRIMARY KEY,
  city_id integer NOT NULL,
  date date NOT NULL,
  temperature integer NOT NULL
);

-- We only query on the city id and weather date so we can use a compound index.
CREATE INDEX weather_date_city_id ON weather (date, city_id);


-- Seeding with a bunch of random cities from here:
-- https://www.latlong.net/category/cities-236-15.html
INSERT INTO 
    cities (name, state, latitude, longitude)
VALUES
    ('Peabody', 'MA',	42.536457, -70.985786),
    ('Northampton', 'MA',	42.328674	, -72.664658),
    ('Newton', 'MA',	42.341042	, -71.217133),
    ('Newburyport', 'MA',	42.810356	, -70.893875),
    ('New Bedford', 'MA',	41.638409	, -70.941208),
    ('Medford', 'MA',	42.419331	, -71.119720),
    ('Malden', 'MA',	42.429752	, -71.071022),
    ('Leominster', 'MA',	42.525482	, -71.764183),
    ('Lawrence', 'MA',	42.701283	, -71.175682),
    ('Holyoke', 'MA',	42.203217	, -72.625481),
    ('Greenfield', 'MA',	42.587334	, -72.603416),
    ('Framingham', 'MA',	42.280418	, -71.423233),
    ('Fitchburg', 'MA',	42.586716	, -71.814468),
    ('Everett', 'MA',	42.408623	, -71.056999),
    ('Chelsea', 'MA',	42.392925	, -71.037109),
    ('Amesbury', 'MA',	42.856842	, -70.963440),
    ('Takoma Park', 'MD',	38.981544	, -77.010674),
    ('Salisbury', 'MD',	38.363350	, -75.605919),
    ('Rockville', 'MD',	39.086437	, -77.161263),
    ('Hagerstown', 'MD',	39.644207	, -77.731430),
    ('Greenbelt', 'MD',	38.998318	, -76.896332),
    ('Cumberland', 'MD',	39.649109	, -78.769714),
    ('Cambridge', 'MD',	38.563461	, -76.085251),
    ('Aberdeen', 'MD',	39.514877	, -76.174110),
    ('Presque Isle', 'ME',	46.680672	, -68.023521),
    ('Portland', 'ME',	43.680031	, -70.310425),
    ('Lewiston', 'ME',	44.101902	, -70.217110),
    ('Gardiner', 'ME',	44.230553	, -69.779633),
    ('Eastport', 'ME',	44.906910	, -66.996201),
    ('Calais', 'ME',	45.188042	, -67.282753),
    ('Biddeford', 'ME',	43.489849	, -70.469711),
    ('Belfast', 'ME',	44.424770	, -69.010620),
    ('Bath', 'ME',	43.917503	, -69.829712),
    ('Augusta', 'ME',	44.331493	, -69.788994),
    ('Auburn', 'ME',	44.090008	, -70.271439),
    ('Thibodaux', 'LA',	29.795111	, -90.828140),
    ('St Martinville', 'LA',	30.124033	, -91.833435),
    ('Morgan City', 'LA',	29.706043	, -91.206917),
    ('Monroe', 'LA',	32.509930	, -92.121742),
    ('Gretna', 'LA',	29.916653	, -90.057854),
    ('Bastrop', 'LA',	32.778889	, -91.919243),
    ('Alexandria', 'LA',	31.284788	, -92.471176),
    ('Richmond', 'KY',	37.746880	, -84.301460),
    ('Paris', 'KY',	38.206348	, -84.270172),
    ('Owensboro', 'KY',	37.760586	, -87.127686),
    ('Newport', 'KY',	39.088970	, -84.500786),
    ('Middlesboro', 'KY',	36.616894	, -83.739494),
    ('Mayfield', 'KY',	36.739876	, -88.646523),
    ('Hazard', 'KY',	37.250626	, -83.195503),
    ('Salina', 'KS',	38.826633	, -97.616257),
    ('Webster City', 'IA',	42.464397	, -93.829056),
    ('Vincennes', 'IN',	38.678299	, -87.522491);


