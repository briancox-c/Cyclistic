/* Add and set non-geographic columns
-- =============================================================================
Day Type
- Weekend: Saturday and Sunday
- Shoulder Weekday: Monday and Friday
- Middle Weekday: Tuesday, Wednesday, and Thursday

Trip Type
- Round Trip: start and end coordinates are the same
- One Way: start and end coordinates are different

Duration
- Number of minutes between ride end time and start time */

ALTER TABLE cyclistic.smp -- Add columns
    ADD COLUMN IF NOT EXISTS day_type STRING,
    ADD COLUMN IF NOT EXISTS trip_type STRING,
    ADD COLUMN IF NOT EXISTS duration_mins FLOAT64;


UPDATE cyclistic.smp -- Set day type values
    SET day_type = CASE
        WHEN EXTRACT(DAYOFWEEK FROM time_start) IN (3, 4, 5)
                THEN 'Middle Weekday'
        WHEN EXTRACT(DAYOFWEEK FROM time_start) IN (2, 6)
                THEN 'Shoulder Weekday' 
        WHEN EXTRACT(DAYOFWEEK FROM time_start) IN (1, 7) THEN 'Weekend'
        END
    WHERE TRUE;


UPDATE cyclistic.smp -- Set trip type values
    SET trip_type = CASE
        WHEN CONCAT(start_lat, start_lng) = CONCAT(end_lat, end_lng)
                THEN 'Round Trip'
        ELSE 'One Way'
        END
    WHERE TRUE;


UPDATE cyclistic.smp -- Set duration values
    SET duration_mins = CAST(TIMESTAMP_DIFF(time_end, time_start, SECOND)
                        AS FLOAT64) / 60
    WHERE TRUE;


-- Validate the values in the added columns
-- =============================================================================

SELECT -- Check day_type values
        COUNTIF(day_type = 'Middle Weekday') AS middles,
        COUNTIF(day_type = 'Shoulder Weekday') AS shoulders,
        COUNTIF(day_type = 'Weekend') AS weekends,
        COUNTIF(day_type IS NULL) AS day_type_nulls,
        COUNTIF(
            CASE
                WHEN day_type IS NOT NULL
                THEN day_type NOT IN
                        ('Middle Weekday', 'Shoulder Weekday', 'Weekend')
            END) AS day_type_other
    FROM cyclistic.smp;


SELECT -- Check trip_type values
        COUNTIF(trip_type = 'Round Trip') AS round_trips,
        COUNTIF(trip_type = 'One Way') AS one_ways,
        COUNTIF(trip_type IS NULL) AS trip_type_nulls,
        COUNTIF(
            CASE
                WHEN trip_type IS NOT NULL
                THEN trip_type NOT IN('Round Trip', 'One Way')
            END)
            AS trip_type_other
    FROM cyclistic.smp;


SELECT -- Check duration values
        COUNTIF(duration_mins IS NULL) AS null_durations,
        COUNTIF(duration_mins <0) AS negative_durations,
        COUNTIF(duration_mins = 0) AS zero_durations,
        COUNTIF(duration_mins >= 1440) AS dayplus_durations,
        ROUND(MIN(duration_mins),3) AS minimum_duration,
        ROUND(AVG(duration_mins), 2) AS mean_duration,
        ROUND(MAX(duration_mins), 2) AS maximum_duration
    FROM cyclistic.smp;


/* Create a separate table containing
   geographically calculated variables for one-way trips.
   I exported this table for use in RStudio and Tableau
-- =============================================================================
Distance
- Miles between trip start and end points

Direction
- Cardinal direction from start point to end point, in integer degrees */

DROP TABLE IF EXISTS cyclistic.geo;
CREATE TABLE cyclistic.geo AS
    SELECT
        ride_id,
        (ST_DISTANCE(ST_GEOGPOINT(end_lng, end_lat),
                ST_GEOGPOINT(start_lng, start_lat)))
                * 0.0006213712 ## convert meters to miles
                AS distance_miles,
        CAST(ST_AZIMUTH(ST_GEOGPOINT(start_lng, start_lat),
                ST_GEOGPOINT(end_lng, end_lat))
                * 57.29578 AS INT64) ## convert radians to degrees
                AS direction
    FROM cyclistic.smp
    WHERE trip_type = 'One Way';


-- Validate geographically calculated variables
-- =============================================================================

SELECT -- check distance counts
        COUNTIF(distance_miles IS NULL) AS null_distance,
        COUNTIF(distance_miles = 0) AS zero_distance,
        COUNTIF(distance_miles < 0) AS negative_distance,
        ROUND(CAST(MIN(distance_miles) AS NUMERIC), 6) AS min_distance,
        ROUND(AVG(distance_miles), 2) AS mean_distance,
        ROUND(MAX(distance_miles), 2) AS max_distance        
    FROM cyclistic.geo;


SELECT -- check distance percentiles
        ROUND(PERCENTILE_DISC(distance_miles, 0.05) OVER(), 2)
                AS fifth_percentile,
        ROUND(PERCENTILE_DISC(distance_miles, 0.25) OVER(), 2)
                AS first_quartile,
        ROUND(PERCENTILE_DISC(distance_miles, 0.5) OVER(), 2)
                AS median,
        ROUND(PERCENTILE_DISC(distance_miles, 0.75) OVER(), 2)
                AS third_quartile,
        ROUND(PERCENTILE_DISC(distance_miles, 0.95) OVER(), 2)
                AS ninety_fifth_percentile,
        ROUND(PERCENTILE_DISC(distance_miles, 1) OVER(), 2) AS maximum
    FROM cyclistic.geo
    LIMIT 1;


SELECT -- check direction counts 
        COUNT(*) AS records,
        COUNTIF(direction BETWEEN 0 AND 360) AS valid_directions,
        COUNTIF(direction IS NULL) AS null_directions,     
        COUNTIF(direction NOT BETWEEN 0 AND 360) AS invalid_directions,
        COUNTIF(direction >= 0 AND direction < 90) AS NE_dir,
        COUNTIF(direction >= 90 AND direction < 180) AS SE_dir,
        COUNTIF(direction >= 180 AND direction < 270) AS SW_dir,
        COUNTIF(direction >= 270 AND direction <= 360) AS NW_dir
    FROM cyclistic.geo;
    

/* Create tables of the most heavily-used stations for efficient use in Tableau
-- =============================================================================

Create table of busy stations, defined as those averaging
at least 1 ride per day on either middle weekdays or weekends
for each user type */

DROP TABLE IF EXISTS cyclistic.busy_list;
CREATE TABLE cyclistic.busy_list AS (  
        SELECT
                member_casual, day_type, 
                start_station_id, start_station_name,
                COUNT(ride_id) / 156 AS trips_per_day
            FROM cyclistic.smp
            WHERE day_type = 'Middle Weekday'
            GROUP BY 1, 2, 3, 4
            HAVING COUNT(ride_id) / 156 >= 1
    UNION ALL
        SELECT
                member_casual, day_type, 
                start_station_id, start_station_name,
                COUNT(ride_id) / 104 AS trips_per_day
            FROM cyclistic.smp
            WHERE day_type = 'Weekend'
            GROUP BY 1, 2, 3, 4
            HAVING COUNT(ride_id) / 104 >= 1
);


/* Consolidate coordinates for busy stations:
   Each station can have many coordinate sets because the coordinates are
   specified to about 4-ft. precision, but the stations can cover thousands
   of square feet.
   This query chooses one (the most common) coordinate set for each station */

DROP TABLE IF EXISTS cyclistic.busy_coords;
CREATE TABLE cyclistic.busy_coords AS (
    WITH temptable AS (
        SELECT
                L.start_station_id,
                CONCAT(CAST(start_lat AS STRING), "|", CAST(start_lng AS STRING))
                        AS coords_string,
                COUNT(ride_id) as trip_count,
                RANK() OVER(PARTITION BY L.start_station_id
                        ORDER BY COUNT(ride_id) DESC) AS rank
            FROM cyclistic.smp S
                RIGHT JOIN cyclistic.busy_list L
                ON S.start_station_id = L.start_station_id
            GROUP BY 1, 2
    )
    SELECT
            start_station_id,
            CAST(SPLIT(coords_string, "|")[OFFSET(0)] AS FLOAT64) AS start_lat,
            CAST(SPLIT(coords_string, "|")[OFFSET(1)] AS FLOAT64) AS start_lng
        FROM temptable
        WHERE rank = 1
);


-- Join consolidated coordinates to busy station list

DROP TABLE IF EXISTS cyclistic.busy_stations;
CREATE TABLE cyclistic.busy_stations AS (
    SELECT
            member_casual, day_type,
            L.start_station_id, start_station_name,
            start_lat, start_lng,
            trips_per_day
        FROM
            cyclistic.busy_list L
            JOIN cyclistic.busy_coords C
            ON L.start_station_id = C.start_station_id
        ORDER BY 1, 2, 7 DESC
);


DROP TABLE IF EXISTS cyclistic.busy_list;
DROP TABLE IF EXISTS cyclistic.busy_coords;


-- Create table of top ten most heavily used stations by user type and day type
-- =============================================================================

DROP TABLE IF EXISTS cyclistic.member_weekday_10;
CREATE TABLE cyclistic.member_weekday_10 AS
    SELECT
        member_casual, day_type,
        start_station_id, start_station_name,
        COUNT(ride_id) AS trip_count
    FROM cyclistic.smp
    WHERE
        member_casual = 'member' AND
        day_type = 'Middle Weekday'
    GROUP BY 1, 2, 3, 4
    ORDER BY 5 DESC
    LIMIT 10;


DROP TABLE IF EXISTS cyclistic.member_weekend_10;
CREATE TABLE cyclistic.member_weekend_10 AS
    SELECT
        member_casual, day_type,
        start_station_id, start_station_name,
        COUNT(ride_id) AS trip_count
    FROM cyclistic.smp
    WHERE
        member_casual = 'member' AND
        day_type = 'Weekend'
    GROUP BY 1, 2, 3, 4
    ORDER BY 5 DESC
    LIMIT 10;


DROP TABLE IF EXISTS cyclistic.casual_weekday_10;
CREATE TABLE cyclistic.casual_weekday_10 AS
    SELECT
        member_casual, day_type,
        start_station_id, start_station_name,
        COUNT(ride_id) AS trip_count
    FROM cyclistic.smp
    WHERE
        member_casual = 'casual' AND
        day_type = 'Middle Weekday'
    GROUP BY 1, 2, 3, 4
    ORDER BY 5 DESC
    LIMIT 10;


DROP TABLE IF EXISTS cyclistic.casual_weekend_10;
CREATE TABLE cyclistic.casual_weekend_10 AS
    SELECT
        member_casual, day_type,
        start_station_id, start_station_name,
        COUNT(ride_id) AS trip_count
    FROM cyclistic.smp
    WHERE
        member_casual = 'casual' AND
        day_type = 'Weekend'
    GROUP BY 1, 2, 3, 4
    ORDER BY 5 DESC
    LIMIT 10;


DROP TABLE IF EXISTS cyclistic.top_list_10;
CREATE TABLE cyclistic.top_list_10 AS
        SELECT * FROM cyclistic.member_weekday_10
    UNION ALL
        SELECT * FROM cyclistic.member_weekend_10
    UNION ALL
        SELECT * FROM cyclistic.casual_weekday_10
    UNION ALL
        SELECT * FROM cyclistic.casual_weekend_10
    ORDER BY 1, 2, 5 DESC;


DROP TABLE IF EXISTS cyclistic.member_weekday_10;
DROP TABLE IF EXISTS cyclistic.member_weekend_10;
DROP TABLE IF EXISTS cyclistic.casual_weekday_10;
DROP TABLE IF EXISTS cyclistic.casual_weekend_10;

-- Consolidate coordinates for top stations

DROP TABLE IF EXISTS cyclistic.top_coords_10;
CREATE TABLE cyclistic.top_coords_10 AS (
    WITH temptable_10 AS (
        SELECT
            L.start_station_id,
            CONCAT(CAST(start_lat AS STRING), "|", CAST(start_lng AS STRING))
                AS coords_string,
            COUNT(ride_id) as trip_count,
            RANK() OVER(PARTITION BY L.start_station_id
                ORDER BY COUNT(ride_id) DESC) AS rank
        FROM cyclistic.smp S
            RIGHT JOIN cyclistic.top_list_10 L
            ON S.start_station_id = L.start_station_id
        GROUP BY 1, 2
    )


    SELECT
        start_station_id,
        CAST(SPLIT(coords_string, "|")[OFFSET(0)] AS FLOAT64) AS start_lat,
        CAST(SPLIT(coords_string, "|")[OFFSET(1)] AS FLOAT64) AS start_lng
    FROM temptable_10
    WHERE rank = 1
);


-- Join consolidated coordinates to top station list

DROP TABLE IF EXISTS cyclistic.top_10_stations;
CREATE TABLE cyclistic.top_10_stations AS (
    SELECT
        member_casual, day_type,
        L.start_station_id, start_station_name,
        start_lat, start_lng,
        trip_count
    FROM cyclistic.top_list_10 L
        JOIN cyclistic.top_coords_10 C
        ON L.start_station_id = C.start_station_id
    ORDER BY 1, 2, 7 DESC
);


DROP TABLE IF EXISTS cyclistic.top_list_10;
DROP TABLE IF EXISTS cyclistic.top_coords_10;


-- Identify the days of the year having the most and fewest rides
-- =============================================================================

SELECT -- 10 busiest days
        DATE(time_start) AS date,
        COUNT(ride_id) AS rides
    FROM cyclistic.entire
    GROUP BY 1
    ORDER BY 2 DESC
    LIMIT 10;


SELECT -- 10 least busy days
        DATE(time_start) AS date,
        COUNT(ride_id) AS rides
    FROM cyclistic.entire
    GROUP BY 1
    ORDER BY 2
    LIMIT 10;