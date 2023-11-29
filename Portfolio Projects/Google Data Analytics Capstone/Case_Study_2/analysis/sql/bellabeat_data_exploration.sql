-- Supposed log period - 30 days
select datediff(day, cast('2016-04-11' as date), cast('2016-05-12' as date))

-- Verify total participants
select count(distinct id) as distinct_id_da from daily_activity; -- 33

select count(distinct id) as distinct_id_dc from daily_calories; -- 33

select count(distinct id) as distinct_id_di from daily_intensities; -- 33

select count(distinct id) as distinct_id_ds from daily_steps; -- 33

select count(distinct id) as distinct_id_hc from hourly_calories; -- 33

select count(distinct id) as distinct_id_hi from hourly_intensities; -- 33

select count(distinct id) as distinct_id_hs from hourly_steps; -- 33

select count(distinct id) as distinct_id_hrs from heartrate_seconds; -- 14

select count(distinct id) as distinct_id_s from sleep_day; -- 24

select count(distinct id) as distinct_id_w from weight_log; -- 8


-- Verify no of rows 
select count(*) as no_of_rows_dly_act from daily_activity; -- 940

select count(*) as no_of_rows_dly_cal from daily_calories; -- 940

select count(*) as no_of_rows_dly_inten from daily_intensities; -- 940

select count(*) as no_of_rows_dly_steps from daily_steps; -- 940

select count(*) as no_of_rows_hr_cal from hourly_calories; -- 22099

select count(*) as no_of_rows_hr_inten from hourly_intensities; -- 22099

select count(*) as no_of_rows_hr_steps from hourly_steps; -- 22099

select count(*) as no_of_rows_hrt from heartrate_seconds; -- 2483658

select count(*) as no_of_rows_weight from weight_log; -- 67

select count(*) as no_of_rows_sleep from sleep_day; -- 413

-- Verify no of columns
select count(*) as no_of_cols_dly_act 
from information_schema.columns 
where table_name = 'daily_activity'; -- 15

select count(*) as no_of_cols_dly_cal 
from information_schema.columns 
where table_name = 'daily_calories'; -- 3

select count(*) as no_of_cols_dly_inten 
from information_schema.columns 
where table_name = 'daily_intensities'; -- 10

select count(*) as no_of_cols_dly_steps 
from information_schema.columns 
where table_name = 'daily_steps'; -- 3

select count(*) as no_of_cols_hr_cal 
from information_schema.columns 
where table_name = 'hourly_calories'; -- 3

select count(*) as no_of_cols_hr_inten 
from information_schema.columns 
where table_name = 'hourly_intensities'; -- 4

select count(*) as no_of_cols_hr_steps
from information_schema.columns 
where table_name = 'hourly_steps'; -- 3

select count(*) as no_of_cols_hrt 
from information_schema.columns 
where table_name = 'heartrate_seconds'; -- 3

select count(*) as no_of_cols_weight
from information_schema.columns 
where table_name = 'weight_log'; -- 8

select count(*) as no_of_cols_sleep
from information_schema.columns 
where table_name = 'sleep_day'; -- 5

-- Check for duplicates
select count(*) from (select distinct * from daily_activity) a; -- 940

select count(*) from (select distinct * from daily_calories) a; -- 940

select count(*) from (select distinct * from daily_intensities) a; -- 940

select count(*) from (select distinct * from daily_steps) a; -- 940

select count(*) from (select distinct * from hourly_calories ) a; -- 22099

select count(*) from (select distinct * from hourly_intensities) a; -- 22099

select count(*) from (select distinct * from hourly_steps) a; -- 22099

select count(*) from (select distinct * from heartrate_seconds ) a; -- 2483658

select count(*) from (select distinct * from weight_log) a; -- 67

select count(*) from (select distinct * from sleep_day) a; -- 410

-- There are duplicate rows in the sleep table 
-- So to remove the duplicated, we will be selecting the distinct rows from the sleep table 
with clean_sleep as (
    select distinct * from sleep_day
)


select top (5) * 
from daily_activity;


select distinct id 
from heartrate_seconds
order by id;

select distinct id 
from daily_activity
order by id;


select distinct Id,
case 
when h.Id in (select distinct id from daily_activity)
then 'y'
else 'n'
end as check_
from heartrate_seconds h







