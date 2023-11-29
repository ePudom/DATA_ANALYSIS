-- Supposed log period - 30 days
select datediff(day, cast('2016-04-11' as date), cast('2016-05-12' as date))

-- Verify total participants
select count(distinct id) as distinct_id_da from daily_activity; -- 33

select count(distinct id) as distinct_id_hrs from heartrate_seconds; -- 14

select count(distinct id) as distinct_id_s from sleep_day; -- 24

select count(distinct id) as distinct_id_w from weight_log; -- 8


-- Verify no of rows 
select count(*) as no_of_rows_dly_act from daily_activity; -- 940

select count(*) as no_of_rows_hrt from heartrate_seconds; -- 2483658

select count(*) as no_of_rows_weight from weight_log; -- 67

select count(*) as no_of_rows_sleep from sleep_day; -- 413

-- Verify no of columns
select count(*) as no_of_cols_dly_act 
from information_schema.columns 
where table_name = 'daily_activity'; -- 15

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

-- We can also verify that the users log of the sleep record is not consistent as 
-- some participants loged less than a day 
-- This further shows how unreliable this data is 
select distinct id, count(SleepDay)
from sleep_day 
group by id 







