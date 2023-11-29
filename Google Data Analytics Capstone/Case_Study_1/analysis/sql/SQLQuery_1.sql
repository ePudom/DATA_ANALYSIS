--Create table data type
create type rideDataType as table 
(
    ride_id [nvarchar](50) not null,
    rideable_type [nvarchar](50) not null,
    started_at [datetime2](7) not null,
    ended_at [datetime2](7) not null,
    start_station_name [nvarchar](100) null,
    start_station_id [nvarchar](50) null,
    end_station_name [nvarchar](100) null,
    end_station_id [nvarchar](50) null,
    start_lat [float] null,
    start_lng [float] null,
    end_lat [float] null,
    end_lng [float] null,
    member_casual [nvarchar](50) not null
)
go 
create function getTable(@rideTable dbo.rideDataType readonly)
returns table 
as 
return (
    select ride_id, rideable_type, started_at, ended_at, datepart(weekday, started_at) as start_day_of_week,
    cast((cast(ended_at as datetime)) - (cast(started_at as datetime)) as time(0)) as trip_duration, member_casual
    from @rideTable
)
go

-- Create function to get the day of the week
create function getWeekDay(@date datetime2)
returns varchar(10)
as 
begin 
    declare @day int
    declare @dayOfWeek varchar(10)
    select 
        @day = datepart(weekday, @date)
    select @dayOfWeek =
        case @day
            when 1 then 'Sunday'
            when 2 then 'Monday'
            when 3 then 'Tuesday'
            when 4 then 'Wednesday'
            when 5 then 'Thursday'
            when 6 then 'Friday'
            when 7 then 'Saturday'
            else NULL
        end 
    return @dayOfWeek
end 
go
-- Create function to get the duration
create function getDuration(@startDate datetime2, @endDate datetime2)
returns time(0)
as 
begin 
    declare @duration time(0)
    select 
        @duration = cast((cast(@endDate as datetime)) - (cast(@startDate as datetime)) as time(0))
    return @duration
end
go

select * from 
(
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_01
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_02
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_03
) a
where started_at is not null
and ended_at is not null;

select * from 
( 
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_04
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_05
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_06
) a
where started_at is not null
and ended_at is not null;

select * from 
( 
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_07
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_08
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_09
) a
where started_at is not null
and ended_at is not null;

select * from 
( 
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_10
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_11
    union
    select ride_id, rideable_type, started_at, ended_at, 
    dbo.getWeekDay(started_at) as start_day_of_week, dbo.getWeekDay(ended_at) as end_day_of_week,
    dbo.getDuration(started_at, ended_at) as trip_duration, member_casual
    from trips_2022_12
) a
where started_at is not null
and ended_at is not null;


