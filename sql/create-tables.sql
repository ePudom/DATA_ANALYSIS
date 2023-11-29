-- test table 
-- select distinct iso_code, location,
-- case
-- when location in ('Africa', 'Europe', 'North America', 'South America', 'Oceania', 'Asia')
-- then 'CONTINENT'
-- when iso_code like 'OWID_%C'
-- then 'INCOME'
-- when location = 'World'
-- then 'WORLD'
-- else 'EU' 
-- end as type
-- into world.[temp_category]
-- from world.[main-covid-data]
-- where continent is null;


-- alter table world.[temp_deaths_country]
-- add id int  not null    identity(1,1)    primary key  


-- select * from world.temp_category

-- select id, iso_code, [location], [type] 
-- into world.[location_category]
-- from world.temp_category

-- select * from world.[covid_vaccinations_country]
-- order by id




-- 1a) covid vaccinations country

-- select id, country_id,  date, total_tests, new_tests, total_tests_per_thousand, 
-- new_tests_per_thousand, new_tests_smoothed, new_tests_smoothed_per_thousand, 
-- tests_per_case, tests_units, total_vaccinations, people_vaccinated, people_fully_vaccinated,
-- total_boosters, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, 
-- people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, total_boosters_per_hundred,
-- new_vaccinations_smoothed_per_million, new_people_vaccinated_smoothed, 
-- new_people_vaccinated_smoothed_per_hundred, stringency_index, population_density, median_age,
-- aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, 
-- diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities, 
-- hospital_beds_per_thousand, life_expectancy, human_development_index, population, 
-- excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, 
-- excess_mortality_cumulative_per_million
-- into world.[covid_vaccinations_country]
-- from world.[temp_vac_country]
-- -- from world.[main-covid-data] , world.[countries] cn
-- -- where continent is not null 
-- -- and cn.country = [location]
;


-- 1b) covid deaths country
-- select id, country_id, date, population, total_cases, new_cases, new_cases_smoothed,
-- total_deaths, new_deaths, new_deaths_smoothed, total_cases_per_million, new_cases_per_million, 
-- new_cases_smoothed_per_million, total_deaths_per_million, new_deaths_per_million, 
-- new_deaths_smoothed_per_million, reproduction_rate, icu_patients, icu_patients_per_million, 
-- hosp_patients, hosp_patients_per_million, weekly_icu_admissions, weekly_icu_admissions_per_million, 
-- weekly_hosp_admissions, weekly_hosp_admissions_per_million
-- into world.[covid_deaths_country]
-- from world.[temp_deaths_country]
-- from world.[main-covid-data], world.[countries] cn
-- where continent is not null
-- and cn.country = [location]


-- 2a) covid vaccinations continent
-- select id, continent_id, date, total_tests, new_tests, total_tests_per_thousand, 
-- new_tests_per_thousand, new_tests_smoothed, new_tests_smoothed_per_thousand, 
-- tests_per_case, tests_units, total_vaccinations, people_vaccinated, people_fully_vaccinated,
-- total_boosters, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, 
-- people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, total_boosters_per_hundred,
-- new_vaccinations_smoothed_per_million, new_people_vaccinated_smoothed, 
-- new_people_vaccinated_smoothed_per_hundred, stringency_index, population_density, median_age,
-- aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, 
-- diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities, 
-- hospital_beds_per_thousand, life_expectancy, human_development_index, population, 
-- excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, 
-- excess_mortality_cumulative_per_million
-- into world.covid_vaccinations_continent
-- from world.temp_vac_cont
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].location in ('Africa', 'Europe', 'North America', 'South America', 'Oceania', 'Asia')
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;

select * from world.[temp_vac_inc]

-- 2b) covid deaths continent
-- select id, continent_id, date, population, total_cases, new_cases, new_cases_smoothed,
-- total_deaths, new_deaths, new_deaths_smoothed, total_cases_per_million, new_cases_per_million, 
-- new_cases_smoothed_per_million, total_deaths_per_million, new_deaths_per_million, 
-- new_deaths_smoothed_per_million, reproduction_rate, icu_patients, icu_patients_per_million, 
-- hosp_patients, hosp_patients_per_million, weekly_icu_admissions, weekly_icu_admissions_per_million, 
-- weekly_hosp_admissions, weekly_hosp_admissions_per_million
-- into world.covid_deaths_continent
-- from world.temp_dea_cont
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].location in ('Africa', 'Europe', 'North America', 'South America', 'Oceania', 'Asia')
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;
 

-- 3a) covid vaccinations income
-- select id, continent_id, date, total_tests, new_tests, total_tests_per_thousand, 
-- new_tests_per_thousand, new_tests_smoothed, new_tests_smoothed_per_thousand, 
-- tests_per_case, tests_units, total_vaccinations, people_vaccinated, people_fully_vaccinated,
-- total_boosters, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, 
-- people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, total_boosters_per_hundred,
-- new_vaccinations_smoothed_per_million, new_people_vaccinated_smoothed, 
-- new_people_vaccinated_smoothed_per_hundred, stringency_index, population_density, median_age,
-- aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, 
-- diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities, 
-- hospital_beds_per_thousand, life_expectancy, human_development_index, population, 
-- excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, 
-- excess_mortality_cumulative_per_million
-- into world.covid_vaccinations_income
-- from world.temp_vac_inc
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].iso_code like 'OWID_%C'
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;

-- 3b) covid deaths income
-- select id, continent_id, date, population, total_cases, new_cases, new_cases_smoothed,
-- total_deaths, new_deaths, new_deaths_smoothed, total_cases_per_million, new_cases_per_million, 
-- new_cases_smoothed_per_million, total_deaths_per_million, new_deaths_per_million, 
-- new_deaths_smoothed_per_million, reproduction_rate, icu_patients, icu_patients_per_million, 
-- hosp_patients, hosp_patients_per_million, weekly_icu_admissions, weekly_icu_admissions_per_million, 
-- weekly_hosp_admissions, weekly_hosp_admissions_per_million
-- into world.covid_deaths_income
-- from world.temp_dea_inc
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].iso_code like 'OWID_%C'
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;


-- 4a) covid vaccinations eu
-- select id, continent_id, date, total_tests, new_tests, total_tests_per_thousand, 
-- new_tests_per_thousand, new_tests_smoothed, new_tests_smoothed_per_thousand, 
-- tests_per_case, tests_units, total_vaccinations, people_vaccinated, people_fully_vaccinated,
-- total_boosters, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, 
-- people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, total_boosters_per_hundred,
-- new_vaccinations_smoothed_per_million, new_people_vaccinated_smoothed, 
-- new_people_vaccinated_smoothed_per_hundred, stringency_index, population_density, median_age,
-- aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, 
-- diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities, 
-- hospital_beds_per_thousand, life_expectancy, human_development_index, population, 
-- excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, 
-- excess_mortality_cumulative_per_million
-- into world.covid_vaccinations_eu
-- from world.temp_vac_eu
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].location = 'European Union'
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;

-- covid deaths eu
-- select id, continent_id, date, population, total_cases, new_cases, new_cases_smoothed,
-- total_deaths, new_deaths, new_deaths_smoothed, total_cases_per_million, new_cases_per_million, 
-- new_cases_smoothed_per_million, total_deaths_per_million, new_deaths_per_million, 
-- new_deaths_smoothed_per_million, reproduction_rate, icu_patients, icu_patients_per_million, 
-- hosp_patients, hosp_patients_per_million, weekly_icu_admissions, weekly_icu_admissions_per_million, 
-- weekly_hosp_admissions, weekly_hosp_admissions_per_million
-- into world.covid_deaths_eu
-- from world.temp_dea_eu
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].location = 'European Union'
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;

alter table world.[temp_dea_wrd]
add id int  not null    identity(1,1)    primary key 

-- covid vaccinations world
-- select id, continent_id, date, total_tests, new_tests, total_tests_per_thousand, 
-- new_tests_per_thousand, new_tests_smoothed, new_tests_smoothed_per_thousand, 
-- tests_per_case, tests_units, total_vaccinations, people_vaccinated, people_fully_vaccinated,
-- total_boosters, new_vaccinations, new_vaccinations_smoothed, total_vaccinations_per_hundred, 
-- people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, total_boosters_per_hundred,
-- new_vaccinations_smoothed_per_million, new_people_vaccinated_smoothed, 
-- new_people_vaccinated_smoothed_per_hundred, stringency_index, population_density, median_age,
-- aged_65_older, aged_70_older, gdp_per_capita, extreme_poverty, cardiovasc_death_rate, 
-- diabetes_prevalence, female_smokers, male_smokers, handwashing_facilities, 
-- hospital_beds_per_thousand, life_expectancy, human_development_index, population, 
-- excess_mortality_cumulative_absolute, excess_mortality_cumulative, excess_mortality, 
-- excess_mortality_cumulative_per_million
-- into world.covid_vaccinations_world
-- from world.temp_vac_wrd
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].location = 'World'
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;

-- covid deaths world
-- select id, continent_id, date, population, total_cases, new_cases, new_cases_smoothed,
-- total_deaths, new_deaths, new_deaths_smoothed, total_cases_per_million, new_cases_per_million, 
-- new_cases_smoothed_per_million, total_deaths_per_million, new_deaths_per_million, 
-- new_deaths_smoothed_per_million, reproduction_rate, icu_patients, icu_patients_per_million, 
-- hosp_patients, hosp_patients_per_million, weekly_icu_admissions, weekly_icu_admissions_per_million, 
-- weekly_hosp_admissions, weekly_hosp_admissions_per_million
-- into world.covid_deaths_world
-- from world.temp_dea_wrd
-- from world.[main-covid-data], world.[category] c
-- where continent is null
-- and world.[main-covid-data].location = 'World'
-- and world.[main-covid-data].location = c.location
-- order by world.[main-covid-data].location, date;