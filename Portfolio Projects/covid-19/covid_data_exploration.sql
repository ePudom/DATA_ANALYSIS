select * 
from PortfolioProject..CovidDeaths
where continent is not NULL
-- and icu_patients is not null
-- and hosp_patients is not null 
order by 3,4

-- COUNTRY --
-- Total Cases vs Total deaths 
-- Shows likelyhood of contacting covid 
select location, date, total_cases, total_deaths, 
(cast(total_deaths as float) / cast(total_cases as float)) * 100 as death_rate, population
from PortfolioProject..CovidDeaths
where location like '%Nigeria%'
order by 1,2

-- Total Cases vs Population 
-- Shows percentage of the population that got covid
select location, date, total_cases, total_deaths, 
(cast(total_cases as float) / cast(population as float)) * 100 as covid_rate, population
from PortfolioProject..CovidDeaths
where location like '%china%'
order by 1,2

-- Countries with highest infection rate compared to population 
select location, population, max(total_cases) as highest_infection_count, 
max(cast(total_cases as float) / cast(population as float)) * 100 as population_infected_percent 
from PortfolioProject..CovidDeaths
-- where location like '%Nigeria%'
group by location, population 
order by population_infected_percent desc 

-- Countries with highest death count per population 
select location, population, max(total_deaths) as total_death_count, 
max(cast(total_deaths as float) / cast(population as float)) * 100 as population_death_percent 
from PortfolioProject..CovidDeaths
where continent is not null
group by location, population 
order by total_death_count desc 

-- CONTINENTAL NUMBERS --
-- Continents with highest death count per population 
select location, population, max(total_deaths) as total_death_count, 
max(cast(total_deaths as float) / cast(population as float)) * 100 as population_death_percent 
from PortfolioProject..CovidDeaths
where continent is null
and location != 'World' and location not like '%income' and location not like '%union'
group by location, population 
order by total_death_count desc 

-- GLOBAL NUMBERS --
-- Total number of cases & deaths per day with percent per population
select date, sum(new_cases) as total_cases_, sum(new_deaths) as total_deaths_, sum(population) as total_population,
case 
when sum(new_cases) = 0 then 0
else (sum(cast(new_deaths as float)) / sum(cast(new_cases as float))) * 100 
end as world_death_percent_per_case,
(cast(sum(new_cases) as float) / cast(sum(population) as float)) * 100 as world_case_percent_per_population,
(cast(sum(new_deaths) as float) / cast(sum(population) as float)) * 100 as world_death_percent_per_population
from PortfolioProject..CovidDeaths 
where continent is not null 
group by date 
order by 1,2

-- Total number of cases & deaths with percent per population
select sum(new_cases) as total_cases_, sum(new_deaths) as total_deaths_, sum(population) as total_population,
case 
when sum(new_cases) = 0 then 0
else (sum(cast(new_deaths as float)) / sum(cast(new_cases as float))) * 100 
end as world_death_percent_per_case,
(cast(sum(new_cases) as float) / cast(sum(population) as float)) * 100 as world_case_percent_per_population,
(cast(sum(new_deaths) as float) / cast(sum(population) as float)) * 100 as world_death_percent_per_population
from PortfolioProject..CovidDeaths 
where continent is not null 
order by 1,2

-- Total population vs vaccination
-- - using CTE
with total_population_vs_vaccination (continent, location, date, population, new_vaccinations, rolling_total_vaccinations)
as (select d.continent, d.location, d.date, d.population, v.new_vaccinations,
    sum(v.new_vaccinations) 
        over (
            partition by d.location
            order by d.location, d.date
        )
    as rolling_total_vaccinations
    from PortfolioProject..CovidDeaths d
    join PortfolioProject..CovidVaccinations v
        on d.location = v.location 
        and d.date = v.date
    where d.continent is not null 
)

select *, (cast(rolling_total_vaccinations as float)/population) * 100 as vaccinantion_rate 
from total_population_vs_vaccination
order by 2,3

-- - using temp table
-- drop table if exists #percentPopulationVaccinated
create table #percentPopulationVaccinated (
    continent nvarchar(255),
    location nvarchar(255),
    date datetime,
    population numeric,
    new_vacinnations numeric,
    rolling_total_vaccinations numeric
)

insert into #percentPopulationVaccinated
select d.continent, d.location, d.date, d.population, v.new_vaccinations,
sum(v.new_vaccinations) 
    over (
        partition by d.location
        order by d.location, d.date
    )
as rolling_total_vaccinations
from PortfolioProject..CovidDeaths d
join PortfolioProject..CovidVaccinations v
    on d.location = v.location 
    and d.date = v.date
where d.continent is not null 

select *, (rolling_total_vaccinations / population) * 100 as vaccinantion_rate 
from #percentPopulationVaccinated
order by 2,3

-- Creating views for visualizations
-- use PortfolioProject
-- go 
-- create view total_population_vaccinated_percent as 
-- select d.continent, d.location, d.date, d.population, v.new_vaccinations,
-- sum(v.new_vaccinations) 
--     over (
--         partition by d.location
--         order by d.location, d.date
--     )
-- as rolling_total_vaccinations
-- from PortfolioProject..CovidDeaths d
-- join PortfolioProject..CovidVaccinations v
--     on d.location = v.location 
--     and d.date = v.date
-- where d.continent is not null 


