/*
Covid 19 Data Exploration 
Skills used: Joins, CTE's, Windows Functions, Aggregate Functions, Creating Views, Converting Data Types
*/

Select *
from Sukhada..covid_death;

--Total no of deaths vs Total no of cases
Select location, date, total_cases, total_deaths, (total_deaths/total_cases)* 100 as Percentagedeaths
from Sukhada..covid_death
--Where total_cases is not null AND total_deaths is not null
Order by location,date;

--Total no of cases vs Population
Select location, date, total_cases,Population, (total_cases/Population)* 100 as Percentagecases
from Sukhada..covid_death
--Where total_cases is not null
Order by location,date;

--highest number of cases by location
Select location, SUM(total_cases) as Totalcasesperlocation
from Sukhada..covid_death
Where continent is not null
Group by location
Order by Totalcasesperlocation desc;

--no of deaths in each year ueing CTE- common table expressions
with CasesperMonth (location, year, total_cases, total_deaths)
AS
(
Select location, DATENAME(YEAR, CAST(date as datetime)) as year, total_cases, total_deaths
from Sukhada..covid_death
)
Select location, year, SUM(total_cases) as totalcasesperyear, SUM(cast(total_deaths as float)) as totaldeathsperyear
from CasesperMonth
where location like '%states%'
group by location, year
order by year;

--covid_vaccination table
select top 10 *
from Sukhada..covid_vaccinations;

--Joining two tables to see fully vaccinated people vs population
select dea.location, dea.date, dea.population, vac.people_fully_vaccinated, vac.new_vaccinations, (vac.people_fully_vaccinated/dea.population)*100 as vaccination_rate
from Sukhada..covid_death dea
JOIN
Sukhada..covid_vaccinations vac
ON dea.location = vac.location AND
   dea.date = vac.date
where dea.location = 'India'
order by dea.date;

--creating VIEW table for previous query for later visualization
CREATE VIEW percentagepeoplevaccinated
AS
select dea.location, dea.date, dea.population, vac.people_fully_vaccinated, vac.new_vaccinations, (vac.people_fully_vaccinated/dea.population)*100 as vaccination_rate
from Sukhada..covid_death dea
JOIN
Sukhada..covid_vaccinations vac
ON dea.location = vac.location AND
   dea.date = vac.date
where dea.location = 'India';

select * from percentagepeoplevaccinated
order by vaccination_rate desc;


