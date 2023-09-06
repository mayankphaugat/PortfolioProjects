/*
COVID 19 Data Exploration

Skills Used: Window Functions, Aggregate Functions, Converting/Casting Data Types, Common Table Expressions (CTEs), Joins, Temp Tables
*/



--Selecting the data to start with

Select location,date,total_cases,new_cases,total_deaths,population
From [COVID Project]..CovidDeaths
Where continent is not null
order by location,date;

--Total Cases vs Total Deaths
--Percentage shows likelihood of death

Select location, date, total_cases, total_deaths, (cast(total_deaths as numeric)/cast(total_cases as numeric))*100 as PercentageDeath
From [COVID Project]..CovidDeaths
Where Location='India'
and continent is not null
order by location,date;

--Total Cases vs Population
--Percentage shows population infected with COVID

Select location, date, population, total_cases, (cast(total_cases as numeric)/cast(population as numeric))*100 as PercentageInfected
From [COVID Project]..CovidDeaths
Where location='India'
and continent is not null
order by location,date;

--Highest Infection Rate wrt Population

Select location, population, MAX(total_cases) as MaxInfectionCount, MAX((cast(total_cases as numeric)/cast(population as numeric)))*100 as PercentageInfected
From [COVID Project]..CovidDeaths
--Where location='India'
Group by location, population
order by PercentageInfected desc;

--Highest Death Count wrt Population

Select location, MAX(cast(total_deaths as numeric)) as MaxDeathCount
From [COVID Project]..CovidDeaths
--Where location='India'
Where continent is not null
Group by location
order by MaxDeathCount desc;

--Highest Death Count wrt Population by Continent

Select continent, MAX(cast(total_deaths as numeric)) as MaxDeathCount
From [COVID Project]..CovidDeaths
--Where location='India'
Where continent is not null
Group by continent
order by MaxDeathCount desc;

--Global

Select SUM(new_cases) as total_cases,
	SUM(new_deaths) as total_deaths, 
	SUM(new_deaths)/SUM(nullif(new_cases,0))*100 as PercentageDeath
From [COVID Project]..CovidDeaths
--Where Location='India'
Where continent is not null
--Group by date
order by total_cases,total_deaths;

--Total Population vs Vaccinations

Select dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	SUM(cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.Location Order by dea.location,dea.date) as RollingCount
	--,(RollingCount/population)*100 as RollingPercentage
From [COVID Project]..CovidDeaths dea
Join [COVID Project]..CovidVaccinations vac
	On dea.location=vac.location
	and dea.date=vac.date
Where dea.continent is not null
order by dea.location,dea.date;

 --CTE to calculate RollingPercentage
 
 With PopvsVac (continent, location, date, population, new_vaccinations, RollingCount)
 as
 (
Select dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	SUM(cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.Location Order by dea.location,dea.date) as RollingCount
	--,(RollingCount/population)*100 as RollingPercentage
From [COVID Project]..CovidDeaths dea
Join [COVID Project]..CovidVaccinations vac
	On dea.location=vac.location
	and dea.date=vac.date
Where dea.continent is not null
--order by dea.location,dea.date
)
Select *, (RollingCount/population)*100 as RollingPercentage
From PopvsVac

--TEMP TABLE to calculate RollingPercentage

Drop Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
RollingCount numeric
)

Insert into #PercentPopulationVaccinated
Select dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	SUM(cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.Location Order by dea.location,dea.date) as RollingCount
	--(RollingCount/population)*100 as RollingPercentage
From [COVID Project]..CovidDeaths dea
Join [COVID Project]..CovidVaccinations vac
	On dea.location=vac.location
	and dea.date=vac.date
--Where dea.continent is not null
--order by dae.location,dea.date

Select *, (RollingCount/population)*100 as RollingPercentage
From #PercentPopulationVaccinated

--Views for later visualisations

Create View PercentPopulationVaccinated 
as
Select dea.continent,
	dea.location,
	dea.date,
	dea.population,
	vac.new_vaccinations,
	SUM(cast(vac.new_vaccinations as numeric)) OVER (Partition by dea.Location Order by dea.location,dea.date) as RollingCount
	--(RollingCount/population)*100 as RollingPercentage
From [COVID Project]..CovidDeaths dea
Join [COVID Project]..CovidVaccinations vac
	On dea.location=vac.location
	and dea.date=vac.date
Where dea.continent is not null
--order by 2,3


