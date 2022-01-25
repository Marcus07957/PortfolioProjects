select *
From PortfolioProject..CovidDeaths$
where continent is not null
order by 3,4

--select *
--From PortfolioProject..CovidVaccinations
--order by 3,4

--Select Data that we are going to be using

Select Location, date, total_cases, new_cases, total_deaths, population
From PortfolioProject..CovidDeaths$
where continent is not null
order by 1,2


--Looking at Total Cases vs Total Deaths
--Shows likelihood of dying if you contract covid in the UK

Select Location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths$
where continent is not null
and location like '%kingdom%'
order by 1,2


--Looking at Total Cases vs Population
--Shows what percentage of population got Covid

Select Location, date, Population, total_cases, (total_cases/Population)*100 as PercentagePopulationInfected
From PortfolioProject..CovidDeaths$
where continent is not null
and location like '%kingdom%'
order by 1,2


-- Looking at Countries with highest infection rate compared to population

Select Location, Population, MAX(total_cases) as HighestInfectionCount, Max((total_cases/Population))*100 as PercentagePopulationInfected
From PortfolioProject..CovidDeaths$
where continent is not null
--where location like '%kingdom%'
Group by Location, Population
order by PercentagePopulationInfected desc


-- LET US BREAK THINGS DOWN BY CONTINENT



-- Showing Countries with the highest death count per population

Select continent, MAX(cast(Total_deaths as bigint)) as TotalDeathCount
From PortfolioProject..CovidDeaths$
--where location like '%kingdom%'
where continent is not null
Group by continent
order by TotalDeathCount desc



-- GLOBAL NUMBERS

Select SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths$
where continent is not null
--and location like '%kingdom%'
--Group By date
order by 1,2

Select date, SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths$
where continent is not null
--and location like '%kingdom%'
Group By date
order by 1,2


-- Looking at total population vs vaccinations
-- Uses CTE

with PopvsVac (Continent, Location, Date, Population, new_vaccinations, RollingPeopleVaccinated)
as
(

Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(Cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.Location Order by dea.location,
dea.Date) as RollingPeopleVaccinated
From PortfolioProject..CovidDeaths$ dea
Join PortfolioProject..CovidVaccinations$ vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3
)
Select *, (RollingPeopleVaccinated/Population)*100
From PopvsVac


-- Uses TEMP TABLE

DROP Table if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255), Location nvarchar(255), Date datetime, Population numeric, new_vaccinations numeric, RollingPeopleVaccinated numeric)

Insert into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(Cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.Location Order by dea.location,
dea.Date) as RollingPeopleVaccinated
From PortfolioProject..CovidDeaths$ dea
Join PortfolioProject..CovidVaccinations$ vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3

Select *, (RollingPeopleVaccinated/Population)*100
From #PercentPopulationVaccinated


-- Creating View to store data for later visualizations

Create View PercentofPopulationVaccinated as
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations, SUM(Cast(vac.new_vaccinations as bigint)) OVER (Partition by dea.Location Order by dea.location,
dea.Date) as RollingPeopleVaccinated
From PortfolioProject..CovidDeaths$ dea
Join PortfolioProject..CovidVaccinations$ vac
	On dea.location = vac.location
	and dea.date = vac.date
where dea.continent is not null
--order by 2,3

Create View GlobalNumbers as
Select SUM(new_cases) as total_cases, SUM(cast(new_deaths as int)) as total_deaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths$
where continent is not null
--and location like '%kingdom%'
--Group By date
--order by 1,2
