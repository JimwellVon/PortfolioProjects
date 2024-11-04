Select *
From PortfolioProject..CovidDeaths
WHERE continent is not null
Order by 3,4

Select *
From PortfolioProject..CovidVax
Order by 3,4

Select location, date, total_cases, new_cases, total_deaths, population
From PortfolioProject..CovidDeaths
Order by 1,2

--Looking at Total cases vs. Total deaths
--shows the likelihood of dying if you get covid in your country

Select location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths
WHERE Location like '%Canada%'
Order by 1,2

--total cases vs. population
Select location, date, population, total_cases, (total_cases/population)*100 as PopPercentage
From PortfolioProject..CovidDeaths
WHERE Location like '%Canada%'
Order by 1,2

--Countries with highest infection rate compared to population
Select location, population, MAX(total_cases) as HighestInfectionCount, Max((total_cases/population))*100 as PopPercentageInfected
From PortfolioProject..CovidDeaths
GROUP BY location, population
Order by PopPercentageInfected desc

--Countries with highest death rate compared to population
Select location, MAX(cast(total_deaths as int)) as TotalDeathCount
From PortfolioProject..CovidDeaths
Where continent is not null
Group by location
Order by TotalDeathCount desc

--BREAKING THIS DOWN BY CONTINENT

--Showing continents witht he highest death count

Select continent, MAX(cast(total_deaths as int)) as TotalDeathCount
From PortfolioProject..CovidDeaths
Where continent is not null
Group by continent
Order by TotalDeathCount desc

--Global numbers
--by date
Select Date, sum(new_cases) as totalcases, SUM(cast(new_deaths as int)) as totaldeaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths
where continent is not null
group by date
Order by 1,2

--aggregate global numbers
Select sum(new_cases) as totalcases, SUM(cast(new_deaths as int)) as totaldeaths, SUM(cast(new_deaths as int))/SUM(new_cases)*100 as DeathPercentage
From PortfolioProject..CovidDeaths
where continent is not null

--Join function
SELECT *
From PortfolioProject..CovidDeaths dea
JOIN PortfolioProject..CovidVax vax
	On dea.location = vax.location
	and dea.date = vax. date


--Looking at Total Population vs. vaccinations

Select dea.continent, dea.location, dea.date, dea.population, vax.new_vaccinations
From PortfolioProject..CovidDeaths dea
JOIN PortfolioProject..CovidVax vax
	On dea.location = vax.location
	and dea.date = vax. date
Where dea.continent is not null
order by 1,2,3



Select dea.continent, dea.location, dea.date, dea.population, vax.new_vaccinations,
sum(Convert(int, vax.new_vaccinations)) OVER (Partition by dea.location Order by dea.location, dea.Date) as Vaxcount
From PortfolioProject..CovidDeaths dea
JOIN PortfolioProject..CovidVax vax
	On dea.location = vax.location
	and dea.date = vax. date
Where dea.continent is not null
order by 2,3

--CTE

With PopVsVax (Continent, Location, Date, population, new_vaccinations, Vaxcount)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vax.new_vaccinations,
sum(Convert(int, vax.new_vaccinations)) OVER (Partition by dea.location Order by dea.location, dea.Date) as Vaxcount
From PortfolioProject..CovidDeaths dea
JOIN PortfolioProject..CovidVax vax
	On dea.location = vax.location
	and dea.date = vax. date
Where dea.continent is not null
)
SELECT *, (Vaxcount/population)*100  as VaxcountPercentage
FROM PopVsVax


-- TEMP Table

DROP TABLE if exists #PercentPopulationVaccinated
Create Table #PercentPopulationVaccinated
(
Continent nvarchar(255),
Location nvarchar(255),
Date datetime,
Population numeric,
new_vaccinations numeric,
Vaxcount numeric
)

Insert into #PercentPopulationVaccinated
Select dea.continent, dea.location, dea.date, dea.population, vax.new_vaccinations,
sum(Convert(int, vax.new_vaccinations)) OVER (Partition by dea.location Order by dea.location, dea.Date) as Vaxcount
From PortfolioProject..CovidDeaths dea
JOIN PortfolioProject..CovidVax vax
	On dea.location = vax.location
	and dea.date = vax. date

SELECT *, (Vaxcount/population)*100  as VaxcountPercentage
FROM #PercentPopulationVaccinated

--Creating View to store data for visualization

Create View PercentPopulationVaccinated as 
Select dea.continent, dea.location, dea.date, dea.population, vax.new_vaccinations,
sum(Convert(int, vax.new_vaccinations)) OVER (Partition by dea.location Order by dea.location, dea.Date) as Vaxcount
From PortfolioProject..CovidDeaths dea
JOIN PortfolioProject..CovidVax vax
	On dea.location = vax.location
	and dea.date = vax. date
where dea.continent is not null
