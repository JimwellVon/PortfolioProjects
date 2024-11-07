/*

Cleaning Data in SQL Queries

*/

Select *
From PortfolioProject.dbo.Nashvillehousing

--------------------------------------------------------------------------------------------------------------------------

-- Standardize Date Format

Select Saledate, CONVERT(Date, Saledate)
From PortfolioProject.dbo.Nashvillehousing

Update PortfolioProject.dbo.Nashvillehousing
SET Saledate = CONVERT(Date, Saledate)

ALTER TABLE PortfolioProject.dbo.Nashvillehousing
Add SaleDateConverted Date;

Update PortfolioProject.dbo.Nashvillehousing
SET SaledateConverted = CONVERT(Date, Saledate)

Select *
From PortfolioProject.dbo.Nashvillehousing


 --------------------------------------------------------------------------------------------------------------------------

-- Populate Property Address data

Select *
From PortfolioProject.dbo.Nashvillehousing
Where PropertyAddress is null

Select a.ParcelID, b.ParcelID, a.UniqueID, b.UniqueID, a.PropertyAddress, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
From PortfolioProject.dbo.Nashvillehousing a
JOIN PortfolioProject.dbo.Nashvillehousing b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID] <> b.[UniqueID]
Where a.PropertyAddress is null

Update a
SET PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From PortfolioProject.dbo.Nashvillehousing a
JOIN PortfolioProject.dbo.Nashvillehousing b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID] <> b.[UniqueID]
Where a.PropertyAddress is null


--------------------------------------------------------------------------------------------------------------------------

-- Breaking out Address into Individual Columns (Address, City, State)

Select *
From PortfolioProject.dbo.Nashvillehousing

SELECT
Substring(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1) as Address
,Substring(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress)) as City
From PortfolioProject.dbo.Nashvillehousing

ALTER TABLE PortfolioProject.dbo.Nashvillehousing
ALTER COLUMN PropertySplitAddress NVARCHAR(255);

Update PortfolioProject.dbo.Nashvillehousing
SET PropertySplitAddress= Substring(PropertyAddress, 1, CHARINDEX(',', PropertyAddress)-1)

ALTER TABLE PortfolioProject.dbo.Nashvillehousing
ADD PropertySplitCity NVARCHAR(255);

Update PortfolioProject.dbo.Nashvillehousing
SET PropertySplitCity = Substring(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress))

SELECT *
From PortfolioProject.dbo.Nashvillehousing



SELECT OwnerAddress
From PortfolioProject.dbo.Nashvillehousing

Select
PARSENAME(REPLACE(OwnerAddress, ',','.') , 3)
,PARSENAME(REPLACE(OwnerAddress, ',','.') , 2)
,PARSENAME(REPLACE(OwnerAddress, ',','.') , 1)
From PortfolioProject.dbo.Nashvillehousing
 


ALTER TABLE PortfolioProject.dbo.Nashvillehousing
ADD OwnerSplitAddress NVARCHAR(255);

Update PortfolioProject.dbo.Nashvillehousing
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',','.') , 3)

ALTER TABLE PortfolioProject.dbo.Nashvillehousing
ADD OwnerSplitCity NVARCHAR(255);

Update PortfolioProject.dbo.Nashvillehousing
SET OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',','.') , 2)

ALTER TABLE PortfolioProject.dbo.Nashvillehousing
ADD OwnerSplitState NVARCHAR(255);

Update PortfolioProject.dbo.Nashvillehousing
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',','.') , 1)

SELECT *
From PortfolioProject.dbo.Nashvillehousing



--------------------------------------------------------------------------------------------------------------------------


-- Change Y and N to Yes and No in "Sold as Vacant" field


SELECT DISTINCT (SoldasVacant), Count(SoldasVacant)
From PortfolioProject.dbo.Nashvillehousing
Group by SoldasVacant
Order by SoldAsVacant


SELECT SoldasVacant
,CASE When SoldasVacant = 'Y' THEN 'Yes'
	  When SoldasVacant = 'N' Then 'No'
	  Else SoldasVacant
	  END
From PortfolioProject.dbo.Nashvillehousing

Update PortfolioProject.dbo.Nashvillehousing
SET SoldasVacant = CASE When SoldasVacant = 'Y' THEN 'Yes'
	  When SoldasVacant = 'N' Then 'No'
	  Else SoldasVacant
	  END



-----------------------------------------------------------------------------------------------------------------------------------------------------------

-- Remove Duplicates

WITH RowNumCTE AS (
SELECT *,
	ROW_NUMBER () OVER (
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
					UNIQUEID
					) row_num
From PortfolioProject.dbo.Nashvillehousing
)
SELECT *
FROM RowNumCTE
WHERE row_num > 1
--ORDER by PropertyAddress





SELECT *
From PortfolioProject.dbo.Nashvillehousing



---------------------------------------------------------------------------------------------------------

-- Delete Unused Columns



SELECT *
From PortfolioProject.dbo.Nashvillehousing


ALTER TABLE PortfolioProject.dbo.Nashvillehousing
DROP COLUMN PropertyAddress, OwnerAddress, TaxDistrict


ALTER TABLE PortfolioProject.dbo.Nashvillehousing
DROP COLUMN SaleDate





