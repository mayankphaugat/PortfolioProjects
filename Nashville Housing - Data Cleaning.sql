/*
Data Cleaning in SQL
*/

--Change Date Format

Select SaleDate, SaleDate2
From [COVID Projects]..HousingNashville

Alter Table HousingNashville
Add SaleDate2 Date

Update HousingNashville
SET SaleDate2 = CONVERT(Date,SaleDate)

--Populating PropertyAdress

Select * 
From [COVID Projects]..HousingNashville
--Where PropertyAddress is null
order by ParcelID

Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress,b.PropertyAddress)
From [COVID Projects]..HousingNashville a
JOIN [COVID Projects]..HousingNashville b
	ON a.ParcelID=b.ParcelID
	and a.[UniqueID ]<>b.[UniqueID ]
Where a.PropertyAddress is null

Update a
SET PropertyAddress = ISNULL(a.PropertyAddress,b.PropertyAddress)
From [COVID Projects]..HousingNashville a
JOIN [COVID Projects]..HousingNashville b
	ON a.ParcelID=b.ParcelID
	and a.[UniqueID ]<>b.[UniqueID ]
Where a.PropertyAddress is null

--Breaking up PropertyAddress into Individual Columns (Address, City)

Select PropertyAddress
From [COVID Projects]..HousingNashville
--Where PropertyAddress is null
--order by ParcelID

Select 
	SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1) as Address,
	SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress)) as City
From [COVID Projects]..HousingNashville

Alter Table HousingNashville
Add PropertySplitAddress nvarchar(255)

Update HousingNashville
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1)

Alter Table HousingNashville
Add PropertySplitCity nvarchar(255)

Update HousingNashville
SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) +1, LEN(PropertyAddress))

--Breaking up OwnerAddress into Individual Columns (Address, City, State)

Select
PARSENAME(REPLACE(OwnerAddress, ',', '.'),3),
PARSENAME(REPLACE(OwnerAddress, ',', '.'),2),
PARSENAME(REPLACE(OwnerAddress, ',', '.'),1)
From [COVID Projects]..HousingNashville

Alter Table HousingNashville
Add OwnerSplitAddress nvarchar(255)

Update HousingNashville
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.'),3)

Alter Table HousingNashville
Add OwnerSplitCity nvarchar(255)

Update HousingNashville
SET OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.'),2)

Alter Table HousingNashville
Add OwnerSplitState nvarchar(255)

Update HousingNashville
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.'),1)

--Change Y and N to Yes and No in SoldAsVacant

Select DISTINCT(SoldAsVacant),COUNT(SoldAsVacant)
From [COVID Projects]..HousingNashville
Group by SoldAsVacant
order by COUNT(SoldAsVacant)

Select	SoldAsVacant,
CASE	When SoldAsVacant = 'Y' Then 'Yes'
		When SoldAsVacant = 'N' Then 'No'
		ELSE SoldAsVacant
		END
From [COVID Projects]..HousingNashville

Update HousingNashville
SET SoldAsVacant = CASE	When SoldAsVacant = 'Y' Then 'Yes'
						When SoldAsVacant = 'N' Then 'No'
						ELSE SoldAsVacant
						END

--Removing Duplicates

WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() OVER(
	PARTITION BY	ParcelID,
					PropertyAddress,
					SalePrice,
					SaleDate,
					LegalReference
					ORDER BY
						UniqueID
						) RowNumber
From [COVID Projects]..HousingNashville
--order by ParcelID
--Where RowNumber>1
)
DELETE
From RowNumCTE
Where RowNumber>1

--Delete Unused Columns

Select *
From [COVID Projects]..HousingNashville

ALTER TABLE [COVID Projects]..HousingNashville
DROP COLUMN SaleDate, OwnerAddress, TaxDistrict, PropertyAddress



