select count(UniqueID)
from PortfolioProject..NashvilleHousing

select * 
from PortfolioProject..NashvilleHousing

-- POPULATE PROPERTY ADDRESS
select *
from PortfolioProject..NashvilleHousing
where PropertyAddress is null 
order by ParcelID
-- 
-- Look for a property that has the same ParcelID and its PropertyAddress is populated
select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress,
isnull(a.PropertyAddress, b.PropertyAddress)
from PortfolioProject..NashvilleHousing a
join PortfolioProject..NashvilleHousing b
    on a.ParcelID = b.ParcelID
    and a.UniqueID != b.UniqueID
where a.PropertyAddress is null 
-- 
-- Update table 
update a
set PropertyAddress = 
    isnull(a.PropertyAddress, b.PropertyAddress)
    from PortfolioProject..NashvilleHousing a
    join PortfolioProject..NashvilleHousing b
        on a.ParcelID = b.ParcelID
        and a.UniqueID != b.UniqueID
    where a.PropertyAddress is null 


-- BREAKOUT PROPERTY ADDRESS INTO ADDRESS & CITY
-- using substring() and charindex()
select PropertyAddress
from PortfolioProject..NashvilleHousing
order by ParcelID

select 
substring(PropertyAddress, 1, charindex(',',PropertyAddress) - 1) as Address,
substring(PropertyAddress, charindex(',',PropertyAddress) + 1, len(PropertyAddress)) as City
from PortfolioProject..NashvilleHousing
-- 
-- Add new columns for PropertyAddressSplit & PropertyCitySplit
alter table PortfolioProject..NashvilleHousing
add PropertyAddressSplit nvarchar(50)

alter table PortfolioProject..NashvilleHousing
add PropertyCitySplit nvarchar(50)

-- Update columns with values 
update PortfolioProject..NashvilleHousing
set PropertyAddressSplit =
    substring(PropertyAddress, 1, charindex(',',PropertyAddress) - 1)

update PortfolioProject..NashvilleHousing
set PropertyCitySplit =
    substring(PropertyAddress, charindex(',',PropertyAddress) + 1, len(PropertyAddress))

select PropertyAddressSplit, PropertyCitySplit
from PortfolioProject..NashvilleHousing

-- select max(len(substring(PropertyAddress, 1, charindex(',',PropertyAddress) - 1)))
-- substring(PropertyAddress, 1, charindex(',',PropertyAddress) - 1) as Address,
-- substring(PropertyAddress, charindex(',',PropertyAddress) + 1, len(PropertyAddress)) as City
-- from PortfolioProject..NashvilleHousing


-- BREAKOUT OWNERS ADDRESS INTO ADDRESS, CITY & STATE 
-- using parsename() and replace()
select OwnerAddress, 
replace(OwnerAddress, ',', '.'),
parsename(replace(OwnerAddress, ',', '.'), 3) as Address,
parsename(replace(OwnerAddress, ',', '.'), 2) as City,
parsename(replace(OwnerAddress, ',', '.'), 1) as State
from PortfolioProject..NashvilleHousing
-- 
-- Add new columns for OwnerAddressSplit, OwnerCitySplit & OwnerStateSplit
alter table PortfolioProject..NashvilleHousing
add OwnerAddressSplit nvarchar(50)

alter table PortfolioProject..NashvilleHousing
add OwnerCitySplit nvarchar(50)

alter table PortfolioProject..NashvilleHousing
add OwnerStateSplit nvarchar(50)
-- 
-- Update columns with values 
update PortfolioProject..NashvilleHousing
set OwnerAddressSplit =
    parsename(replace(OwnerAddress, ',', '.'), 3)

update PortfolioProject..NashvilleHousing
set OwnerCitySplit =
    parsename(replace(OwnerAddress, ',', '.'), 2)

update PortfolioProject..NashvilleHousing
set OwnerStateSplit =
    parsename(replace(OwnerAddress, ',', '.'), 1)

select OwnerAddressSplit, OwnerCitySplit, OwnerStateSplit
from PortfolioProject..NashvilleHousing

-- CHANGE Y & N TO YES & NO RESPECTIVELY IN 'SOLD AS VACANT' COLUMN 
-- View unique values in the column 
select distinct SoldAsVacant, count(SoldAsVacant)
from PortfolioProject..NashvilleHousing
group by SoldAsVacant
order by 2

select SoldAsVacant,
case SoldAsVacant
    when 'Y' then 'Yes'
    when 'N' then 'No'
    else SoldAsVacant
end as newSoldAsVacant
from PortfolioProject..NashvilleHousing
-- 
-- Update column with new values 
update PortfolioProject..NashvilleHousing
set SoldAsVacant = 
    case SoldAsVacant
        when 'Y' then 'Yes'
        when 'N' then 'No'
        else SoldAsVacant
    end


-- REMOVING DUPLICATES 
select * 
from PortfolioProject..NashvilleHousing

-- select distinct row_num, count(row_num)
-- select UniqueID, row_num
-- from (
    -- select *,
    -- row_number() over (
    --     partition by 
    --         ParcelID,
    --         PropertyAddress,
    --         SalePrice,
    --         SaleDate
    --     order by UniqueID
    -- ) as row_num
    -- from PortfolioProject..NashvilleHousing
    -- order by UniqueID
-- ) a
-- where row_num =2
-- -- group by row_num
-- order by UniqueID

-- using CTE
with RowNum as (
    select *,
    row_number() over (
        partition by 
            ParcelID,
            PropertyAddress,
            SalePrice,
            SaleDate
        order by UniqueID
    ) as row_num
    from PortfolioProject..NashvilleHousing
    -- order by UniqueID
)
select * 
-- Delete duplicate rows 
-- delete
from RowNum
where row_num > 1
-- order by UniqueID


-- DELETE UNUSED COLUMNS (DO NOT DO THIS FOR MAIN TABLES IN DB)
alter table PortfolioProject..NashvilleHousing
drop column OwnerAddress, PropertyAddress, TaxDistrict



