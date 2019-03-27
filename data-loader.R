loginfo("Loading data-loader...")

col_types = cols(
    MSSubClass = col_factor(),
    MSZoning = col_factor(),
    LotFrontage = col_double(),
    LotArea = col_double(),
    Street = col_factor(),
    Alley = col_factor(),
    LotShape = col_factor(),
    LandContour = col_factor(),
    Utilities = col_factor(),
    LotConfig = col_factor(),
    LandSlope = col_factor(),
    Neighborhood = col_factor(),
    Condition1 = col_factor(),
    Condition2 = col_factor(),
    BldgType = col_factor(),
    HouseStyle = col_factor(),
    OverallQual = col_integer(),
    OverallCond = col_integer(),
    YearBuilt = col_integer(),
    YearRemodAdd = col_integer(),
    RoofStyle = col_factor(),
    RoofMatl = col_factor(),
    Exterior1st = col_factor(),
    Exterior2nd = col_factor(),
    MasVnrType = col_factor(),
    MasVnrArea = col_double(),
    ExterQual = col_factor(),
    ExterCond = col_factor(),
    Foundation = col_factor(),
    BsmtQual = col_factor(),
    BsmtCond = col_factor(),
    BsmtExposure = col_factor(),
    BsmtFinType1 = col_factor(),
    BsmtFinSF1 = col_double(),
    BsmtFinType2 = col_factor(),
    BsmtFinSF2 = col_double(),
    BsmtUnfSF = col_double(),
    TotalBsmtSF = col_double(),
    Heating = col_factor(),
    HeatingQC = col_factor(),
    CentralAir = col_factor(),
    Electrical = col_factor(),
    `1stFlrSF` = col_double(),
    `2ndFlrSF` = col_double(),
    LowQualFinSF = col_double(),
    GrLivArea = col_double(),
    BsmtFullBath = col_integer(),
    BsmtHalfBath = col_integer(),
    FullBath = col_integer(),
    HalfBath = col_integer(),
    KitchenQual = col_factor(),
    TotRmsAbvGrd = col_integer(),
    Functional = col_factor(),
    Fireplaces = col_integer(),
    FireplaceQu = col_factor(),
    GarageType = col_factor(),
    GarageYrBlt = col_integer(),
    GarageFinish = col_factor(),
    GarageCars = col_integer(),
    GarageArea = col_double(),
    GarageQual = col_factor(),
    GarageCond = col_factor(),
    PavedDrive = col_factor(),
    WoodDeckSF = col_double(),
    OpenPorchSF = col_double(),
    EnclosedPorch = col_double(),
    `3SsnPorch` = col_double(),
    ScreenPorch = col_double(),
    PoolArea = col_double(),
    PoolQC = col_factor(),
    Fence = col_factor(),
    MiscFeature = col_factor(),
    MiscVal = col_double(),
    MoSold = col_integer(),
    YrSold = col_integer(),
    SaleType = col_factor(),
    SaleCondition = col_factor(),
    Id = col_character()
)

load_data = function(filename) {
    paste("Loading ", filename) %>% loginfo()
    read_csv(filename, col_types=col_types, na=character()) %>%
        # replace NA as appropriate
        mutate(LotFrontage = if_else(is.na(LotFrontage), 0, LotFrontage)) %>%
        mutate(MasVnrArea = if_else(is.na(MasVnrArea), 0, MasVnrArea)) %>%
        mutate(GarageYrBlt = if_else(is.na(GarageYrBlt), YearBuilt, GarageYrBlt)) %>%

        # add some columns to try make things better
        mutate(AnyGarage = YearBuilt != GarageYrBlt) %>%
        mutate(GarageAfter = GarageYrBlt - YearBuilt) %>%
        mutate(AnyRemod = YearBuilt != YearRemodAdd) %>%
        mutate(RemodAfter = YearRemodAdd - YearBuilt) %>%
        mutate(SoldAfter = YrSold - YearBuilt) %>%

        # drop the id
        select(-Id)
}
