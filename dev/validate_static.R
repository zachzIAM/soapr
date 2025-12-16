library(data.table)
library(ggplot2)

# load package
devtools::load_all("./generated_packages/barrawana")


# create objects ------------------------------------------------------------------------------

tf <- barrawana::TypeFactory$new()
client <- barrawana::new_client()


# login ---------------------------------------------------------------------------------------


user = "iamquant"
password = "TheCatSatOnTheMat99"
client_id = "kzdr85j9hv"

client$Login(User = user, Client = client_id, Password = password)


# set analysis date ---------------------------------------------------------------------------
client$SetAnalysisDate("2025-11-30")
client$GetAnalysisDate()


# create invalid portfolio object -------------------------------------------------------------
tf$Portfolio() # Incorrect! Type = NA
tf$Portfolio(Type = "DUMMY") # success! throws expected error
tf$Portfolio(Type = "WEIGHT") # success


# create valid portfolio object ---------------------------------------------------------------

mid <- tf$MID(ID = "2046251", IDType = "SEDOL")
pos <- tf$Position(Value = 1, Currency = "USD", MID = list(mid))

# correct portfolio
p <- tf$Portfolio(
  Type = "WEIGHT",
  PortfolioValue = 1,
  PortfolioCurrency = "USD",
  Pos = list(pos)
)


# set adhoc portfolio -------------------------------------------------------------------------

# This should not throw an error
client$SetAdHocPortfolio(Portfolio = p)

# set benchmark
settings <- tf$CurrentSettings(Benchmark = "MSCIJPND", BenchmarkOwner = "SYSTEM")
client$SetCurrentSettings(CurrentSettings = settings)

settings <- tf$CurrentSettings(BaseCurrency = "EUR")
client$SetCurrentSettings(CurrentSettings = settings) # Error: SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents

client$GetCurrentSettings() |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE) # benchmark has not been changed

client$SetCurrentSettingsKeyVal(CurrentSettings = settings) # Error: SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents

# set model
client$GetCurrentSessionModel()
client$SetModel(tf$Model(Name = "MAC.XL", Owner = "SYSTEM"))
client$GetCurrentSessionModel() # I am able to change the model although it sets it to MAC.L instead of MAC.XL???
client$SetModel(tf$Model(Name = "GEMLTL", Owner = "SYSTEM"))
client$GetCurrentSessionModel() # NOT CHANGED! still MAC.L

# using existing portfolio --------------------------------------------------------------------

status <- client$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")

while (!status$StatusResponse$IsDone) {
  Sys.sleep(1)
  status <- client$CheckRequestStatus(status$StatusResponse$RequestId)
}

client$GetCurrentSettings(InclSessionOverride = TRUE) |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE) # benchmark is correct


client$GetCurrentSessionModel()
client$SetModel(tf$Model(Name = "MAC.XL", Owner = "SYSTEM"))a
client$GetCurrentSessionModel() # I am able to change the model although it sets it to MAC.L instead of MAC.XL???
client$SetModel(tf$Model(Name = "GEMLTL", Owner = "SYSTEM"))
client$GetCurrentSessionModel() # NOT CHANGED! still MAC.L

# change benchmark ----------------------------------------------------------------------------

settings <- tf$CurrentSettings(Benchmark = "MSCIJPND", BenchmarkOwner = "SYSTEM")
client$SetCurrentSettings(CurrentSettings = settings)

client$GetCurrentSettings(FALSE) |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE) # benchmark has not been changed


client$GetCurrentSettings(InclSessionOverride = TRUE) |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE) # benchmark has not been changed


client$SetCurrentSettingsKeyVal(CurrentSettings = settings) # Error: SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents


# set base currency ---------------------------------------------------------------------------

settings$BaseCurrency <- "EUR"
client$SetCurrentSettings(CurrentSettings = settings)
client$GetCurrentSettings() |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE) # Currency is still USD

settings <- tf$CurrentSettings(BaseCurrency = "EUR")
client$SetCurrentSettings(CurrentSettings = settings) # Error: SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents

# logout --------------------------------------------------------------------------------------

client$Logout()
