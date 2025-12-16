library(data.table)
library(ggplot2)

# load package
devtools::load_all(".")


# create objects ------------------------------------------------------------------------------
service <- soapr::Service$new('https://www.barraone.com/bdti/BDTInteractive?wsdl')
client <- service$client
tf <- client$factory

# login ---------------------------------------------------------------------------------------


user = "iamquant"
password = "TheCatSatOnTheMat99"
client_id = "kzdr85j9hv"

client$Login(User = user, Client = client_id, Password = password)


# create invalid portfolio object -------------------------------------------------------------

# INCORRECT: from the WSDL allowed portfolio types are HOLDINGS, WEIGHT and VALUE;
# Type = "DUMMY" should be throwing an error; instead we get an object
p <- tf$create("Portfolio")
p$Type <- "DUMMY" # THIS SHOULD NOT BE ALLOWED


# create valid portfolio object ---------------------------------------------------------------
mid <- tf$create("MID")
mid$ID <- "2046251"
mid$IDType <- "SEDOL"

pos <- tf$create("Position")
pos$Value <- 1
pos$Currency <- "USD"
pos$MID <- list(mid)


# correct portfolio
p <- tf$create("Portfolio")
p$Type <- "WEIGHT"
p$PortfolioValue <- 1
p$PortfolioCurrency <- "USD"
p$Pos <- list(pos)


# set date ------------------------------------------------------------------------------------
client$SetAnalysisDate(AnalysisDate = "2025-11-30")
client$GetAnalysisDate()  # correctly sets Analysis date

# set adhoc portfolio -------------------------------------------------------------------------

# THIS WORKS AS EXPECTED
client$SetAdHocPortfolio(Portfolio = p)
client$GetCurrentSettings(InclSessionOverride = "true") |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE)  # benchmark is cash as expected


# set model -----------------------------------------------------------------------------------

model <- tf$create("Model")
model$Name <- "MAC.XL" # "GEMLTL"
model$Owner <- "SYSTEM"
client$SetModel(Model = model)
client$GetCurrentSessionModel() # correctly sets the model

model <- tf$create("Model")
model$Name <- "GEMLTL" # "GEMLTL"
model$Owner <- "SYSTEM"
client$SetModel(Model = model)
client$GetCurrentSessionModel() # correctly sets the model

# set benchmark -------------------------------------------------------------------------------
settings <- tf$create("CurrentSettings")
settings$Benchmark <- "MSAWIF_AD" # "MSCIJPND"
settings$BenchmarkOwner <- "SYSTEM"

client$SetCurrentSettings(CurrentSettings = settings)
client$GetCurrentSettings(InclSessionOverride = "true") |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE)  # INCORRECT Benchmark has remained CASH

client$SetCurrentSettingsKeyVal(CurrentSettings = settings) # SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents



# set base currency ---------------------------------------------------------------------------

settings$BaseCurrency <- "EUR"
client$SetCurrentSettings(CurrentSettings = settings)

client$GetCurrentSettings() |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE)  # INCORRECT BaseCurrency has remained USD


settings <- tf$create("CurrentSettings")
settings$BaseCurrency <- "EUR"
client$SetCurrentSettings(CurrentSettings = settings)  # SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents

# use existing portfolio ----------------------------------------------------------------------

status <- client$SetCurrentPortfolio(PortfolioName = "FOGGEQUI", PortfolioOwner = "InvestecAd")
while (!status$StatusResponse$IsDone) {
  Sys.sleep(1)
  status <- client$CheckRequestStatus(RequestId = status$StatusResponse$RequestId)
}

client$GetCurrentSettings() |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE)

# change model
model <- tf$create("Model")
model$Name <- "MAC.XL" # "GEMLTL"
model$Owner <- "SYSTEM"
client$SetModel(Model = model)
client$GetCurrentSessionModel() # correctly sets the model

# set new benchmark
settings <- tf$create("CurrentSettings")
settings$Benchmark <- "MSCIJPND"
settings$BenchmarkOwner <- "SYSTEM"

client$SetCurrentSettings(CurrentSettings = settings)
client$GetCurrentSettings(InclSessionOverride = TRUE) |>
  purrr::map(~data.table::rbindlist(purrr::map(.x, data.table::as.data.table))) |>
  data.table::rbindlist(fill = TRUE)  # INCORRECT Benchmark has remained MSAWIF_AD

client$SetCurrentSettingsKeyVal(CurrentSettings = settings) # SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents

# set base currency
settings$BaseCurrency <- "EUR"
client$SetCurrentSettings(CurrentSettings = settings)
client$GetCurrentSettings() |>
  purrr::map(~rbindlist(purrr::map(.x, as.data.table))) |>
  rbindlist(fill = TRUE)  # INCORRECT BaseCurrency has remained USD

settings <- tf$create("CurrentSettings")
settings$BaseCurrency <- "EUR"
client$SetCurrentSettings(CurrentSettings = settings)  # SOAP Fault: [S:Server] Entity References are not allowed in SOAP documents

# using key value
eur <- tf$create("KeyValue")
eur$Key <- "BaseCurrency"
eur$Value <- "EUR"
client$SetCurrentSettingsKeyVal(CurrentSettings = eur)
client$SetCurrentSettingsKeyVal(CurrentSettings = list(CurrentSettings = eur))

x <- client$GetCurrentSettings()

# logout --------------------------------------------------------------------------------------

client$Logout()
