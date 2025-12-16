#! /c/Programs/R/R-4.1.3/bin/Rscript.exe

.libPaths("C:/rlibraries/R/4.1.3")

library(rbdti)
library(data.table)

# login ---------------------------------------------------------------------------------------

user = "iamquant"
password = "TheCatSatOnTheMat99"
client_id = "kzdr85j9hv"

rbdti::login(user = user, client_id = client_id, password = password)


# set current portfolio -----------------------------------------------------------------------

status <- rbdti::set_current_portfolio(portfolio_name = "FOGGEQUI", portfolio_owner = "InvestecAd")

Sys.sleep(5)


# set analysis date ---------------------------------------------------------------------------
rbdti::set_analysis_date(as.Date("2025-11-30"))

# get settings --------------------------------------------------------------------------------
cs <- rbdti::get_current_settings()
message("Orginal Settings")
message("================\n\n")
print(rbindlist(purrr::map(cs[c(1, 9)], ~{data.table(name = .x$Key, value=.x$Value)})))


# test changing settings ----------------------------------------------------------------------

rbdti::set_current_settings(benchmark = "MSCIJPND", benchmark_owner = "SYSTEM")

ns <- rbdti::get_current_settings()
# testthat::expect_error(rbdti::set_current_settings(base_currency = "JPY"))
# testthat::expect_true(ns[[1]]$Value, "MSCIJPND")
# testthat::expect_equal(cs[[9]]$Value, ns[[9]]$Value)

rbdti::set_current_settings(benchmark = "MSCIJPND", benchmark_owner = "SYSTEM", base_currency = "JPY")
ns <- rbdti::get_current_settings()
# testthat::expect_equal(ns[[1]]$Value, "MSCIJPND")
# testthat::expect_equal(ns[[9]]$Value, "JPY")

message("New Settings")
message("============\n\n")
print(rbindlist(purrr::map(ns[c(1, 9)], ~{data.table(name = .x$Key, value=.x$Value)})))

rbdti::logout()
