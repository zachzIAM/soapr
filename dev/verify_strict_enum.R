# Verify Strict ENUMS
devtools::load_all("c:/workspace/soapr")
devtools::load_all("c:/workspace/barrawana")

tf <- barrawana::TypeFactory$new()

cat("Starting Checks for GetReportColumnName...\n", file = "verify_enum_result.txt")

cat("Testing GetReportColumnName constructor...\n")
tryCatch(
    {
        # ReportName is required enum
        obj <- tf$GetReportColumnName()
        cat("FAILURE: GetReportColumnName instantiated without arguments! ReportName should be required.\n", file = "verify_enum_result.txt", append = TRUE)
    },
    error = function(e) {
        cat(paste("SUCCESS Check 1: Caught error for missing strict enum:", e$message, "\n"), file = "verify_enum_result.txt", append = TRUE)
    }
)

cat("Testing with valid enum...\n")
tryCatch(
    {
        # Use a valid value. ReportName enum.
        # From scan: "AllocationSelection" etc.
        obj <- tf$GetReportColumnName(ReportName = "AllocationSelection")
        cat("SUCCESS Check 2: Instantiated with valid enum.\n", file = "verify_enum_result.txt", append = TRUE)
    },
    error = function(e) {
        cat(paste("FAILURE: Valid enum failed:", e$message, "\n"), file = "verify_enum_result.txt", append = TRUE)
    }
)
