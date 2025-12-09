# Examples for Static Client Generator
# This script demonstrates how to generate and use a static client.

library(soapr)

# 1. Generate a Client
# ====================
# We'll use the Calculator service as an example.
wsdl_url <- "http://www.dneonline.com/calculator.asmx?WSDL"
output_dir <- tempdir()
package_name <- "CalculatorClient"

# Generate the package
generate_package(wsdl_url, output_dir, package_name)

# Install the package (optional, for this example we'll load it via pkgload)
pkg_path <- file.path(output_dir, package_name)
pkgload::load_all(pkg_path)

# 2. Initialize Client
# ====================
# Default initialization uses the URL from the WSDL
client <- new_client()
print(client$url)

# 3. Call Operations
# ==================
# Add 10 + 20
result <- Add(client, intA = 10, intB = 20)
print(paste("10 + 20 =", result))

# 4. Configuration (e.g., Basic Auth)
# ===================================
# You can pass httr configuration options to new_client.
# This is useful for services requiring Basic Authentication.
client_auth <- new_client(
    config = httr::authenticate("user", "password")
)

# 5. WS-Security (Headers)
# ========================
# You can add arbitrary SOAP headers using add_header.
# Here we add a dummy header.
header <- xml2::read_xml(
    '<tns:MyHeader xmlns:tns="http://example.com">Value</tns:MyHeader>'
)
client_header <- add_header(client, header)

# 6. Mocking (Testing)
# ====================
# You can inject a custom transport function to mock responses.
# This allows testing your code without making real network requests.
mock_transport <- function(url, action, body, config, handle) {
    # Return a mock response
    # Note: The content must be a raw vector
    content <- charToRaw(
        '
    <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
      <soap:Body>
        <AddResponse xmlns="http://tempuri.org/">
          <AddResult>999</AddResult>
        </AddResponse>
      </soap:Body>
    </soap:Envelope>
  '
    )
    list(status_code = 200, content = content)
}

client_mock <- new_client(transport = mock_transport)
result_mock <- Add(client_mock, intA = 10, intB = 20)
print(paste("Mocked Result:", result_mock))
