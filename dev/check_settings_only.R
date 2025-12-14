library(httr)
library(xml2)

outfile <- "c:/workspace/soapr/settings_state.txt"
con <- file(outfile, open = "wt")
sink(con, type = "output")
sink(con, type = "message")

wsdl_url <- "https://www.barraone.com/bdti/BDTInteractive?wsdl"
handle <- httr::handle(wsdl_url)

send_soap_xml <- function(xml_body, action = "") {
    response <- httr::POST(
        url = wsdl_url,
        handle = handle,
        body = xml_body,
        encode = "raw",
        httr::add_headers(
            "Content-Type" = "text/xml; charset=utf-8",
            "SOAPAction" = action
        )
    )
    httr::stop_for_status(response)
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    cat("Status:", httr::status_code(response), "\n")
    return(content)
}

# 1. Login
cat("1. Logging in...\n")
xml_login <- '
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <Login xmlns="http://barra.com/cp/bdti">
      <User>iamquant</User>
      <Client>kzdr85j9hv</Client>
      <Password>TheCatSatOnTheMat99</Password>
    </Login>
  </soap:Body>
</soap:Envelope>
'
send_soap_xml(xml_login)

# 2. SetAnalysisDate
cat("2. SetAnalysisDate...\n")
xml_date <- '
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <SetAnalysisDate xmlns="http://barra.com/cp/bdti">
      <AnalysisDate>2025-11-30</AnalysisDate>
    </SetAnalysisDate>
  </soap:Body>
</soap:Envelope>
'
send_soap_xml(xml_date)

# 3. SetCurrentPortfolio
cat("3. SetCurrentPortfolio...\n")
xml_port <- '
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <SetCurrentPortfolio xmlns="http://barra.com/cp/bdti">
      <PortfolioName>FOGGEQUI</PortfolioName>
      <PortfolioOwner>InvestecAd</PortfolioOwner>
    </SetCurrentPortfolio>
  </soap:Body>
</soap:Envelope>
'
send_soap_xml(xml_port)

# 4. GetCurrentSettings
cat("4. GetCurrentSettings...\n")
xml_get <- '
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <GetCurrentSettings xmlns="http://barra.com/cp/bdti"/>
  </soap:Body>
</soap:Envelope>
'
res <- send_soap_xml(xml_get)
print(res)

# 5. Logout
cat("5. Logout...\n")
xml_logout <- '
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Body>
    <Logout xmlns="http://barra.com/cp/bdti"/>
  </soap:Body>
</soap:Envelope>
'
send_soap_xml(xml_logout)

sink()
