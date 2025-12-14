# Reproduce XML Generation
devtools::load_all("c:/workspace/soapr")
library(xml2)

# Mock XMLSerializer
# Assume elementFormDefault="qualified" (as per WSDL)
# Assume targetNamespace="http://barra.com/cp/bdti"
serializer <- soapr::XMLSerializer$new(element_form_default = "qualified", target_namespace = "http://barra.com/cp/bdti")

# Create a root node with namespace definition
root <- xml_new_root("Envelope",
    "xmlns:ns1" = "http://barra.com/cp/bdti",
    "xmlns:soapenv" = "http://schemas.xmlsoap.org/soap/envelope/"
)

# Create Body and Operation Wrapper (SetCurrentSettings)
body <- xml_add_child(root, "soapenv:Body")
op <- xml_add_child(body, "ns1:SetCurrentSettings")

# Create the object to serialize
# CurrentSettings is a complex type with attributes
cs_obj <- list(LossTarget = 99.99, Benchmark = "MSCIJPND")
# soapr usually marks complex objects with class "soap_object" and "soap_attrs"
class(cs_obj) <- c("soap_object", "list")
attr(cs_obj, "soap_attrs") <- c("LossTarget", "Benchmark") # These are attributes

# Serialize
serializer$serialize(op, "CurrentSettings", cs_obj) # Name from parameter

cat("Generated XML:\n")
cat(as.character(root), "\n")
