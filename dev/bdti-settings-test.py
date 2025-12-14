from zeep import Client
from zeep.helpers import serialize_object
import polars as pl
import time

pl.Config.set_tbl_rows(1000)
pl.Config.set_tbl_cols(1000)


client = Client(wsdl='https://www.barraone.com/bdti/BDTInteractive?wsdl')
service = client.service
factory = client.type_factory('ns0')

# Login
user = "iamquant"
password = "TheCatSatOnTheMat99"
client_id = "kzdr85j9hv"

service.Login(User=user, Client=client_id, Password=password)

# Set Analysis Date
service.SetAnalysisDate("2025-11-30")
service.GetAnalysisDate()

# Set Current Portfolio
status = service.SetCurrentPortfolio(PortfolioName="FOGGEQUI", PortfolioOwner="InvestecAd")
while not status.IsDone:
    time.sleep(0.5)
    status = service.CheckRequestStatus(status.RequestId)

cs = service.GetCurrentSettings(InclSessionOverride=False)
pl.DataFrame(serialize_object(cs))

# Change settings
service.SetCurrentSettings(factory.CurrentSettings(Benchmark="MSCIJPND", BenchmarkOwner="SYSTEM"))

ns = service.GetCurrentSettings(InclSessionOverride=True)
pl.DataFrame(serialize_object(ns))