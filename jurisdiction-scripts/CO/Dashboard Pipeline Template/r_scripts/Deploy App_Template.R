rsconnect::setAccountInfo(name='ENTER ACCOUNT NAME',
                          token='ENTER TOKEN', 
                          secret='ENTER SECRET')
rsconnect::deployApp(appDir="dashboards/InternalDashboard",
                     appName = "WWInternalDashboard_NWSSTemplate",
                     forceUpdate=TRUE,
                     account = "ENTER ACCOUNT NAME",
                     upload = TRUE
)
