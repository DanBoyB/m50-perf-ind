con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "{ODBC Driver 13 for SQL Server}",
                      Server = "TIIDB1/PRODUCTION",
                      Database = "TravelTimes",
                      #UID = "tii\\dan.brennan",
                      #PWD = "da22582n1*TII3",
                      Port = 1433,
                      TrustedConnection = "Yes")