library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "/usr/local/lib/psqlodbcw.so",
                      Server   = "",
                      Database = "jira",
                      UID      = rstudioapi::askForPassword("Database user"),
                      PWD      = rstudioapi::askForPassword("Database password"),
                      Port     = 5432)

return(con)


jira_df <- tbl(con, in_schema("kafka", "jira_daily_statuses")) %>% 
  collect()

jira_df$project_key[jira_df$project_key == "KAF"][1]

jira_df$project_key







