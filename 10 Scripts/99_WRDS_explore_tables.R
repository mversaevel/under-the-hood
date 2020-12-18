res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
db_TotalDB <- dbFetch(res, n = -1)
dbClearResult(res)



res <- dbSendQuery(wrds, "select distinct table_name
                   from information_schema.columns
                   where table_schema='crsp'
                   order by table_name")
db_Library <- dbFetch(res, n = -1)
dbClearResult(res)



res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema ='crspa'
                   and table_name ='msf'
                   order by column_name")
db_DataSet <- dbFetch(res, n = -1)
dbClearResult(res)


data <- ""
res <- dbSendQuery(wrds, "select * from compa.funda")
data <- dbFetch(res, n = 100)
dbClearResult(res)
colnames(data) %>% sort()
