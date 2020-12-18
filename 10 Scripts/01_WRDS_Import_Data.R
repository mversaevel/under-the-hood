# CRSP
res <- dbSendQuery(wrds, "select cusip,date,permco,ret,prc,shrout,cfacpr,cfacshr
                   from crsp.msf
                   where date > '12-31-1949'")
df_CRSP <- dbFetch(res, n = -1)
dbClearResult(res)


# CRSP / Compustat lookup table from CRSP
res <- dbSendQuery(wrds, "select gvkey,lpermco 
                   from crsp.ccm_lookup")
df_CRSP_COMP_ID <- dbFetch(res, n = -1)
dbClearResult(res)


# Compustat fundamental data
res <- dbSendQuery(wrds, "select cusip,gvkey,datadate,fyear,conm,ni,sale
                   from compa.funda
                   where consol = 'C' AND datafmt = 'STD' AND popsrc = 'D'")
df_Compustat <- dbFetch(res, n = -1)
dbClearResult(res)


# Compustat sector data
res <- dbSendQuery(wrds, "select gvkey,ggroup,gind,gsector,gsubind
                   from compa.company")
df_CompustatGICS <- dbFetch(res, n = -1)
dbClearResult(res)


# Compustat GICS sector descriptions
res <- dbSendQuery(wrds, "select giccd, gicdesc
                   from compa.r_giccd")
df_CompustatGICS_desc <- dbFetch(res, n = -1)
dbClearResult(res)

# Store results
saveRDS(df_CRSP, "00 Raw Data/df_CRSP.rds")
saveRDS(df_CRSP_COMP_ID, "00 Raw Data/df_CRSP_COMP_ID.rds")
saveRDS(df_Compustat, "00 Raw Data/df_Compustat.rds")
saveRDS(df_CompustatGICS, "00 Raw Data/df_CompustatGICS.rds")
saveRDS(df_CompustatGICS_desc, "00 Raw Data/df_CompustatGICS_desc.rds")

rm(df_CRSP, df_CRSP_COMP_ID, df_Compustat, df_CompustatGICS, df_CompustatGICS_desc, res)








