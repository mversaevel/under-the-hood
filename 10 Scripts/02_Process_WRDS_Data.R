library(dplyr)


# Read stored WRDS data
df_CRSP <- readRDS("00 Raw Data/df_CRSP.rds")
df_CRSP_COMP_ID <- readRDS("00 Raw Data/df_CRSP_COMP_ID.rds")
df_Compustat <- readRDS("00 Raw Data/df_Compustat.rds")
df_CompustatGICS <- readRDS("00 Raw Data/df_CompustatGICS.rds")
df_CompustatGICS_desc <- readRDS("00 Raw Data/df_CompustatGICS_desc.rds")



### CRSP

# Prepare and clean CRSP/Compustat ID dataset

df_CRSP_COMP_ID_proc <- df_CRSP_COMP_ID %>%
  mutate(TestForDuplicates = paste0(gvkey, ".", lpermco)) %>%
  group_by(TestForDuplicates) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-TestForDuplicates)
  
# Prepare and clean CRSP dataset
df_CRSP_proc <- df_CRSP %>%
  right_join(., df_CRSP_COMP_ID_proc, by = c("permco" = "lpermco"))

df_CRSP_proc <- df_CRSP_proc %>%
  filter(!is.na(ret), !is.na(prc), !is.na(shrout), !is.na(cfacpr), !is.na(cfacshr), cfacpr != 0, cfacshr != 0) %>%
  mutate(prc = abs(prc), #price sometimes unexplicably switches to negative and back to positive from month to month
         Year = substr(date, 1, 4),
         IDandYear = paste0(gvkey, ".", Year))

df_CRSP_proc$Year <- as.numeric(df_CRSP_proc$Year)

# separate block of code that removes incomplete cases where there is no full year (12 month) data available
df_CRSP_proc <- df_CRSP_proc %>%
  group_by(IDandYear) %>%
  mutate(CountIDandYear = n()) %>%
  filter(CountIDandYear == 12) %>%
  select(-CountIDandYear) %>%
  ungroup()

# calculate annual returns, adjustment factors, market cap, 
df_CRSP_proc <- df_CRSP_proc %>%
  group_by(IDandYear) %>%
  mutate(Annualret = prod(1 + ret) - 1) %>%
  ungroup() %>%
  mutate(PRCADJ = prc / cfacpr, #prices after adj factor
         SHROUTADJ = (shrout * cfacshr), #shares outstanding after adj factor
         MarketCap = (PRCADJ * SHROUTADJ) / 1000) %>%
  group_by(IDandYear) %>%
  mutate(AvgMarketCap = mean(MarketCap)) %>%
  ungroup()

# convert to annual data by retaining one month (december is retained, so that market cap is "end of period")
df_CRSP_proc <- df_CRSP_proc %>%
  filter(substr(date, 6, 7) == 12) %>%
  arrange(Year) %>%
  group_by(cusip) %>%
  mutate(MarketCapStartOfYear = lag(MarketCap, n = 1)) #calculate "beginning of period" market cap for proper return calculation


### Compustat (and related tables); adds same id/year as CRSP database for joining (gvkey+year); and joins GICS codes and descriptions from other Compustat tables
df_Compustat_proc <- df_Compustat %>%
  mutate(IDandYear = paste0(gvkey, ".", fyear)) %>%
  left_join(., df_CompustatGICS, by = "gvkey") %>%
  left_join(., df_CompustatGICS_desc, by = c("gsector" = "giccd")) %>%
  rename("gsectorname" = "gicdesc") %>%
  mutate(gsectorname = paste0(gsectorname, " (", gsector, ")")) %>%
  left_join(., df_CompustatGICS_desc, by = c("ggroup" = "giccd")) %>%
  rename("ggroupname" = "gicdesc") %>%
  mutate(ggroupname = paste0(ggroupname, " (", ggroup, ")")) %>%
  left_join(., df_CompustatGICS_desc, by = c("gind" = "giccd")) %>%
  rename("gindname" = "gicdesc") %>%
  mutate(gindname = paste0(gindname, " (", gind, ")")) %>%
  left_join(., df_CompustatGICS_desc, by = c("gsubind" = "giccd")) %>%
  rename("gsubindname" = "gicdesc") %>%
  mutate(gsubindname = paste0(gsubindname, " (", gsubind, ")"))
  
  
### Merge CRSP and Compustat, rename some columns, drop missing sector cases, select relevant subset
IntData <- merge(df_Compustat_proc, df_CRSP_proc, by = "IDandYear") %>% # --> Output of this script: dataframe "IntData"
  rename("gvkey" = "gvkey.x", "cusip" = "cusip.x") %>%
  filter(!is.na(gsector)) %>%
  select(IDandYear, date, Year, fyear, gvkey, cusip, permco, conm, gsector, gsectorname, ggroup, ggroupname, gind, gindname, gsubind, gsubindname, sale, ni, Annualret, MarketCap, MarketCapStartOfYear, AvgMarketCap)

# Store results
saveRDS(IntData, "01 Processed Data/IntData.rds")

#rm(df_CRSP, df_CRSP_COMP_ID, df_Compustat, df_CompustatGICS, df_CompustatGICS_desc)

