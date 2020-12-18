#Packages required but not necessary to load in namespace
  #PerformanceAnalytics::
  #quantmod::
  #zoo::
  #tidyr::

library(dplyr)

#Settings:
options(scipen = 999)
int_PeriodVector <- c(5, 10)
str_IndustryVector <- c("gind", "gsubind")

#Parameters below are not settings, but required configurations in order for the code to run correctly
#------
StartTime <- Sys.time()
int_i <- 1 #this variable acts as a "counter" in the for-loop, more concretely, to properly store the output in list form
lst_IntData2 <- list() #sets empty list
lst_PhaseDataModel1 <- list() #sets empty list
lst_PhaseDataModel2 <- list() #sets empty list
IntData <- readRDS("01 Processed Data/IntData.rds") #Load required input data, which is the output from script 02_Process_WRDS_Data.R
source("10 Scripts/99_Custom_Functions.R") #Load custom functions
#------


for(int_Period in int_PeriodVector){
  for(str_Industry in str_IndustryVector){

    #the code below identifies and removes industries for which there is not enough data to calculate long run figures
    #for instance, for an industry with only 10 years of data, one cannot calculate 15 year averages. These are dropped from the sample depending on the required length (int_Period)
    str_IndustriesToKeep <- IntData %>%
      group_by_at(c(str_Industry, "Year")) %>%
      summarize() %>%
      group_by_at(str_Industry) %>%
      summarize(count = n()) %>%
      filter(count > int_Period) %>%
      pull(!!str_Industry)
    
    IntData <- IntData %>%
      filter(eval(as.name(str_Industry)) %in% str_IndustriesToKeep)
    
    rm(str_IndustriesToKeep)
    
    #there appears to be a data issue surrounding Philip Morris Intl and Altria; the 2009 spinoff of Philip Morris Intl from Altria group adds about 90 billion of market cap out of nowhere. This corporate action seems to have been administrated incorrectly. Therefore, the company is removed from the sample.
    #IntData <- filter(IntData, permco != 52978) 
    
    #=======================================================================================================================
    # The following code adds to df_IntData2 instead of making changes to the original dataframe (IntData)
    # The structure of this data is such that it is aggregated from the company level to industry level, while retaining annual periodicity
    #=======================================================================================================================
 
    df_IntData2 <- IntData %>% 
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize()
    
    #Adds columns with specific scenario
    df_IntData2 <- df_IntData2 %>%
      mutate(IndustryLevel = str_Industry,
             IndustryName = eval(as.name(paste0(str_Industry, "name"))),
             TimeHorizon = paste0(int_Period,"-years"),
             Setting_IndustryTime = paste0(IndustryLevel,int_Period,"yr"))
    
    # =============================================
    # Construct new variables with annual frequency
    # =============================================
    
    #Sales: absolute
    df_IntData2 <- IntData %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(TotalSales = sum(sale, na.rm = TRUE)) %>%
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year")) #add to pre-existing dataframe (df_IntData2)

    #No of producers
    df_IntData2 <- IntData %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(TotalNumberOfCompanies = n()) %>% #Number of companies per industry
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year")) #add to pre-existing dataframe (df_IntData2)

    #Market cap metrics
    df_IntData2 <- IntData %>%
      group_by(Year) %>%
      mutate(RelMarketCapPerCompany = AvgMarketCap/sum(AvgMarketCap, na.rm = TRUE)) %>%
      ungroup %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(TotalMarketCap = sum(AvgMarketCap, na.rm = TRUE),
                RelMarketCap = sum(RelMarketCapPerCompany, na.rm=TRUE)) %>% #RelIndSizeByMarketCap (Relative size of an industry by looking at market cap)
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year")) #add to pre-existing dataframe (df_IntData2)
    
    #Net income, Profit margin and Market Share
    df_IntData2 <- IntData %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(TotalNetIncome = sum(ni, na.rm = TRUE),
                ProfitMargin = TotalNetIncome / sum(sale, na.rm = TRUE)) %>%
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year")) #add to pre-existing dataframe (df_IntData2)
    

    # ===================================================================================================================================
    # One year Delta (YoY) metrics, possibly based on metrics above. Length of these vectors are n, starting with one NA and n-1 YoY metrics
    # ===================================================================================================================================
    
    #Change in market share (YoY)
    df_IntData2 <- IntData %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      mutate(MarketSharePerCompany = sale / sum(sale, na.rm = TRUE)) %>%
      ungroup %>%
      group_by(permco) %>%
      arrange(Year) %>% #must be sorted chronologically, otherwise the YoY calculations will not make any sense
      mutate(MarketSharePerCompanyYoY = simple_diff(MarketSharePerCompany, 1)) %>%
      ungroup %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(MarketShareInstabilityScaled = round(sum(abs(MarketSharePerCompanyYoY), na.rm = TRUE) / n(), 8)) %>%
      mutate(MarketShareInstabilityScaled = replace(MarketShareInstabilityScaled, MarketShareInstabilityScaled == 0, NA)) %>% #add NA values
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year"))

    #Changes in number of companies: entries
    #As the dataset is grouped by company, the lag(Year) checks: what was the first year for each company in the dataset? If the first year is found, it returns NA. This lagged year (+1) is then combined with permco and matched with the IDandYear key. Only for the first year (which returned NA) are these non identical.
    df_IntData2 <- IntData %>%
      group_by(permco) %>%
      arrange(Year) %>% #must be sorted chronologically, otherwise the lag/lead shifts will not make any sense
      mutate(LaggedIDandYear = paste0(permco, ".", dplyr::lag(Year + 1, 1))) %>%   
      mutate(CompanyNewToSample = case_when(IDandYear == LaggedIDandYear ~ 0,
                                            TRUE ~ 1)) %>%
      ungroup() %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(NumberOfEntries = sum(CompanyNewToSample, na.rm = TRUE)) %>%
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year"))
    
    #changes in number of companies: exits
    #Works same as entries, but the other way around: identifies last year in sample. To aggregate to industry level and address the question of what year the company exits, the aggregate count needs to be shifted ahead one year (lag). This is a necessity because companies that stop existing can only be counted when they still exist; which is one year BEFORE they drop from the sample. Hence, the required lagging.
    df_IntData2 <- IntData %>%
      group_by(permco) %>%
      arrange(Year) %>% #must be sorted chronologically, otherwise the lag/lead shifts will not make any sense      
      mutate(LeadIDandYear = paste0(permco, ".", dplyr::lead(Year - 1, 1))) %>%
      mutate(CompanyLastYearInSample = case_when(IDandYear == LeadIDandYear ~ 0,
                                                 TRUE ~ 1)) %>%
      ungroup() %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(NumberOfExits = sum(CompanyLastYearInSample, na.rm = TRUE)) %>%
      mutate(NumberOfExits = dplyr::lag(NumberOfExits, 1, default = 0)) %>% #default = 0 means that non-existent rows are not assigned NA, but 0
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year"))
    
    
    #turbulence: absolute industry turbulence and average (aka scaled) for industry size by number of companies
    df_IntData2 <- df_IntData2 %>%
      mutate(IndustryTurbulence = NumberOfEntries + NumberOfExits,
             IndustryTurbulenceScaled = IndustryTurbulence / TotalNumberOfCompanies)

    # ===================================================================================================================================
    # Multiyear delta, average, or growth (CAGR) metrics. Period length in years and determined by global parameter "int_Period". Length of these vectors are n, starting with #int_Period NA and #(n - int_Period) metrics
    # Note that these mostly operate on already calculated metrics, aggregated at the industry level, in df_IntData2
    # ===================================================================================================================================    

    #Growth metrics and LT avg metrics: sales   
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(TotalSalesPeriodAVG = zoo::rollapply(data = TotalSales, 
                                                  FUN = mean, width = int_Period, align = "right",
                                                  na.rm = TRUE, fill = NA),
             TotalSalesPeriodCAGR = (1 + quantmod::Delt(TotalSales, k = int_Period)) ^ (1 / int_Period) - 1,
             TotalSalesPeriodCAGR = replace(TotalSalesPeriodCAGR, TotalSalesPeriodCAGR == Inf, NA)) %>%
      ungroup()
    
    #Growth bucket/decile: sales
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
        mutate(TotalSalesPeriodAVGDecile = .bincode(TotalSalesPeriodAVG, 
                                                     breaks = quantile(df_IntData2$TotalSalesPeriodAVG, probs = seq(0, 1, 0.1), na.rm = TRUE)),
               TotalSalesPeriodCAGRDecile = .bincode(TotalSalesPeriodCAGR, 
                                                     breaks = quantile(df_IntData2$TotalSalesPeriodCAGR, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>% 
      ungroup()
    
    #Growth median: sales above/below median
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(TotalSalesPeriodCAGRMedian = median(TotalSalesPeriodCAGR, na.rm = TRUE),
             TotalSalesPeriodCAGRMedianHighLow = case_when(TotalSalesPeriodCAGR <= median(TotalSalesPeriodCAGR, na.rm = TRUE) ~ "BelowMedian",
                                                           TotalSalesPeriodCAGR > median(TotalSalesPeriodCAGR, na.rm = TRUE) ~ "AboveMedian")) %>%
      select(-TotalSalesPeriodCAGRMedian) %>%
      ungroup()

    #Growth metrics: no. of producers
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(TotalNumberOfCompaniesPeriodAVG = zoo::rollapply(data = TotalNumberOfCompanies, 
                                                              FUN = mean, width = int_Period, align = "right",
                                                              na.rm = TRUE, fill = NA),
             TotalNumberOfCompaniesPeriodCAGR = (1 + quantmod::Delt(TotalNumberOfCompanies, k = int_Period)) ^ (1 / int_Period) - 1,
             TotalNumberOfCompaniesPeriodCAGR = replace(TotalNumberOfCompaniesPeriodCAGR, TotalNumberOfCompaniesPeriodCAGR == Inf, NA)) %>%
      ungroup()
    
    #Growth bucket/decile: no of producers
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(TotalNumberOfCompaniesPeriodAVGDecile = .bincode(TotalNumberOfCompaniesPeriodAVG, 
                                                              breaks = quantile(df_IntData2$TotalNumberOfCompaniesPeriodAVG, probs = seq(0, 1, 0.1), na.rm = TRUE)),
             TotalNumberOfCompaniesPeriodCAGRDecile = .bincode(TotalNumberOfCompaniesPeriodCAGR, 
                                                               breaks = quantile(df_IntData2$TotalNumberOfCompaniesPeriodCAGR, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>%
      ungroup()    

    #Growth median: no of producers above/below median
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(TotalNumberOfCompaniesPeriodCAGRMedian = median(TotalNumberOfCompaniesPeriodCAGR, na.rm = TRUE),
             TotalNumberOfCompaniesPeriodCAGRMedianHighLow = case_when(TotalNumberOfCompaniesPeriodCAGR <= median(TotalNumberOfCompaniesPeriodCAGR, na.rm = TRUE) ~ "BelowMedian",
                                                                       TotalNumberOfCompaniesPeriodCAGR > median(TotalNumberOfCompaniesPeriodCAGR, na.rm = TRUE) ~ "AboveMedian")) %>%
      select(-TotalNumberOfCompaniesPeriodCAGRMedian) %>%
      ungroup()    

    #Growth metrics: development of market share instability
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(MarketShareInstabilityScaledPeriodAVG = zoo::rollapply(data = MarketShareInstabilityScaled, 
                                                          FUN = mean, width = int_Period, align = "right",
                                                          na.rm = TRUE, fill = NA),
             MarketShareInstabilityScaledPeriodCAGR = (1 + quantmod::Delt(MarketShareInstabilityScaled, k = int_Period)) ^ (1 / int_Period) - 1,
             MarketShareInstabilityScaledPeriodCAGR = replace(MarketShareInstabilityScaledPeriodCAGR, MarketShareInstabilityScaledPeriodCAGR == Inf, NA)) %>%
      ungroup()
    
    #Growth bucket/decile: development of market share instability
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(MarketShareInstabilityScaledPeriodAVGDecile = .bincode(MarketShareInstabilityScaledPeriodAVG, 
                                                                    breaks = quantile(df_IntData2$MarketShareInstabilityScaledPeriodAVG, probs = seq(0, 1, 0.1), na.rm = TRUE)),
             MarketShareInstabilityScaledPeriodCAGRDecile = .bincode(MarketShareInstabilityScaledPeriodCAGR,
                                                                     breaks = quantile(df_IntData2$MarketShareInstabilityScaledPeriodCAGR, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>%
      ungroup()
        
    #Growth metrics: development of turbulence
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(IndustryTurbulenceScaledPeriodAVG = zoo::rollapply(data = IndustryTurbulenceScaled, 
                                                                FUN = mean, width = int_Period, align = "right",
                                                                na.rm = TRUE, fill = NA),
             IndustryTurbulenceScaledPeriodCAGR = (1 + quantmod::Delt(IndustryTurbulenceScaled, k = int_Period)) ^ (1 / int_Period) - 1,
             IndustryTurbulenceScaledPeriodCAGR = replace(IndustryTurbulenceScaledPeriodCAGR, IndustryTurbulenceScaledPeriodCAGR == Inf, NA)) %>%
      ungroup()

    #Growth bucket/decile: development of turbulence
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(IndustryTurbulenceScaledPeriodAVGDecile = .bincode(IndustryTurbulenceScaledPeriodAVG, 
                                                          breaks = quantile(df_IntData2$IndustryTurbulenceScaledPeriodAVG, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>%
      mutate(IndustryTurbulenceScaledPeriodCAGRDecile = .bincode(IndustryTurbulenceScaledPeriodCAGR, 
                                                           breaks = quantile(df_IntData2$IndustryTurbulenceScaledPeriodCAGR, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>%
      ungroup()
    
    #Growth metrics: development of profit margin
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(ProfitMarginPeriodAVG = zoo::rollapply(data = 1 + ProfitMargin,
                                                     FUN = prod, width = int_Period, align = "right", 
                                                     na.rm = TRUE, fill = NA) ^ (1 / int_Period) - 1,
             ProfitMarginPeriodAVGDelta = quantmod::Delt(ProfitMarginPeriodAVG, k = 1),
             ProfitMarginPeriodAVGDelta = replace(ProfitMarginPeriodAVGDelta, ProfitMarginPeriodAVGDelta == Inf, NA)) %>%
      ungroup()
    
    #Growth bucket/decile: development of profit margin
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(ProfitMarginPeriodAVGDecile = .bincode(ProfitMarginPeriodAVG, 
                                                    breaks = quantile(df_IntData2$ProfitMarginPeriodAVG, probs = seq(0, 1, 0.1), na.rm = TRUE)),
             ProfitMarginPeriodAVGDeltaDecile = .bincode(ProfitMarginPeriodAVGDelta, 
                                                         breaks = quantile(df_IntData2$ProfitMarginPeriodAVGDelta, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>%
      ungroup()
    
    #######

    #=======================================================================================================================
    # Calculate returns by years/industry, both value and equal weighted, and bucket into deciles
    #=======================================================================================================================        
    
    # Value Weighted returns 
    #-----------------------

    #Normal value weighted (cap weighted) returns. Adds from IntData to industry level dataframe
    df_IntData2 <- IntData %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      mutate(MarketCapWeightWithinIndustry = MarketCapStartOfYear / sum(MarketCapStartOfYear, na.rm = TRUE), #First, calculate market cap weight within industry
             IndustryVWR = MarketCapWeightWithinIndustry * Annualret) %>%
      summarize(IndustryVWR = sum(IndustryVWR, na.rm = TRUE)) %>%
      #mutate(IndustryVWR = case_when(IndustryVWR < 1 ~ IndustryVWR,
      #                               IndustryVWR > 1 ~ 1)) %>% #cap industry returns at 100%
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year"))
    
    #Excess returns (vs market)
    df_IntData2 <- IntData %>%
      group_by(Year) %>%
      summarize(MarketReturn = sum((MarketCapStartOfYear / sum(MarketCapStartOfYear, na.rm = TRUE)) * Annualret, na.rm = TRUE)) %>%
      left_join(df_IntData2, ., by = "Year") %>%
      mutate(IndustryVWR_Excess = (1 + IndustryVWR) / (1 + MarketReturn) - 1 ) %>%
      select(-MarketReturn)

    #Rolling, forward 5 year value weighted returns
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(IndustryVWR5yrForward = ShiftUp(zoo::rollapply(data = IndustryVWR, 
                                                FUN = PerformanceAnalytics::mean.geometric, 
                                                width = 5, partial = FALSE, align = "right", na.rm = TRUE, fill = NA),
                                             4),
             IndustryVWR_Excess5yrForward = ShiftUp(zoo::rollapply(data = IndustryVWR_Excess, 
                                                            FUN = PerformanceAnalytics::mean.geometric, 
                                                            width = 5, partial = FALSE, align = "right", na.rm = TRUE, fill = NA),
                                                    4)) %>%
      ungroup()    
         
    #Cumulative log returns of value weighted returns
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(IndustryVWRCumulativeLog = log(cumprodSkipNA(1 + IndustryVWR))) %>%
      ungroup()
    
    # Equal Weighted returns 
    #-----------------------
      
    #Equal weighted returns. Note that equal weights (1/n) with "n" at the end of period are used, not beginning of period. Impact should not be large, especially for the research topic in question, but is a "nice to have" area of improvement nonetheless
    df_IntData2 <- IntData %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      summarize(IndustryEWR = sum(Annualret * (1 / n()), na.rm = TRUE)) %>%
      #mutate(IndustryEWR = case_when(IndustryEWR < 1 ~ IndustryEWR,
      #                               IndustryEWR > 1 ~ 1)) %>% #cap industry returns at 100%
      left_join(df_IntData2, ., by = c(str_Industry, paste0(str_Industry, "name"), "Year"))

    #Equal weighted excess returns (vs market)
    df_IntData2 <- IntData %>%
      group_by(Year) %>%
      summarize(MarketEWR = sum(Annualret * (1/ n()), na.rm = TRUE)) %>%
      left_join(df_IntData2, ., by = "Year") %>%
      mutate(IndustryEWR_Excess = (1 + IndustryEWR) / (1 + MarketEWR) - 1 ) %>%
      select(-MarketEWR)
    
    #Rolling multiyear equal weighted returns
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(IndustryEWRPeriod = zoo::rollapply(data = IndustryEWR, 
                                                FUN = PerformanceAnalytics::mean.geometric, 
                                                width = int_Period, align = "right", na.rm = TRUE, fill = NA)) %>%
      ungroup()

    #Rolling multiyear excess equal weighted returns
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(IndustryEWR_ExcessPeriod = zoo::rollapply(data = IndustryEWR_Excess, 
                                                       FUN = PerformanceAnalytics::mean.geometric, 
                                                       width = int_Period, partial = FALSE, align = "right", na.rm = TRUE, fill = NA)) %>%
      ungroup()        
    
    #Deciles/buckets of rolling multiyear equal weighted returns    
    df_IntData2 <- df_IntData2 %>%
      group_by(Year) %>%
      mutate(IndustryEWRPeriodDecile = .bincode(IndustryEWRPeriod, 
                                                breaks = quantile(df_IntData2$IndustryEWRPeriod, probs = seq(0, 1, 0.1), na.rm = TRUE))) %>%
      ungroup()
    
    #Shifted (forward) rolling multiyear equal weighted returns (shift by int_Period)
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      mutate(IndustryEWRForward = ShiftUp(IndustryEWRPeriod, int_Period - 1)) %>% #"rollapply" has different window than "quantmod::Delt" - so requires a different shift 
      ungroup()    
    
    
    #Cumulative log returns of equal weighted returns
    df_IntData2 <- df_IntData2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%        
      mutate(IndustryEWRCumulativeLog = log(cumprodSkipNA(1 + IndustryEWR))) %>%
      ungroup() 


    #=======================================================================================================================
    # Calculate phase models
    #=======================================================================================================================        
    
    #### Model 1:
    df_PhaseDataModel1 <- df_IntData2 %>%
      filter(Year > (min(Year) + int_Period)) %>% #filter out observations that fall outside of the x-year rolling time window
      mutate(PhaseModel = "Model1", 
             Phase1 = case_when(TotalNumberOfCompaniesPeriodAVGDecile <= 2 & 
                                TotalSalesPeriodAVGDecile <= 3 & 
                                TotalSalesPeriodCAGRDecile >= 8 ~ 1,
                                TRUE ~ 0),
             Phase2 = case_when(TotalNumberOfCompaniesPeriodCAGRDecile >= 8 &
                                TotalSalesPeriodCAGRDecile >= 8 &
                                MarketShareInstabilityScaledPeriodAVGDecile >= 8 & 
                                Phase1 != 1 ~ 1,
                                TRUE ~ 0),
             Phase3 = case_when(TotalNumberOfCompaniesPeriodCAGR >= -0.03 & 
                                TotalNumberOfCompaniesPeriodCAGR <= 0.03 & 
                                IndustryTurbulenceScaledPeriodAVGDecile >= 7 &
                                ProfitMarginPeriodAVGDeltaDecile <= 3 &
                                Phase1 != 1 & 
                                Phase2 != 1 ~ 1,
                                TRUE ~ 0),
             Phase4 = case_when(TotalSalesPeriodCAGR >= -0.02 & 
                                TotalSalesPeriodCAGR <= 0.02 &
                                TotalNumberOfCompaniesPeriodCAGR <= 0 &
                                MarketShareInstabilityScaledPeriodAVGDecile <= 3 &  
                                Phase1 != 1 &
                                Phase2 != 1 &
                                Phase3 != 1 ~ 1,
                                TRUE ~ 0),
             Phase5 = case_when(TotalSalesPeriodCAGRDecile <= 3 &
                                TotalSalesPeriodCAGR <= 0 &
                                TotalNumberOfCompaniesPeriodCAGR <= 0 &
                                Phase1 != 1 &
                                Phase2 != 1 &
                                Phase3 != 1 &
                                Phase4 != 1 ~ 1,
                                TRUE ~ 0),
             Phase = case_when(Phase1 == 1 ~ 1,
                               Phase2 == 1 ~ 2,
                               Phase3 == 1 ~ 3,
                               Phase4 == 1 ~ 4,
                               Phase5 == 1 ~ 5,
                               TRUE ~ 0), #0 means missing data
             PhaseName = case_when(Phase == 1 ~ "Phase 1: Birth",
                                   Phase == 2 ~ "Phase 2: Growth",
                                   Phase == 3 ~ "Phase 3: Shake-out",
                                   Phase == 4 ~ "Phase 4: Consolidation",
                                   Phase == 5 ~ "Phase 5: Decline",
                                   Phase == 0 ~ "No Phase"))
    
    # Save results for model 1. The following adds coded columns "previous to current phase", signifying at point t=0 the transition from the previous to the current phase
    # This is done for in two separate variations. The "PreviousPhaseToCurrentPhaseExclNoPhase" vector first excludes all the phases that are 0 (no classification)
    # The other "PreviousPhaseToCurrentPhase" does retain the phase 0 observations.
    
    df_PhaseDataModel1ExclNoPhase <- df_PhaseDataModel1 %>%
      filter(Phase != 0) %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      arrange(Year) %>%
      mutate(PreviousPhase = dplyr::lag(Phase, n = 1),
             PreviousPhaseToCurrentPhaseExclNoPhase = paste0(PreviousPhase, ".", Phase)) %>%
      tidyr::drop_na(PreviousPhase, PreviousPhaseToCurrentPhaseExclNoPhase) %>%
      ungroup() %>%
      select(Year, c(str_Industry, paste0(str_Industry, "name")), PreviousPhaseToCurrentPhaseExclNoPhase)
    
    df_PhaseDataModel1 <- df_PhaseDataModel1 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      arrange(Year) %>%
      mutate(PreviousPhase = dplyr::lag(Phase, n = 1),
             PreviousPhaseToCurrentPhase = paste0(PreviousPhase, ".", Phase)) %>%
      tidyr::drop_na(PreviousPhase, PreviousPhaseToCurrentPhase) %>%
      ungroup() %>%
      left_join(., df_PhaseDataModel1ExclNoPhase, by = c(str_Industry, paste0(str_Industry, "name"), "Year")) %>%
      select(Year, IndustryName, TimeHorizon, Setting_IndustryTime, TotalMarketCap, IndustryVWR, IndustryVWR_Excess, IndustryVWR5yrForward, IndustryVWR_Excess5yrForward, IndustryEWR, Phase, PhaseName, PreviousPhaseToCurrentPhase, PreviousPhaseToCurrentPhaseExclNoPhase) #22/8/20 removed equal weighted returns
    
    rm(df_PhaseDataModel1ExclNoPhase)

    #### Model 2:
    df_PhaseDataModel2 <- df_IntData2 %>%
      mutate(PhaseModel = "Model2",
             Phase1 = case_when(TotalSalesPeriodCAGRMedianHighLow == "AboveMedian" & 
                                TotalNumberOfCompaniesPeriodCAGRMedianHighLow == "AboveMedian" ~ 1,
                                TRUE ~ 0),
             Phase2 = case_when(TotalSalesPeriodCAGRMedianHighLow == "AboveMedian" & 
                                TotalNumberOfCompaniesPeriodCAGRMedianHighLow == "BelowMedian" ~ 1,
                                TRUE ~ 0),
             Phase3 = case_when(TotalSalesPeriodCAGRMedianHighLow == "BelowMedian" & 
                                TotalNumberOfCompaniesPeriodCAGRMedianHighLow == "AboveMedian" ~ 1,
                                TRUE ~ 0),
             Phase4 = case_when(TotalSalesPeriodCAGRMedianHighLow == "BelowMedian" & 
                                TotalNumberOfCompaniesPeriodCAGRMedianHighLow == "BelowMedian" ~ 1,
                                TRUE ~ 0),
             Phase = case_when(Phase1 == 1 ~ 1,
                               Phase2 == 1 ~ 2,
                               Phase3 == 1 ~ 3,
                               Phase4 == 1 ~ 4,
                               TRUE ~ 999), #999 means missing data
             PhaseName = case_when(Phase == 1 ~ "Phase 1: Growth",
                                   Phase == 2 ~ "Phase 2: Consolidating",
                                   Phase == 3 ~ "Phase 3: Technological Change",
                                   Phase == 4 ~ "Phase 4: Declining")) %>%
      filter(Phase != 999)
    
    df_PhaseDataModel2 <- df_PhaseDataModel2 %>%
      group_by_at(c(str_Industry, paste0(str_Industry, "name"))) %>%
      arrange(Year) %>%
      mutate(PreviousPhase = dplyr::lag(Phase, n = 1),
             PreviousPhaseToCurrentPhase = paste0(PreviousPhase, ".", Phase)) %>%
      tidyr::drop_na(PreviousPhase, PreviousPhaseToCurrentPhase) %>%
      ungroup() %>%
      select(Year, IndustryName, TimeHorizon, Setting_IndustryTime, TotalMarketCap, IndustryVWR, IndustryVWR_Excess, IndustryVWR5yrForward, IndustryVWR_Excess5yrForward, IndustryEWR, Phase, PhaseName, PreviousPhaseToCurrentPhase) #22/8/20 removed equal weighted returns
 
    #save to drive
    lst_IntData2[[int_i]] <- data.frame(df_IntData2)
    names(lst_IntData2)[[int_i]] <- paste0("Data", str_Industry, int_Period, "yr")        
    
    lst_PhaseDataModel1[[int_i]] <- data.frame(df_PhaseDataModel1)
    names(lst_PhaseDataModel1)[[int_i]] <- paste0("PhaseDataModel1", str_Industry, int_Period, "yr")
    
    lst_PhaseDataModel2[[int_i]] <- data.frame(df_PhaseDataModel2)
    names(lst_PhaseDataModel2)[[int_i]] <- paste0("PhaseDataModel2", str_Industry, int_Period, "yr")
    
    rm(df_IntData2, df_PhaseDataModel1, df_PhaseDataModel2)
    gc() # Collect garbage

    int_i <- int_i + 1

  }
}
    

#Store results
#------

saveRDS(lst_IntData2, "01 Processed Data/lst_IntData2.rds") # --> Output of this script: list "Data"
saveRDS(lst_PhaseDataModel1, "01 Processed Data/lst_IntDataModel1.rds")
saveRDS(lst_PhaseDataModel2, "01 Processed Data/lst_IntDataModel2.rds")
    
    
rm(IntData, int_i, int_Period,  str_Industry) #remove temporary values/variables

#time elapsed:
EndTime <- Sys.time() 
print(EndTime - StartTime)
rm(StartTime, EndTime)

