### All user functions



#custom shift-function:
ShiftUp <- function(x, n){
  c(x[-(seq(n))], rep(NA, n)) 
} 

ShiftDown <- function(x, n){
  c(rep(NA, n), x[(seq(length(x)-n))]) 
}



#custom cumulative product function ( courtesy of https://stackoverflow.com/questions/25576358/calculate-cumsum-while-ignoring-na-values ):
cumprodSkipNA <- function(x) { 
  stopifnot(is.vector(x), is.numeric(x))
  x[!is.na(x)] <- cumprod(x[!is.na(x)])
  x
}



#custom difference function; home made - previously called custom_diff
simple_diff <- function(x, lag, NA_at_start = TRUE){
  #This function requires a numeric vector as inputs. Let the vector be length n. The function will calculate the difference between each of these values, where the lag (k) can also be specified: [from (1+k) to n]((Vn - V(n-k))
  #Suppose input vector {1,2,3,-4,5} with lag = 1
  #Calculates: (2-1), (3-2), ((-4)-3), (5-(-4)) == {1,1,-7,9}
  #Let input vector be length n. The function will always return a vector of length n
  #Following the example above, with setting NA_at_start=TRUE, the function will return {NA,1,1,7,9}
  #The lag specifices which difference to take. Lag 2 will calculate V3-V1, V4-V2, ... Vn - V(n-2)
  if(length(x) <= lag){return(rep(NA,length(x)))} #catches instances where the specified lag (k) is bigger than the number of elements in the vector (n) - returns vector of NA's of length x
  if(NA_at_start==TRUE){
    output <- c(rep(NA,lag), 
                x[(1+lag):length(x)] - x[1:(length(x)-lag)])
  }
  if(NA_at_start==FALSE){
    output <- c(x[(1+lag):length(x)] - x[1:(length(x)-lag)], 
                rep(NA,lag))
  }
  return(output)
}



#custom cumulative absolute difference function; home made
cum_abs_diff <- function(x, lag, NA_at_start = TRUE){
  #this is a cumulative absolute difference function - inputs a vector of length x, calculates the sum of absolute difference over 2 or more (variable: lag) entries of that vector, and outputs a vector of length x
  #the difference in length of x and number of lags is filled with NA, either at the start (NA_at_start = TRUE)
  output <- NULL
  intermediate_output <- NULL
  tmpvec1 <- as.double(0)
  
  if(length(x) <= lag){return(rep(NA,length(x)))} #catches instances where input vector x is too small relative to the specified lag - returns vector of NA's of length x
  
  for(i in 1:(length(x)-lag)){
    tmpvec1 <- x[i:(i+lag)]
    tmpvec2 <- as.double(0) #reset 
    for(j in 1:(length(tmpvec1)-1)){
      tmpvec2 <- tmpvec2 + (abs(tmpvec1[j+1] - tmpvec1[j]))
    }
    intermediate_output <- c(intermediate_output, tmpvec2)
  }
  if(NA_at_start==TRUE){
    output <- c(rep(NA,lag), 
                intermediate_output)
  }
  if(NA_at_start==FALSE){
    output <- c(intermediate_output, 
                rep(NA,lag))
  }    
  return(output)  
}



ProcessPhaseData <- function(df_Data, str_Year, str_Industry, str_PhaseInput, str_TotalMarketCap, str_IndustryVWR,
                             int_PhasePeriodLength = 5, bool_OnlyFullResults = TRUE, bool_AggregateToPhase = TRUE ){
  #This function requires as input a dataframe containing the following:
  # - a vector with phase shift data in the format x.x, e.g. 5.4 for shift from phase 5 to 4 (PhaseInput)
  # - a vector containing years (Year)
  # - a vector containing industries (Industry)
  # - a vector containing market capitalization (TotalMarketCap)
  # - a vector containing industry value weighted returns (IndustryVWR)
  #Also requires dplyr package:
  require(dplyr)
  
  #performs check:
  if ( nchar(df_Data[[str_PhaseInput]]) != 3 ) { 
    stop("Incorrect phase vector entered. Must be 'x.x' ; supports only single digits.") 
  }
  
  df_Results <- data.frame()
  
  for (int_RowCount in 1:nrow(df_Data) ) {
    
    int_PreviousPhase <- as.numeric(substr(df_Data[[str_PhaseInput]][int_RowCount], 1, 1))
    int_CurrentPhase <- as.numeric(substr(df_Data[[str_PhaseInput]][int_RowCount], 3, 3))
    
    if (int_PreviousPhase != int_CurrentPhase ) {
      
      int_Year <- df_Data[[str_Year]][int_RowCount]
      str_Ind <- df_Data[[str_Industry]][int_RowCount]
      
      df_Results <- df_Data %>%
        filter(!!as.name(str_Year) > (int_Year - (int_PhasePeriodLength + 1) ), 
               !!as.name(str_Year) < (int_Year + (int_PhasePeriodLength + 1) ), 
               !!as.name(str_Industry) == str_Ind) %>%
        mutate(PhaseEvent = int_CurrentPhase,
               YearEvent = int_Year,
               EventID = paste0(YearEvent, ".", str_Ind),
               PhaseSampleSize = n() ) %>%
        bind_rows(df_Results, .)
      
    }
  }
  
  if (bool_OnlyFullResults == TRUE) {
    df_Results <- df_Results %>%
      filter(PhaseSampleSize == (int_PhasePeriodLength * 2) + 1)
  }
  
  #assign sample numbers 
  #--------
  df_Results <- df_Results %>%
    group_by(EventID) %>%
    summarize() %>%
    mutate(SampleNumber = row_number()) %>%
    left_join(df_Results, ., by = "EventID")
  
  #Variation one: keep possible overlapping periods/industries, but calculate market returns and excess returns based on sample
  df_Results <- df_Results %>%
    group_by(SampleNumber) %>%
    mutate(Time = factor(paste0("t=", c(1:n()) - (int_PhasePeriodLength + 1) ),
                         levels = paste0("t=", c(1:n()) - (int_PhasePeriodLength + 1) ))) %>%
    ungroup() %>%
    group_by_at(str_Year) %>% ##### changed from Time into str_Year
    mutate(SampleMarketReturn = sum( !!as.name(str_TotalMarketCap) / sum( !!as.name(str_TotalMarketCap), na.rm = TRUE) * !!as.name(str_IndustryVWR), na.rm = TRUE),
           SampleIndustryExcessReturn = !!as.name(str_IndustryVWR) - SampleMarketReturn) %>%
    ungroup() %>%
    group_by(SampleNumber) %>%
    mutate(CumulativeSampleIndustryExcessReturn = cumprod(1 + SampleIndustryExcessReturn) - 1) %>%
    ungroup()
  
  #
  #--------
  if ( bool_AggregateToPhase == TRUE ){

    df_ResultsByPhase <- df_Results %>%
      group_by_at(c("PhaseEvent", "Time")) %>%
      mutate(Phase_RelMarketCap = !!as.name(str_TotalMarketCap) / sum( !!as.name(str_TotalMarketCap), na.rm = TRUE),
             Phase_RelCount = 1 / n() ) %>%
      summarize(Phase_ExcessVWR = sum(Phase_RelMarketCap * SampleIndustryExcessReturn, na.rm = TRUE),
                Phase_ExcessEWR = sum(Phase_RelCount * SampleIndustryExcessReturn, na.rm = TRUE),
                SampleSize = n(),
                CumulativeSE = sd(CumulativeSampleIndustryExcessReturn) / sqrt(SampleSize)) %>%
      mutate(Phase_ExcessVWRCumulative = cumprod(1 + Phase_ExcessVWR) - 1,
             Phase_ExcessEWRCumulative = cumprod(1 + Phase_ExcessEWR) - 1) %>%
      left_join(., select(distinct(df_Results, Phase, .keep_all = TRUE), Phase, PhaseName), by = c("PhaseEvent" = "Phase") ) %>%
      ungroup() %>%
      group_by(PhaseName) %>%
      mutate(PhaseNameAndSampleSize = paste0(PhaseName, " (N=", round(mean(SampleSize), 1), ")")) %>%
      ungroup()

    return(df_ResultsByPhase)
    
  } else {
    return(df_Results)
  }
}





CreateTransitionMatrix <- function(df_Data, str_PhaseInput, OutputProbabilities = TRUE, Formatted = TRUE) {
  
  str_PhaseInput <- df_Data[[str_PhaseInput]]
  
  #Count number of distinct phases... 
  int_NumberOfPhases <- length(
    unique(
      c(substr(str_PhaseInput, 1, 1), substr(str_PhaseInput, 3, 3))
    )
  )
  
  #...and create empty transition matrix with those dimensions
  mat_TransitionMatrixCount <- matrix(0, nrow = int_NumberOfPhases, ncol = int_NumberOfPhases)
  
  for ( int_Row in 1:length(str_PhaseInput) ) {
    
    int_FromPhase <- as.numeric(substr(str_PhaseInput[int_Row], 1, 1))
    int_ToPhase <- as.numeric(substr(str_PhaseInput[int_Row], 3, 3))
    mat_TransitionMatrixCount[int_FromPhase, int_ToPhase] <- as.numeric(mat_TransitionMatrixCount[int_FromPhase, int_ToPhase]) + 1
    
  }
  
  mat_TransitionMatrix <- mat_TransitionMatrixCount / colSums(mat_TransitionMatrixCount)
  mat_TransitionMatrixProb <- mat_TransitionMatrix / rowSums(mat_TransitionMatrix)    
  
  #Add row and column names if user requires formatting
  if (Formatted == TRUE) {
    mat_TransitionMatrixProb <- matrix(paste0(round(mat_TransitionMatrixProb * 100, 1), "%"), ncol = ncol(mat_TransitionMatrixProb))
    
    rownames(mat_TransitionMatrixProb) <- paste0("From ", seq(1, int_NumberOfPhases, 1))
    colnames(mat_TransitionMatrixProb) <- paste0("To ", seq(1, int_NumberOfPhases, 1))
    
    rownames(mat_TransitionMatrixCount) <- paste0("From ", seq(1, int_NumberOfPhases, 1))
    colnames(mat_TransitionMatrixCount) <- paste0("To ", seq(1, int_NumberOfPhases, 1))
  }
  
  if ( OutputProbabilities == TRUE ){
    return(mat_TransitionMatrixProb)
  } else {
    return(mat_TransitionMatrixCount)
  }
  
}



