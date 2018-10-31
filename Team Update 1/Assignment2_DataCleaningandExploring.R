
# load required libraries
library( 'dplyr' )
library( 'knitr' )
library( 'ggplot2' )


# options for setting directories
setwd("D:/sbr_Development/MSc/Semester 1/MT5762/Assignments/#2")	


# load data into R
team.data <- read.table('babies23.data', header=TRUE)
#write.csv( my.data, 'quickLookie.csv' )


# add naming conventions to columns to replace numeric counterparts
# here we'll also take the opportunity to generalise all unknowns to NA...
# we can later show those as 'Unknown' if we wish - at this stage however just
# keeps things a little cleaner
team.clean.data <-  team.data %>%
                    mutate( 
                      sex = case_when( sex == 1 ~ 'Male',
                                       sex == 2 ~ 'Female',
                                       TRUE ~ NA_character_ ),
                      
                      race = case_when( race %in% 0:5 ~ 'White',
                                        race == 6 ~ 'Mex', 
                                        race == 7 ~ 'Black',
                                        race == 8 ~ 'Asian',
                                        TRUE ~ NA_character_ ),

                      ed = case_when( ed == 0 ~ 'Less than 8th Grade',
                                      ed == 1 ~ '8th - 12th Grade - did not graduate',
                                      ed == 2 ~ 'HS graduate - no other schooling',
                                      ed == 3 ~ 'HS + trade',
                                      ed == 4 ~ 'HS + some College',
                                      ed == 5 ~ 'College graduate',
                                      ed %in% 6:7  ~ 'Trade school HS unclear',
                                      TRUE ~ NA_character_ ),
                      
                      drace = case_when(  drace %in% 0:5 ~ 'White',
                                          drace == 6 ~ 'Mex', 
                                          drace == 7 ~ 'Black',
                                          drace == 8 ~ 'Asian',
                                          TRUE ~ NA_character_ ),
                
                      ded = case_when(  ded == 0 ~ 'Less than 8th Grade',
                                        ded == 1 ~ '8th - 12th Grade - did not graduate',
                                        ded == 2 ~ 'HS graduate - no other schooling',
                                        ded == 3 ~ 'HS + trade',
                                        ded == 4 ~ 'HS + some College',
                                        ded == 5 ~ 'College graduate',
                                        ded %in% 6:7  ~ 'Trade school HS unclear',
                                        TRUE ~ NA_character_ ),
                      
                      marital = case_when(  marital == 1 ~ 'Married',
                                            marital == 2 ~ 'Legally Separated', 
                                            marital == 3 ~ 'Divorced',
                                            marital == 4 ~ 'Widowed',
                                            marital == 5 ~ 'Never Married',
                                          TRUE ~ NA_character_ ),
                      
                      inc = case_when(  inc == 1 ~ 'Under 2500',
                                        inc == 2 ~ '2500 - 4999', 
                                        inc == 3 ~ '5000 - 7499',
                                        inc == 4 ~ '7500 - 9999',
                                        inc == 5 ~ '10000 - 12499', 
                                        inc == 6 ~ '12500 - 14999',
                                        inc == 7 ~ '15000 - 17499',
                                        inc == 8 ~ '17500 - 19999', 
                                        inc == 9 ~ '20000 and over',
                                        # 98 is unknown, 99 not asked ~ same thing?
                                        TRUE ~ NA_character_ ),
                      
                      smoke = case_when(  smoke == 0 ~ 'Never',
                                          smoke == 1 ~ 'Smokes now', 
                                          smoke == 2 ~ 'Until current pregnancy',
                                          smoke == 3 ~ 'Once did, not now',
                                          TRUE ~ NA_character_ ),
                      
                      time = case_when( time == 0 ~ 'Never smoked',
                                        time == 1 ~ 'Still smokes',
                                        time == 2 ~ 'During current pregnancy', 
                                        time == 3 ~ 'Within 1 year',
                                        time == 4 ~ '1 to 2 years ago',
                                        time == 5 ~ '2 to 3 years ago', 
                                        time == 6 ~ '3 to 4 years ago',
                                        time == 7 ~ '5 to 9 years ago',
                                        time == 8 ~ '10+ years ago', 
                                        time == 9 ~ 'Quit and dont know',
                                        # 98 is unknown, 99 not asked ~ same thing?
                                        TRUE ~ NA_character_ ),
                      
                      number = case_when( number == 0 ~ 'Never',
                                          number == 1 ~ '1-4',
                                          number == 2 ~ '5-9', 
                                          number == 3 ~ '10-14',
                                          number == 4 ~ '15-19',
                                          number == 5 ~ '20-29', 
                                          number == 6 ~ '30-39',
                                          number == 7 ~ '40-60',
                                          number == 8 ~ '60+', 
                                          number == 9 ~ 'Smoke but dont know',
                                          # 98 is unknown, 99 not asked ~ same thing?
                                          TRUE ~ NA_character_ ) 
                          ) %>%
                    # add m prefix to all columns associated with 'Mother' to clarify 
                    rename( 'mparity' = parity, 'mage' = age, 'mwt' = wt.1, 
                            'mht' = ht, 'mrace' = race, 'med' = ed, 
                            'msmoke' = smoke, 'mtime' = time, 'mnumber' = number )

# generalise all 999 unknown's to NA
unknown999 <- c( 'gestation', 'wt', 'mage', 'mwt', 'dage', 'dwt' )
team.clean.data[ unknown999 ] [ team.clean.data[ unknown999 ] == 999 ] <- NA

# generalise all 99 unknown's to NA
unknown99 <- c( 'mparity', 'mht', 'dht' )
team.clean.data[ unknown99 ] [ team.clean.data[ unknown99 ] == 99 ] <- NA


# view cleaned resulting data set
#View( team.clean.data )

# check structure of new team.clean.data
#str( team.clean.data )

# before we jump to running statisical model function, let's isolate the character (i.e. useful) columns
characterCols <- names( team.clean.data[sapply( team.clean.data, class ) == 'character'] )
# finally we need to add our y variable ~ babies weight ( wt ) 
modelCols <- c( 'wt', characterCols )

# quick look at our refined data subset if we want
data.statsmodel <- team.clean.data[ modelCols ]
View( team.clean.data[ modelCols ] )


# custom function, applying statistical methodology outlined in lecture slides
# variable names could be cleaned up a little.. 
statisticalMethodology <- function ( data, base.COlumnIndex )
{
  # global variables
  base.X <- 1
  pvalue.Boundary <- 0.05
  pvalue.Search <- 'Pr(>F)1'

  # grab focus x variable name  
  columnElement <- colnames( data )[[ base.COlumnIndex ]]
  
  # generate focus data set
  x <-  data %>% 
        mutate( varY = .[[ base.COlumnIndex ]], varX = .[[ base.X ]]) %>%
        select( varY, varX ) %>%
        mutate( varY = as.factor( varY ) ) %>%
        na.omit()
  
  # Initialise output variables to default values
  test.ND <- NA
  test.CD <- NA
  test.ANOVANullHyp <- 0
  test.KruskalNullHyp <- 0
  test.NComparisons <- NA
  test.SignificantDifferences <- NA
  
  # Constant Spread ; Test 
  # Is Standard Deviation variance less than 2 times the smallest Standard Deviation
  # - if so, TRUE, else FALSE
  base.StandardDeviations <-  x %>% 
    mutate( baseSD = varX,  
            groupBy = varY )  %>%
    group_by( groupBy ) %>% 
    summarise( tempSD = sd( baseSD ), 
               count = n(),
               mean = mean( baseSD ),
               SD = ifelse( count == 1, mean, tempSD ) )
  
  test.minSD <- min( abs( base.StandardDeviations$SD ) )
  test.min2SD <- 2 * test.minSD
  
  test.result <-  base.StandardDeviations %>% 
    mutate( min2SD = test.min2SD, 
            variance = ifelse( count == 1, 0, min2SD - abs( SD ) ), 
            marker = ifelse( variance < 0, 1, 0 ) ) %>% 
    summarise( nSamples = n(), marker = sum( marker ) )
  
  test.CD <- ifelse( test.result$marker > 0, FALSE, TRUE )
  
  # Calculate ANOVA results
  base.ANOVA <- aov( varX ~ varY, data = x )
  
  # Normality ; Test
  # Using Shapiro-Wilk Normality Test
  # when p-value is greater than 0.05 it can be assumed data follows 
  # a normal distribution
  test.shaprio <- shapiro.test( base.ANOVA$residuals )
  #test.shaprio <- shapiro.test( x[[base.COlumnIndex]] ) # not clear on input...
  
  test.ND <- ifelse( test.shaprio$p.value >= pvalue.Boundary, TRUE, FALSE )
  
  # before we continue with ANOVA test, we need to ensure assumptions of
  # Normality & Constant Spread are passed
  if ( test.CD && test.ND )
  {
    # Extract p value from ANOVA result
    # Can we reject the Null hypothesis ( i.e. means are the same )
    sum_test <- unlist( summary( base.ANOVA ) )
    
    test.ANOVANullHyp <- ifelse( sum_test[ pvalue.Search ] >= pvalue.Boundary, 0, 1 )
    
    if ( test.ANOVANullHyp )
    {
      # if ANOVA Null hypothesis rejected, for post-hoc comparisons we need to
      # adjust for multiple comparisons using Tukey's test
      base.Tukey <- TukeyHSD( base.ANOVA )
      
      base.posthoc <- as.data.frame( base.Tukey$`x$varY` )
      
      base.Tukey <- base.posthoc %>%
                    mutate( marker = ifelse( `p adj` < pvalue.Boundary, 1, 0 ) ) %>%
                    summarise( nSamples = n(), marker = sum( marker ) )
      
      test.NComparisons <- base.Tukey$nSamples
      test.SignificantDifferences <- base.Tukey$marker
    }
  }
  else
  {
    # assumptions for ANOVA ~ Normality & Constant Spread failed
    # we now have to utilise non-parametric test
    
    # conduct a Kruskal-Wallis Anova test
    base.Kruskal <- kruskal.test( varX ~ varY, data = x )
    
    # Extract p value from Kruskal-Wallis ANOVA result
    # Can we reject the Null hypothesis ( i.e. means are the same )
    test.KruskalNullHyp <- ifelse( base.Kruskal$p.value >= pvalue.Boundary, 0, 1 )
    
    if ( test.KruskalNullHyp )
    {
      # if Kruskal Null hypothesis rejcted, for post-hoc comparisons we need to
      # adjust for multiple comparisons using Wilcox's test
      base.Wilcox <- pairwise.wilcox.test( x$varX, x$varY, 
                                           paired = FALSE, 
                                           p.adjust.method = "holm" )
      
      
      base.WilcoxResults <- as.data.frame( as.table( base.Wilcox$p.value ) )
      
      base.WilcoxResults <- na.omit( base.WilcoxResults )
      
      base.WilcoxResults <- base.WilcoxResults %>%
        mutate( marker = ifelse( Freq < pvalue.Boundary, 1, 0 ) ) %>%
        summarise( nSaples = n(), marker = sum( marker ) )
      
      test.NComparisons <- base.WilcoxResults$nSaples
      test.SignificantDifferences <- base.WilcoxResults$marker
    }
  }
  
  # generate custom output table with useful columns we are interested in
  return.table <- cbind.data.frame( Element = columnElement,
                                    Normality = test.ND, 
                                    ConstantSpread = test.CD, 
                                    rejectANOVANullHyp = test.ANOVANullHyp,
                                    rejectKruskalNullHyp = test.KruskalNullHyp,
                                    nComparisons = test.NComparisons,
                                    nSignificantDifferences = test.SignificantDifferences, 
                                    row.names = NULL
  )
  # return table to user
  return( return.table )
}


#length( data.statsmodel )
# Complete statistical model for all chemical elements 
# miss out column index 2 (as only male babies in this study - need atleast 2 to perform tests!)
temp.Results <- list()
for ( i in 3:11 )
{
  sm <- statisticalMethodology( data = data.statsmodel, base.COlumnIndex = i )
  temp.Results[[i]] <- sm
}
table.Results <- rbind_list( temp.Results )
#View( table.Results )

# clean up the table to make it easier for end user to interpret
userfriendly.Results <-  table.Results %>%
  mutate( statsTestApplied = ifelse( rejectANOVANullHyp == 1, 'ANOVA', 'Kruskal-Wallis' ),
          rejectANOVANullHyp = ifelse( rejectANOVANullHyp == 1, TRUE, FALSE ),
          rejectKruskalNullHyp = ifelse( rejectKruskalNullHyp == 1, TRUE, FALSE ),
          rejectNullHyp = ifelse( statsTestApplied == 'ANOVA', rejectANOVANullHyp, rejectKruskalNullHyp ) ) %>%
  rename( 'Normality Assumption Passed' = Normality, 
          'Constant Spread Assumption Passed' = ConstantSpread,
          'Statistical Test Used' = statsTestApplied,
          'Reject Null Hypothesis' = rejectNullHyp,
          'Total Possible Soil Type comparisons' = nComparisons,
          'Significant Differences observed between comparisons' = nSignificantDifferences ) %>%
  select( 'Element', 'Normality Assumption Passed', 'Constant Spread Assumption Passed',
          'Statistical Test Used', 'Reject Null Hypothesis', 
          'Total Possible Soil Type comparisons', 
          'Significant Differences observed between comparisons' )
#kable( userfriendly.Results )
View( userfriendly.Results )

# armed with these results, lets plot some
# Create a simple box-plot to more clearly visualise results from table above
# some differences
somediffs.BoxPlot <-  ggplot( data = data.statsmodel,
                      aes( x = mrace , y = wt )) +
                      geom_boxplot() +
                      ggtitle("Mother's Race has some redictive value on baby weight") +
                      xlab( "Mother's Race") +
                      ylab( "Baby Weight (ounces)") +
                      theme_light()
somediffs.BoxPlot

# no real differences
nodiff.BoxPlot <- ggplot( data = data.statsmodel,
                  aes( x = inc , y = wt )) +
                  geom_boxplot() +
                  ggtitle("Family income has no impact on baby weight") +
                  xlab( "Family Income Range") +
                  ylab( "Baby Weight (ounces)") +
                  theme_light()
nodiff.BoxPlot