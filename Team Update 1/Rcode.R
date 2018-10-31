
# load required libraries
library( 'dplyr' )
library( 'knitr' )
library( 'ggplot2' )
library( 'car' )


# need to change this to one of our ID's...
# ensures when we take a random 80% sample for training and 20% for testing 
# of model that we get the same results
set.seed(123)
#set.seed(99999)

# options for setting directories
setwd("D:/sbr_Development/MSc/Semester 1/MT5762/Assignments/#2")	


# load data into R
team.data <- read.table('babies23.data', header=TRUE)
#write.csv( my.data, 'quickLookie.csv' )
#View( team.data )

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
                                        race == 9 ~ 'Mixed',
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
                                          drace == 9 ~ 'Mixed',
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
                                          TRUE ~ NA_character_ ),
                      
                      newY_Groupedwt = case_when( wt <= 108.5 ~ 1,
                                                  wt <= 120  ~ 2,
                                                  wt <= 131 ~ 3,
                                                  # 98 is unknown, 99 not asked ~ same thing?
                                                  TRUE ~ 4 ),
                                  
                      # lowe birth weight references;
                      # https://www.verywellfamily.com/baby-birth-weight-statistics-2633630
                      # https://www.babycentre.co.uk/a1033196/low-birth-weight-in-babies
                      lowBirthWeight = case_when( wt <= 88 ~ 1,
                                                  TRUE ~ 0 )
                          ) %>%
                    # add m prefix to all columns associated with 'Mother' to clarify 
                    rename( 'mparity' = parity, 'mage' = age, 'mwt' = wt.1, 
                            'mht' = ht, 'mrace' = race, 'med' = ed, 
                            'msmoke' = smoke, 'mtime' = time, 'mnumber' = number ) %>%
  
                    # remove msmoke due to collinearity with mtimes & mnumber
                    # in addition we can remove other unrequired columns 
                    select( -sex, -id, -pluralty, -outcome )


# generalise all 999 unknown's to NA
unknown999 <- c( 'gestation', 'wt', 'mage', 'mwt', 'dage', 'dwt' )
team.clean.data[ unknown999 ] [ team.clean.data[ unknown999 ] == 999 ] <- NA

# generalise all 99 unknown's to NA
unknown99 <- c( 'mparity', 'mht', 'dht' )
team.clean.data[ unknown99 ] [ team.clean.data[ unknown99 ] == 99 ] <- NA

# view cleaned resulting data set
#View( team.clean.data )
#str( team.clean.data )


# *
# * BASE DATA SET FOR MODELLING 
# *
base.model.dataset <- team.clean.data %>% 
                      select( -newY_Groupedwt, -lowBirthWeight ) %>% 
                      na.omit() 



# *

#  backwards p-value method  -------------------------------------------------

p.model.data <- base.model.dataset
p.model <- lm(wt ~ ., data = p.model.data)
Anova(p.model)

#Remove the coliniar covariantes
new.p.Model.1 <- update(p.model, . ~ . -mtime)
Anova(new.p.Model.1)

new.p.Model.2 <- update(new.p.Model.1, . ~ . -msmoke)
Anova(new.p.Model.2)
# Now continue

new.p.Model.3 <- update(new.p.Model.2, . ~ . -dage)
Anova(new.p.Model.3)

new.p.Model.4 <- update(new.p.Model.3, . ~ . -mage)
Anova(new.p.Model.4)

new.p.Model.5 <- update(new.p.Model.4, . ~ . -marital)
Anova(new.p.Model.5)

new.p.Model.6 <- update(new.p.Model.5, . ~ . -inc)
Anova(new.p.Model.6)

new.p.Model.7 <- update(new.p.Model.6, . ~ . -dht)
Anova(new.p.Model.5)

new.p.Model.8 <- update(new.p.Model.7, . ~ . -med)
Anova(new.p.Model.8)

new.p.Model.9 <- update(new.p.Model.8, . ~ . -mwt)
Anova(new.p.Model.9)

new.p.Model.10 <- update(new.p.Model.9, . ~ . -mrace)
Anova(new.p.Model.8)

new.p.Model.11 <- update(new.p.Model.10, . ~ . -date)
Anova(new.p.Model.11)

new.p.Model.12 <- update(new.p.Model.11, . ~ . -ded)
Anova(new.p.Model.12)

new.p.Model.13 <- update(new.p.Model.12, . ~ . -dwt)
Anova(new.p.Model.13)

# Date is not considered to be of interest as a covariate and is therefore removed



# We now have a model following the p-value method which contains the 
# gestation, mparity, mht, drace,, ded, dwt, mtime, mnumber
p.value.model <- lm(wt ~ gestation + mparity + mht + drace  +  mnumber, data = lmModel.1)
summary(p.value.model)
plot(p.value.model)
AIC(p.value.model)

# * INITIAL MODEL SELECTION ~ AIC Stepwise 
# *
# with our base model dataset
model3.lm.AICStep <- lm( wt ~ ., data = base.model.dataset )
model3.AICStep <- step( model3.lm.AICStep )
# check model for collinearity - proves atleast one exists
vif( model3.AICStep )
# confirm which variables are collinear - mtime and mnumber
alias( model3.AICStep )

# however, from our data exploration we know something is up with msmoke & mtime
# so lets apply the same approach as above, but force the step() function to disregard 
# mtime - easiest way to do that is to remove mtime from the data set provided
# try another model removing mtime from dataset
base.model.dataset.without.mtime <- base.model.dataset %>% 
                                    select( -mtime )
model3.lm.AICStep.without.mtime <- lm( wt ~ ., data = base.model.dataset.without.mtime )
# calculate new best model, and what do we have here... AIC is now 3294.08 - lower than our previous best!...
model3.AICStep.without.mtime <- step( model3.lm.AICStep.without.mtime )

# so lets confirm that we achieve the same AIC result by selecting the specific predictive variables
# test model without time on data set including mtime
model3.lm.comparisonStuff <- lm( wt ~ mnumber + dwt + mparity + msmoke + drace + mht + gestation, data = base.model.dataset ) 
# we do, so our actual best model is this one - not the above
model3.comparisonStuff <- step( model3.lm.comparisonStuff )

# now check for collinearity - proves atleast one exists
vif( model3.lm.comparisonStuff )
# confirm which variables are collinear - mnumber and msmoke
alias( model3.lm.comparisonStuff )


# therefor our final best model for AIC stepwise approach is below
model3.lm.final <- lm( wt ~ mnumber + dwt + mparity + drace + mht + gestation, data = base.model.dataset ) 
model3.final <- step( model3.lm.final )

# returnining an AIC value of 5003.591
AIC( model3.final )



# FARZAM - to put in assumtions code and bootstrap
# check model for collinearity - proves atleast one exists
vif( model3.AICStep )
# confirm which variables are collinear - mtime and mnumber
alias( model3.AICStep )

# however, from our data exploration we know something is up with msmoke & mtime
# so lets apply the same approach as above, but force the step() function to disregard 
# mtime - easiest way to do that is to remove mtime from the data set provided
# try another model removing mtime from dataset
base.model.dataset.without.mtime <- base.model.dataset %>% 
                                    select( -mtime )
model3.lm.AICStep.without.mtime <- lm( wt ~ ., data = base.model.dataset.without.mtime )
# calculate new best model, and what do we have here... AIC is now 3294.08 - lower than our previous best!...
model3.AICStep.without.mtime <- step( model3.lm.AICStep.without.mtime )

# so lets confirm that we achieve the same AIC result by selecting the specific predictive variables
# test model without time on data set including mtime
model3.lm.comparisonStuff <- lm( wt ~ mnumber + dwt + mparity + msmoke + drace + mht + gestation, data = base.model.dataset ) 
# we do, so our actual best model is this one - not the above
model3.comparisonStuff <- step( model3.lm.comparisonStuff )

# now check for collinearity - proves atleast one exists
vif( model3.lm.comparisonStuff )
# confirm which variables are collinear - mnumber and msmoke
alias( model3.lm.comparisonStuff )

# There are aliased coefs ranning into perfect collinearity 
# Indentifying which linearly dependent variables are culprits
ld.variables <- attributes(alias(model3.lm.comparisonStuff)$Complete)$dimnames[[1]]

# therefor our final best model for AIC stepwise approach is below
model3.lm.final <- lm( wt ~ mnumber + dwt + mparity + drace + mht + gestation, data = base.model.dataset ) 
model3.final <- step( model3.lm.final )

# Returnining an AIC value of 5003.591
AIC( model3.final )
# Evaluate Collinearity using variance inflation factors 
vif(model3.lm.final)
alias(model3.lm.final)

# Model diagnostics: collinearity
# Examining this using Variance Inﬂation Factors (VIFs)
# Measurinng the instability of parameter estimates due to dependencies between covariates 
# They are non-negative and a rule of thumb is that VIFs >10 is problematic 
vif(model3.lm.final) > 10

# Model diagnostics - error distribution
# Once all the signal is eliminated, only iid Normal distributed remain. Shape • 
# Checking shape of errors by the distribution of residuals (yi −ˆ yi) • 
# Using QQ-Norm plots and tests of Normality (Shapiro-Wilks, H0 the data (errors) are Normally distributed)
# Model diagnostics - error shape
qqnorm(resid(model3.lm.final)) 
qqline(resid(model3.lm.final))
shapiro.test(resid(model3.lm.final))

# Trying to track down the extreme residuals 
hist(resid(model3.lm.final))
High.Resid <- which(abs(resid(model3.lm.final))>5) 
base.model.dataset[High.Resid,]

# Evaluate Nonlinearity
# component + residual plot 
crPlots(model3.lm.final)
# Showing Ceres plots 
ceresPlots(model3.lm.final)

# Assessing linearity 
library(Epi)
par(mfrow=c(3,2))
termplot(model3.lm.final, se=T)
termplot(model3.lm.final, se=T, partial.resid = T)

# Spread: 
# Checking variance of residuals (variance of the error distribution) is constant WRT ˆ y and the x 
# Testing with Breusch-Pagan (H0 the errors are homoscedastic/have constant variance)
ncvTest(model3.lm.final)
# Plots residuals against ˆ y and the x
Model.Resid <- resid(model3.lm.final)
par(mfrow=c(3,2))
plot(base.model.dataset$wt, Model.Resid, ylab = 'residuals', xlab = 'Mother Weight')
plot(base.model.dataset$mage, Model.Resid, ylab = 'residuals', xlab = 'Mother Age')
plot(fitted(model3.lm.final), Model.Resid, ylab = 'residuals', xlab = 'Fitted values')

# plotting studentized residuals vs. fitted values 
spreadLevelPlot(model3.lm.final)

# One can be explicit about which covariate to test against: 
ncvTest(model3.lm.final, wt ~ .)

# Independence: 
# Checking serial correlation of residuals, when ordered in some logical way e.g. order of collection, ˆ y, or an x 
# Plots residuals against ˆ y and the x 
# Testing with Durbin-Watson (H0 the errors are uncorrelated)
durbinWatsonTest(model3.lm.final)
Graph_1 <- plot(model3.lm.final, Model.Resid, ylab = 'residuals', xlab = 'Baby Weight')
Graph_2 <- plot(model3.lm.final, which = 1:2)

cut.fit <- cut(fitted(model3.lm.final), breaks = quantile(fitted(model3.lm.final),
                                                        probs = c(seq(0,1,length=20))))

table(cut.fit)

# plotting paris covariates
library(GGally) 
numericonly <- base.model.dataset %>% select_if(is.numeric)
ggpairs(numericonly)
# Running summary
summary(model3.lm.final)
Anova(model3.lm.final) 
confint(model3.lm.final)
plot(confint(model3.lm.final))
# demonstrating the confidence intervals graphically 
# demonstrating the confidence intervals graphically 
library(effects)
effect.model <- base.model.dataset %>% 
  mutate(gestation= factor(gestation), mparity = factor(mparity), 
         drace= factor(drace), mnumber= factor(mnumber), mht= factor(mht),
         dwt = factor(dwt)) 

altered.model <- update(model3.lm.final, wt ~. , data = effect.model)
plot(effect(term = "drace", mod = altered.model))
plot(effect(term = "mnumber", mod = altered.model))
plot(effect(term = "mparity", mod = altered.model))
plot(effect(term = "dwt", mod = altered.model))
plot(effect(term = "gestation", mod = altered.model))
plot(effect(term = "mht", mod = altered.model))
