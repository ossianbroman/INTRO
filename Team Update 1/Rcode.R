
# AIC section -------------------------------------------------------------

# SAM - to put in his funciton and data cleaning


# AIC step
library(car)

lmModel.1 <- team.clean.data %>% na.omit() %>% select(-sex, -id, -outcome, -pluralty, -date)
fullModel.1 <- lm(wt ~ ., data = lmModel.1)
step(fullModel.1)
#AIC of 3299.5 is the mbst model according to step() with mnumber, dwt, mtime, mparity, drace, mht, and gestation 
test.2 <- lm(wt ~ mnumber + dwt  + as.factor(mparity) + drace + mht + gestation , data = lmModel.1)
test.3 <- lm(wt ~ mnumber + dwt  + mparity + drace + mht + gestation , data = lmModel.1)

summary(test.2)
plot(test.2)
AIC(test.2, test.3, p.value.model)


wt <- team.clean.data$wt
summary(wt)

#  backwards p-value method  -------------------------------------------------

p.model.data <- team.clean.data %>% na.omit() %>% select(-sex, -id, -outcome, -pluralty, -date)
p.model <- lm(wt ~ ., data = p.model.data)
Anova(p.model)

new.p.Model.1 <- update(p.model, . ~ . -dage)
Anova(new.p.Model.1)

new.p.Model.2 <- update(new.p.Model.1, . ~ . -mage)
Anova(new.p.Model.2)

new.p.Model.3 <- update(new.p.Model.2, . ~ . -marital)
Anova(new.p.Model.3)

new.p.Model.4 <- update(new.p.Model.3, . ~ . -inc)
Anova(new.p.Model.4)

new.p.Model.5 <- update(new.p.Model.4, . ~ . -dht)
Anova(new.p.Model.5)

new.p.Model.6 <- update(new.p.Model.5, . ~ . -med)
Anova(new.p.Model.6)

new.p.Model.7 <- update(new.p.Model.6, . ~ . -mwt)
Anova(new.p.Model.7)

new.p.Model.8 <- update(new.p.Model.7, . ~ . -mrace)
Anova(new.p.Model.8)

new.p.Model.9 <- update(new.p.Model.8, . ~ . -ded)
Anova(new.p.Model.9)

#Remove the coliniar covariantes
new.p.Model.10 <- update(new.p.Model.9, . ~ . -mtime)
Anova(new.p.Model.10)

new.p.Model.11 <- update(new.p.Model.10, . ~ . -msmoke)
Anova(new.p.Model.11)
# We now have a model following the p-value method which contains the 
    # gestation, mparity, mht, drace,, ded, dwt, mtime, mnumber
p.value.model <- lm(wt ~ gestation + mparity + mht + drace  + dwt + mnumber, data = lmModel.1)
summary(p.value.model)
plot(p.value.model)
AIC(p.value.model)

# FARZAM - to put in assumtions code and bootstrap
