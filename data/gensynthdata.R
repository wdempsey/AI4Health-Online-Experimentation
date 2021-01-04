setwd("./GitHub/AI4Health-Online-Experimentation/data/")
load("analysis-data.RData")
names(analysis.data)

## Construct action-centered variable
analysis.data$ac = analysis.data$action - analysis.data$probability 

## Fixes to day and action centered versions
analysis.data$day = as.numeric(analysis.data$day)
analysis.data$acday = analysis.data$ac * analysis.data$day
analysis.data$acdosage = analysis.data$ac * analysis.data$dosage
analysis.data$acengagement = analysis.data$ac * analysis.data$engagement
analysis.data$aclocation = analysis.data$ac * analysis.data$other.location
analysis.data$acvariation = analysis.data$ac * analysis.data$variation
analysis.data$actemp = analysis.data$ac * analysis.data$temperature
analysis.data$aclogpresteps = analysis.data$ac * analysis.data$logpresteps
analysis.data$acsqrttotsteps = analysis.data$ac * analysis.data$sqrt.totalsteps
analysis.data$acprioranti = analysis.data$ac * analysis.data$prior.anti

## Need gamm4 for generalized additive mixed effects model
if(!require("gamm4")){install.packages("gamm4")}
library("gamm4")

## Model for reward given state, action.
## Modeling available and unavailable data separately
list_ids = unique(analysis.data$id)
current_id = list_ids[3]
availablereward.model <- gamm(reward ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti + ## Baseline Model
                                s(acday) + s(acdosage) + s(acengagement, by = id) + s(aclocation) + s(acvariation) + ## Treatment model
                                s(aclogpresteps, by = id) + s(acsqrttotsteps) + s(acprioranti),
                              # random = ~(acengagement | id) + (actemp | id) + (aclogpresteps | id),
                              data = subset(analysis.data, availability == 1))

summary(availablereward.model$gam)

saveRDS(object = availablereward.model, file = "availablereward.RDS")

unavailablereward.model <- gamm(reward ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                variation + s(temperature) + s(logpresteps, by = id) + s(sqrt.totalsteps) + prior.anti,
                              # random = ~(engagement | id) + (temperature | id) + (logpresteps | id),
                              data = subset(analysis.data, availability == 0))

summary(unavailablereward.model$gam)

saveRDS(object = unavailablereward.model, file = "unavailablereward.RDS")

## Availability model
'
Make a population-level availability model
that only depends on current state. 
'
availability.model <- gamm(availability ~ s(day, by = id) + s(dosage) + engagement + other.location + ## Baseline Model
                                  variation + s(temperature) + s(logpresteps, by = id) + s(sqrt.totalsteps) + prior.anti,
                                data = analysis.data, family = binomial)

summary(availability.model$gam)

saveRDS(object = availability.model, file = "availability.RDS")

## Transition model
'
Day, Dosage are deterministic.
Temperature: exogenous so use the users exact temperature.
Location: (assume it is exogenous to the intervention so use the users exact sequence)
Build rest via a sequence of conditional distributions: 
'
# Engagement indicator
engagement.model <- gamm(engagement ~ s(day) + s(dosage) + other.location + s(temperature),
                         data = analysis.data, family = binomial)

saveRDS(object = availability.model, file = "availability.RDS")

# Variation indicator
variation.model <- gamm(variation ~ s(day) + s(dosage) + other.location + 
                          s(temperature) + engagement,
                         data = analysis.data, family = binomial)

saveRDS(object = availability.model, file = "availability.RDS")