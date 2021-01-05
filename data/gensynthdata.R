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

## Generate subsets of size 5 and fit models 1-40
## Save models as 1-40. We will treat these as cohorts.
set.seed("1298371")

cohort = 1
subset_ids = sample(unique(analysis.data$id), size = 5)
availablereward.model <- gamm(reward ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti + ## Baseline Model
                                s(acday) + s(acdosage) + s(actemp) + s(acengagement) + s(acvariation) + ## Treatment model
                                s(aclogpresteps) + s(acsqrttotsteps) + acprioranti,
                              # random = ~(acengagement | id) + (actemp | id) + (aclogpresteps | id),
                              data = subset(analysis.data, availability == 1 & is.element(id, subset_ids)))

summary(availablereward.model$gam)

saveRDS(object = availablereward.model, file = paste("availablereward_cohort_",cohort,".RDS", sep = ""))

unavailablereward.model <- gamm(reward ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti,
                              # random = ~(engagement | id) + (temperature | id) + (logpresteps | id),
                              data = subset(analysis.data, availability == 0 & is.element(id, subset_ids)))

summary(unavailablereward.model$gam)

saveRDS(object = unavailablereward.model, file = paste("unavailablereward_cohort_",cohort,".RDS", sep = ""))

## Availability model
availability.model <- gamm(availability ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                  variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti,
                                data = subset(analysis.data, is.element(id, subset_ids)), family = binomial)

summary(availability.model$gam)

saveRDS(object = availability.model, file = paste("availability_cohort_",cohort,".RDS", sep = ""))

## Transition model
'
Day, Dosage are deterministic.
Temperature: exogenous so use the users exact temperature.
Location: (assume it is exogenous to the intervention so use the users exact sequence)
Build rest via a sequence of conditional distributions: 
'
# Engagement indicator
engagement.model <- gamm(engagement ~ s(day) + s(dosage) + other.location + s(temperature),
                         data = subset(analysis.data, is.element(id, subset_ids)), family = binomial)

saveRDS(object = engagement.model, file = paste("engagement_cohort_",cohort,".RDS", sep = ""))

# Variation indicator
variation.model <- gamm(variation ~ s(day) + s(dosage) + other.location + 
                          s(temperature) + engagement,
                         data = subset(analysis.data, is.element(id, subset_ids)), family = binomial)

saveRDS(object = variation.model, file = paste("variation_cohort_",cohort,".RDS", sep = ""))

## Log Pre Steps
logpresteps.model <- gamm(logpresteps ~ s(day) + s(dosage) + other.location + 
                          s(temperature) + engagement + variation,
                        data = subset(analysis.data, is.element(id, subset_ids)))

saveRDS(object = logpresteps.model, file = paste("logpresteps_cohort_",cohort,".RDS", sep = ""))

## Sqrt total Steps
sqrttotalsteps.model <- gamm(sqrt.totalsteps ~ s(day) + s(dosage) + other.location + 
                            s(temperature) + engagement + variation + s(logpresteps),
                          data = subset(analysis.data, is.element(id, subset_ids)))

saveRDS(object = sqrttotalsteps.model, file = paste("sqrttotalsteps_cohort_",cohort,".RDS", sep = ""))

## Prior Antisedentary
prioranti.model <- gamm(prior.anti ~ s(day) + s(dosage) + other.location + 
                               s(temperature) + engagement + variation + s(logpresteps) + s(sqrt.totalsteps),
                             data = subset(analysis.data, is.element(id, subset_ids)), family = binomial)

saveRDS(object = prioranti.model, file = paste("prioranti_cohort_",cohort,".RDS", sep = ""))


# Generative models
# Inputs: Current State, Action, and Prob
input = list("state" = output$state, "available" = 1, "action" = 1, "probability" = 0.5)

pred.data = data.frame("id" = input$state[1])
pred.data$day = input$state[2]
pred.data$decision.time = input$state[3]
pred.data$dosage = input$state[4]
pred.data$engagement = input$state[5]
pred.data$other.location = input$state[6]
pred.data$variation = input$state[7]
pred.data$temperature= input$state[8]
pred.data$logpresteps = input$state[9]
pred.data$sqrt.totalsteps = input$state[10]
pred.data$prior.anti = input$state[11]
pred.data$action = input$action
pred.data$probability = input$probability
pred.data$ac = input$action - input$probability
pred.data$acday = pred.data$ac * pred.data$day
pred.data$acdosage = pred.data$ac * pred.data$dosage
pred.data$acengagement = pred.data$ac * pred.data$engagement
pred.data$aclocation = pred.data$ac * pred.data$other.location
pred.data$acvariation = pred.data$ac * pred.data$variation
pred.data$actemp = pred.data$ac * pred.data$temperature
pred.data$aclogpresteps = pred.data$ac * pred.data$logpresteps
pred.data$acsqrttotsteps = pred.data$ac * pred.data$sqrt.totalsteps
pred.data$acprioranti = pred.data$ac * pred.data$prior.anti

cohort = pred.data$id %% 5

available_reward_model = readRDS(file = paste("availablereward_cohort_",cohort,".RDS", sep = ""))
unavailable_reward_model = readRDS(file = paste("unavailablereward_cohort_",cohort,".RDS", sep = ""))
expit <- function(x) {1/(1+exp(-x))}

if(input$available == 1) {
 mean_reward = predict(available_reward_model$gam, current_state) 
 sig = sigma(available_reward_model$lme)
 reward = mean_reward + rnorm(1, mean = 0, sd = sig)
} else {
  mean_reward = predict(unavailable_reward_model$gam, current_state) 
  sig = sigma(unavailable_reward_model$lme)
  reward = mean_reward + rnorm(1, mean = 0, sd = sig)
}



if(decision.time > 5) {
  print("Decision time must be integer <= 5.")
} else {
  next.decision.time = (decision.time + 1)%% 5
  if (next.decision.time == 1) {
    next.day = day + 1 
  } else {
    next.day = day
  }
}

current.row = which(analysis.data$id == id & analysis.data$day == next.day & analysis.data$decision.time == next.decision.time)

if(identical(current.row, integer(0))) {
  return(NA)
} else {
  next.temp = analysis.data$temperature[current.row]
  next.dosage = current.dosage * 0.95 + 1*(action == 1)
  next.location = analysis.data$other.location[current.row]
}

engagement.model = readRDS(file = paste("engagement_cohort_",cohort,".RDS", sep = ""))
pred.data = analysis.data[current.row,]
pred.data$dosage = next.dosage
pred.data$probability = prob
pred.data$action = 1

logit_engagement = predict(engagement.model$gam, pred.data)
pred.data$engagement = rbinom(n=1, size = 1, prob = expit(logit_engagement))

variation.model = readRDS(file = paste("variation_cohort_",cohort,".RDS", sep = ""))
logit_variation = predict(variation.model$gam, pred.data)
pred.data$variation = rbinom(n=1, size = 1, prob = expit(logit_engagement))

logpresteps.model = readRDS(file = paste("logpresteps_cohort_",cohort,".RDS", sep = ""))
pred.data$logpresteps = predict(variation.model$gam, pred.data) + rnorm(1, mean = 0, sd = sigma(variation.model$lme))

logpresteps.model = readRDS(file = paste("sqrttotalsteps_cohort_",cohort,".RDS", sep = ""))
pred.data$logpresteps = predict(logpresteps.model$gam, pred.data) + rnorm(1, mean = 0, sd = sigma(logpresteps.model$lme))

sqrttotalsteps.model = readRDS(file = paste("logpresteps_cohort_",cohort,".RDS", sep = ""))
if(next.decision.time == 1) {
  pred.data$sqrt.totalsteps = predict(sqrttotalsteps.model$gam, pred.data) + rnorm(1, mean = 0, sd = sigma(sqrttotalsteps.model$lme))
} else{
  pred.data$sqrt.totalsteps = sqrt.totalsteps
}

prioranti.model = readRDS(file = paste("prioranti_cohort_",cohort,".RDS", sep = ""))
logit_prioranti = predict(prioranti.model$gam, pred.data)
pred.data$prior.anti = rbinom(n=1, size = 1, prob = expit(logit_prioranti))

availability_model = readRDS(file = paste("availability_cohort_",cohort,".RDS", sep = ""))
logit_avail = predict(availability_model$gam, current_state)
user_avail = rbinom(n = 1, size = 1, prob = expit(logit_avail))

output = list("reward" = reward,
              "state" = c(pred.data$id, pred.data$day, pred.data$decision.time, pred.data$dosage, 
                          pred.data$engagement, pred.data$other.location, pred.data$variation, 
                          pred.data$temperature, pred.data$logpresteps, pred.data$sqrt.totalsteps,
                          pred.data$prior.anti),
              "availability" = user_avail)
