## Load data
load("C:/Users/Balthazar/Box/HeartSteps/Walter/data/analysis-data-2.RData")
setwd("./RDS_files/")

## Need gamm4 for generalized additive mixed effects model
if(!require("gamm4")){install.packages("gamm4")}
library("gamm4")

## Model for reward given state, action.
## Modeling available and unavailable data separately
list_ids = unique(analysis.data$id)

## Generate subsets of size 5 and fit models 1-40
## Save models as 1-40. We will treat these as cohorts.
set.seed("1298371")
group_size = 5

for (cohort in 1:22) { 
  print(cohort)
  subset_ids = (cohort-1)*group_size + 1:5
  #sample(unique(analysis.data$id), size = 5)
  zeroavailablereward.model <- gamm(zeroreward ~ s(day) + dosage + engagement + other.location + ## Baseline Model
                                  variation + temperature + s(logpresteps) + sqrt.totalsteps + prior.anti + ## Baseline Model
                                  s(acday) + acdosage + actemp + acengagement + acvariation + ## Treatment model
                                  s(aclogpresteps) + acsqrttotsteps + acprioranti,
                                # random = ~(acengagement | id) + (actemp | id) + (aclogpresteps | id),
                                family = binomial,
                                data = subset(analysis.data, availability == 1 & is.element(id, subset_ids)))
  
  saveRDS(object = zeroavailablereward.model, file = paste("zeroavailablereward_cohort_",cohort,".RDS", sep = ""))
  
  availablereward.model <- gamm(reward ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                  variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti + ## Baseline Model
                                  s(acday) + s(acdosage) + s(actemp) + s(acengagement) + s(acvariation) + ## Treatment model
                                  s(aclogpresteps) + s(acsqrttotsteps) + acprioranti,
                                # random = ~(acengagement | id) + (actemp | id) + (aclogpresteps | id),
                                data = subset(analysis.data, availability == 1 & zeroreward == 0 & is.element(id, subset_ids)))
  
  saveRDS(object = availablereward.model, file = paste("availablereward_cohort_",cohort,".RDS", sep = ""))
  
  zerounavailablereward.model <- gamm(zeroreward ~ s(day) + dosage + engagement + other.location + ## Baseline Model
                                      variation + temperature + s(logpresteps) + sqrt.totalsteps + prior.anti, ## Baseline Model
                                    family = binomial,
                                    data = subset(analysis.data, availability == 0 & is.element(id, subset_ids)))
  
  saveRDS(object = zerounavailablereward.model, file = paste("zerounavailablereward_cohort_",cohort,".RDS", sep = ""))
  
  unavailablereward.model <- gamm(reward ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                                    variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti,
                                  # random = ~(engagement | id) + (temperature | id) + (logpresteps | id),
                                  data = subset(analysis.data, availability == 0 & zeroreward == 0 & is.element(id, subset_ids)))
  
  
  saveRDS(object = unavailablereward.model, file = paste("unavailablereward_cohort_",cohort,".RDS", sep = ""))
  
  ## Availability model
  availability.model <- gamm(availability ~ s(day) + s(dosage) + engagement + other.location + ## Baseline Model
                               variation + s(temperature) + s(logpresteps) + s(sqrt.totalsteps) + prior.anti,
                             data = subset(analysis.data, is.element(id, subset_ids)), family = binomial)
  
  summary(availability.model$gam)
  
  saveRDS(object = availability.model, file = paste("availability_cohort_",cohort,".RDS", sep = ""))
  
  ## Transition model
  # Day, Dosage are deterministic.
  # Temperature: exogenous so use the users exact temperature.
  # Location: (assume it is exogenous to the intervention so use the users exact sequence)
  # Build rest via a sequence of conditional distributions: 

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
  zerologpresteps.model <- gamm(zerologpre ~ s(day) + s(dosage) + other.location + 
                              s(temperature) + engagement + variation,
                              family = binomial,
                            data = subset(analysis.data, is.element(id, subset_ids)))
  
  saveRDS(object = zerologpresteps.model, file = paste("zerologpresteps_cohort_",cohort,".RDS", sep = ""))
  
  logpresteps.model <- gamm(logpresteps ~ s(day) + s(dosage) + other.location + 
                              s(temperature) + engagement + variation,
                            data = subset(analysis.data, zerologpre == 0 & is.element(id, subset_ids)))
  
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
}

