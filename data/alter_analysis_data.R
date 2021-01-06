load("C:/Users/Balthazar/Box/HeartSteps/Walter/data/analysis-data.RData")
setwd("./GitHub/AI4Health-Online-Experimentation/data/RDS_files")
names(analysis.data)
analysis.data$id = as.numeric(analysis.data$id)

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
analysis.data$zeroreward = as.numeric(analysis.data$reward == min(analysis.data$reward, na.rm = TRUE))
analysis.data$zerologpre = as.numeric(analysis.data$logpresteps == min(analysis.data$logpresteps, na.rm = TRUE))

save(analysis.data, file = "C:/Users/Balthazar/Box/HeartSteps/Walter/data/analysis-data-2.RData")

ggdrive.data = data.frame("id" = analysis.data$id, "day" = analysis.data$day,
                         "decision.time" = analysis.data$decision.time, 
                         "other.location" = analysis.data$other.location,
                         "temperature" = analysis.data$temperature)

save(ggdrive.data, file = "C:/Users/Balthazar/Box/HeartSteps/Walter/data/ggdrive.RData")


init.states = subset(analysis.data, day == 1 & decision.time == 1)
save(init.states, file = "C:/Users/Balthazar/Box/HeartSteps/Walter/data/initstates.RData")
