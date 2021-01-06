# Generative models
# Inputs: Current State, Action, and Prob

generate_reward_state <- function(input) {
  # Input is a list with 3 arguments:
  # 1. State: (id, day, decision.time, dosage, engagement, other.location, 
  #            variation, temperature, logpresteps, sqrt.totalsteps, prior.anti)
  # 2. Availability: 0/1 
  # # 3. Action: 0/1
  # Ouput is a list with 3 arguments:
  # 1. Reward: Numeric value which is log of step-count with offset = 0.5
  # 2. State: Same as before
  # 3. Availability: for next decision time
  
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
  pred.data$prior.anti = as.logical(input$state[11])
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
  
  cohort = pred.data$id %% 5 +1
  
  available_reward_model = readRDS(file = paste("availablereward_cohort_",cohort,".RDS", sep = ""))
  unavailable_reward_model = readRDS(file = paste("unavailablereward_cohort_",cohort,".RDS", sep = ""))
  expit <- function(x) {1/(1+exp(-x))}
  
  if(input$available == 1) {
    mean_reward = predict(available_reward_model$gam, pred.data) 
    sig = sigma(available_reward_model$lme)
    reward = mean_reward + rnorm(1, mean = 0, sd = sig)
  } else {
    mean_reward = predict(unavailable_reward_model$gam, pred.data) 
    sig = sigma(unavailable_reward_model$lme)
    reward = mean_reward + rnorm(1, mean = 0, sd = sig)
  }
  
  if(pred.data$decision.time > 5) {
    print("Decision time must be integer <= 5.")
  } else {
    pred.data$decision.time = (pred.data$decision.time + 1)%%5 
    if (pred.data$decision.time == 0) {pred.data$decision.time = 5}
    if (pred.data$decision.time == 1) {
      pred.data$day = pred.data$day + 1 
    } else {
      pred.data$day = pred.data$day
    }
  }
  
  current.row = which(analysis.data$id == pred.data$id & analysis.data$day == pred.data$day & analysis.data$decision.time == pred.data$decision.time)
  
  if(identical(current.row, integer(0))) {
    return(NA)
  } else {
    pred.data$temperature = analysis.data$temperature[current.row]
    pred.data$dosage = pred.data$dosage * 0.95 + 1*(pred.data$action == 1)
    pred.data$other.location = analysis.data$other.location[current.row]
  }
  
  engagement.model = readRDS(file = paste("engagement_cohort_",cohort,".RDS", sep = ""))
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
  if(pred.data$decision.time == 1) {
    pred.data$sqrt.totalsteps = predict(sqrttotalsteps.model$gam, pred.data) + rnorm(1, mean = 0, sd = sigma(sqrttotalsteps.model$lme))
  }
  
  prioranti.model = readRDS(file = paste("prioranti_cohort_",cohort,".RDS", sep = ""))
  logit_prioranti = predict(prioranti.model$gam, pred.data)
  pred.data$prior.anti = as.logical(rbinom(n=1, size = 1, prob = expit(logit_prioranti)))
  
  availability_model = readRDS(file = paste("availability_cohort_",cohort,".RDS", sep = ""))
  logit_avail = predict(availability_model$gam, pred.data)
  user_avail = rbinom(n = 1, size = 1, prob = expit(logit_avail))
  
  output = list("reward" = reward,
                "state" = c(pred.data$id, pred.data$day, pred.data$decision.time, pred.data$dosage, 
                            pred.data$engagement, pred.data$other.location, pred.data$variation, 
                            pred.data$temperature, pred.data$logpresteps, pred.data$sqrt.totalsteps,
                            pred.data$prior.anti),
                "availability" = user_avail)
  
  output
}

pred.data = analysis.data[14941,]
reward = 1
user_avail = 1

output = list("reward" = reward,
              "state" = c(pred.data$id, pred.data$day, pred.data$decision.time, pred.data$dosage, 
                          pred.data$engagement, pred.data$other.location, pred.data$variation, 
                          pred.data$temperature, pred.data$logpresteps, pred.data$sqrt.totalsteps,
                          pred.data$prior.anti),
              "availability" = user_avail)

results = matrix(nrow = 30, ncol = 11)
rewards = vector(length = 30)
for(i in 1:30) {
  print(i)
  input = list("state" = output$state, "available" = 1, "action" = rbinom(n = 1, size = 1, prob = 0.5), "probability" = 0.5)
  output = generate_reward_state(input)
  results[i,] = output$state
  rewards[i] = output$reward
}

cbind(results, rewards)
