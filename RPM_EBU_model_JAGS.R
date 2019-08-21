rm(list = ls())

if (!"rjags" %in% installed.packages()) install.packages("rjags")
if (!"PerformanceAnalytics" %in% installed.packages()) install.packages("PerformanceAnalytics")
library(rjags)
library(PerformanceAnalytics)

setwd("C:/Users/Owner/Desktop/Ebullition_FCR_forecasts/")

data = read.csv('ebu_AR_model_RQT_21May19.csv')

base_model <- lm(log_ebu_mgCH4_m2_d ~ log_ebu_mgCH4_m2_d_1 + temp_b_avg, data = data)
summary(base_model)



N <- length(data)

sink("jags_model.bug")
cat('model {
  for (i in 1:N) {
    ebu[i] ~ dnorm(ebu.hat[i], tau)
    ebu.hat[i] <- beta[1] + beta[2]*ebu_lag[i] + beta[3]*temp[i]
  }
  
  #Vague priors on the beta
  for(j in 1:3){
    beta[j] ~ dnorm(0,1/100000)
  }

  # Prior for the inverse variance
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
}'
)
sink()


jags <- jags.model('jags_model.bug',
                   data = list('ebu' = data$log_ebu_mgCH4_m2_d,
                               'ebu_lag' = data$log_ebu_mgCH4_m2_d_1,
                               'temp' = data$temp_b_avg,
                               'N' = N),
                   n.chains = 10,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 1000)

samples = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 10000)

summary(samples)

par_matrix <- as.matrix(samples[1])

chart.Correlation(par_matrix)

save(samples, file = "MCMC_output_AR_RPM.Rdata")



# 
# y = data$ebu_mgCH4_m2_d
# ####################################################3
# sink("jags_model.bug")
# cat('model {
#     
# for(i in 1:N){
# y[i] ~ dnorm(m[i], tau_obs)  #this is a placeholder distribution
# m[i] <- exp(ebu_ghost[i])
# }
# 
# 
# 
# for (i in 2:N) {
#     ebu_ghost[i] ~ dnorm(ebu_lambda[i], tau)
#     ebu_lambda[i] <- beta[1] + beta[2]*ebu_ghost[i-1] + beta[3]*temp[i]
#     }
#     
#     #Vague priors on the beta
#     for(j in 1:3){
#     beta[j] ~ dnorm(0,1/100000)
#     }
#     
#     # Prior for the inverse variance
#     ebu_ghost[1] ~ dnorm(log(0.1), 100)
#     sigma ~ dunif(0, 100) # standard deviation
#     tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
#     tau_obs <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS placeholder
#     }'
# )
# sink()
# 
# jags <- jags.model('jags_model.bug',
#                    data = list('y' = y,
#                                'temp' = data$temp_b_avg,
#                                'N' = N),
#                    n.chains = 3,
#                    n.adapt = 100)
# 
# #burn in, this updates the jags$state()
# update(jags,n.iter = 1000)
# 
# #sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
# samples = coda.samples(model = jags,
#                        variable.names = c('beta','sigma'),
#                        n.iter = 1000)



