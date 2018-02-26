#Data generation

x <- c(-log(2:5/1.2),log(1:5/1.2))

#Multigenerate
dat <- multigenerate(list(c("gumbel_r","ab", 0, 0, 0.5, 1),
                          c("gumbel_l","weibull", 0, 0, 0.5, 1),
                          c("gauss","linear", 0, 0, 0.5, -0.5),
                          c("logistic","linear", 0, 0, 0.5, 0.5))
                     , x, rep(30,length(x)),
                     100
                     )

#Multifit
fitDat <-  multifit(dat, list(c("gumbel_l", "ab"), c("gauss", "linear"), c("logistic", "ab"), c("gumbel_l", "weibull")))

#filtering unimpropriate data
fitDat <- fitDat %>% filter(fitGamma + fitLambda < 1)
#treshold estimate
fitDat <- fitDat %>% group_by(sigmoid, core, gamma, lambda, param1, param2, fitSigmoid, fitCore, fitGamma, fitLambda, fitParam1, fitParam2) %>% mutate(treshold=PSfunction(gamma, lambda, sigmoid, core, 0.5, param1, param2, inverse = TRUE))
#slope
fitDat <- fitDat %>% group_by(sigmoid, core, gamma, lambda, param1, param2, fitSigmoid, fitCore, fitGamma, fitLambda, fitParam1, fitParam2) %>% mutate(slope=PSfunction(gamma, lambda, sigmoid, core, treshold, param1, param2, type="pdf"))
#treshold estimate
fitDat <- fitDat %>% group_by(sigmoid, core, gamma, lambda, param1, param2, fitSigmoid, fitCore, fitGamma, fitLambda, fitParam1, fitParam2) %>% mutate(fitTreshold=PSfunction(fitGamma, fitLambda, fitSigmoid, fitCore, 0.5, fitParam1, fitParam2, inverse = TRUE))
#slope
fitDat <- fitDat %>% group_by(sigmoid, core, gamma, lambda, param1, param2, fitSigmoid, fitCore, fitGamma, fitLambda, fitParam1, fitParam2) %>% mutate(fitSlope=PSfunction(fitGamma, fitLambda, fitSigmoid, fitCore, fitTreshold, fitParam1, fitParam2, type="pdf"))

#adding variables numberOfLevels, numberOfObservations, numberOfObervationsPerLevel
fitDat$numberOfLevels <- rep(NaN, nrow(fitDat))
fitDat$numberOfObservations <- rep(NaN, nrow(fitDat))
fitDat$meanOfObservationsPerLevel <- rep(NaN, nrow(fitDat))
fitDat$meanTresholdLevelDistance <- rep(NaN, nrow(fitDat))
for(i in 1:nrow(fitDat)){
  dat <- fitDat[i,]$data[[1]]
  #number of levels
  fitDat[i,]$numberOfLevels <- nrow(dat)
  #number of observations
  fitDat[i,]$numberOfObservations <- sum(dat$obsNumber)
  #number of observations per level
  fitDat[i,]$meanOfObservationsPerLevel <- mean(dat$obsNumber)
  #mean distance of treshold and level
  tres <- fitDat[i,]$treshold[[1]]
  fitDat[i,]$meanTresholdLevelDistance <- mean(abs(dat$level - tres))

}
