#Data generation

x <- log(1:5/1.2)

#Multigenerate
dat <- multigenerate(list(c("gumbel_r","ab", 0, 0, 0.5, 1),
                          c("gumbel_l","ab", 0, 0, 0.5, 1),
                          c("gauss","linear", 0, 0, 0.5, 1),
                          c("logistic","linear", 0, 0, 0.5, 1))


                     , x, rep(30,length(x)))

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
#number of levels
fidDat <- fitDat %>% group_by(sigmoid, core, gamma, lambda, param1, param2, fitSigmoid, fitCore, fitGamma, fitLambda, fitParam1, fitParam2) %>% mutate(numberOfLevels=nrow(data))
#number of observations
#number of observations per level
