#pozn - sample
definitionLine <- c()
definitionLine$numberOfLevels <- 50
definitionLine$bottomLevel <- exp(0)
definitionLine$treshold <- exp(0.6)
definitionLine$upperLevel <- exp(2)

x <- log((0:(definitionLine$numberOfLevels/2-1)) / (definitionLine$numberOfLevels/2-1) * (definitionLine$treshold - definitionLine$bottomLevel) + definitionLine$bottomLevel)

definitionLine$helpline <- log((0:(definitionLine$numberOfLevels/2-1)) / (definitionLine$numberOfLevels/2-1) * (definitionLine$upperLevel - definitionLine$treshold) + definitionLine$treshold)

x <- c(x, c((log(definitionLine$upperLevel) + log(definitionLine$treshold))) - rev(definitionLine$helplinex))



#rm(definitionLine)
