#data visualization
fitDataVisualization <- unnest(fitDat, data)


plot <- ggplot(data=fitDataVisualization)
plot <- plot + geom_line( mapping=aes(x=level, y=hitPercentage))
plot <- plot + geom_line( mapping=aes(x=level, y=fitedHitPercentage), colour="red")
plot <- plot + geom_point( mapping=aes(x=level, y=simulatedHitPercentage))
plot <- plot + geom_point( mapping=aes(x=treshold, y=PSfunction(gamma, lambda, sigmoid, core, treshold, param1, param2)), colour="green")
plot <- plot + geom_point( mapping=aes(x=fitTreshold, y=PSfunction(fitGamma, fitLambda, fitSigmoid, fitCore, fitTreshold, fitParam1, fitParam2)), colour="yellow")
plot <- plot + facet_grid(fitSigmoid + fitCore~sigmoid + core + gamma + lambda + param1 + param2)
plot

