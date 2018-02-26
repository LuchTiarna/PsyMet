#data visualization
fitDataVisualization <- unnest(fitDat, data)


plot <- ggplot(data=fitDataVisualization)
plot <- plot + geom_line( mapping=aes(x=level, y=hitPercentage))
plot <- plot + geom_line( mapping=aes(x=level, y=fitedHitPercentage, group=as.factor(id)), alpha=0.2, colour="red")
#plot <- plot + geom_point( mapping=aes(x=level, y=simulatedHitPercentage), position="jitter")
plot <- plot + geom_point( mapping=aes(x=fitTreshold, y=0.5), colour="yellow")
plot <- plot + geom_point( mapping=aes(x=treshold, y=0.5), colour="green")
plot <- plot + facet_grid(fitSigmoid + fitCore~sigmoid + core + gamma + lambda + param1 + param2)
plot
