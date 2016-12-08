
#switching graph axis for social choice coded as 1 rather than zero.  
# Try to plot this 

limits <- aes(ymax = 1-d.pred$PI.U, ymin = 1-d.pred$PI.L)
tryingPlot <- ggplot(data = d.pred, aes(Condition, 1-means, shape = Sex))
tryingPlot + geom_point(data = d.pred, stat="identity", position = position_dodge(width=0.3), size = 3.5) + 
  geom_errorbar(limits, width = 0.08, position = position_dodge(width=0.3)) +
  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) + 
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion Chose Social Information") +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_x_discrete(limits=c("Control", "Social Risky","Asocial Risky")) 

#getting graph switched#
d.pred$flippedMean <- 1 - d.pred$means
d.pred$flipped_L <- 1 - d.pred$PI.L  
d.pred$flipped_U <- 1 - d.pred$PI.U
d.pred$estimateM <- d.pred$means - 0.5
d.pred$estimateL <- d.pred$PI.L - 0.5
d.pred$estimateU <-  d.pred$PI.U - 0.5

#saving this
write.table(d.pred, file = "d.pred", sep = "\t")
#reopen this again
readD.Pred <- read.delim("d.pred", sep = "\t")
