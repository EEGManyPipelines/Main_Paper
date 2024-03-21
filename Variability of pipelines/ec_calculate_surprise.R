library(reshape2)
#the entropy is the sum over all events of the expected surprisal.
#So what we're looking for is the expected surprisal of each level of each variable.
exp_surprisal <- function(x, base=exp(1)) {
  t <- table(x)
  freq <- t/sum(t)
  ifelse(freq==0, 0, -freq * log(freq, base))
}

lapply(h1_data, exp_surprisal)
entropy_data <- function(x) sum(exp_surprisal(x))
entropy_v2 <- lapply(h1_data, entropy_data)
plot_entr_v2<-melt(as.data.frame(entropy_v2))

par(mar = c(2,10,2,2)) # Set the margin on all sides to 2
barplot(height=plot_entr_v2$value, names=plot_entr_v2$variable, 
        col="#69b3a2",
        horiz=T, las=1)

