library(wavethresh)
library(ggplot2)
library(cowplot)

p1 <- draw.default(filter.number=4, family="DaubExPhase", enhance=FALSE, main="db4 wavelet function", 
             scaling.function = F, xlab = "", ylab = "", plot.it = FALSE) # mother wavelet
p2 <- draw.default(filter.number=4, family="DaubExPhase", enhance=FALSE, main="db4 scaling function", 
             scaling.function = T,xlab = "", ylab = "", plot.it =FALSE) # father wavelet

p1 <- data.frame(p1)
p2 <- data.frame(p2)

g1 <- ggplot(data = p1, aes(x=x,y=y))+
  geom_point()+
  theme_bw()+
  labs(x = "", y = "", title = "db4 wavelet function")

g2 <- ggplot(data = p2, aes(x=x,y=y))+
  geom_point()+
  theme_bw()+
  labs(x = "", y = "", title = "db4 scaling function")

plot_grid(ncol = 2,  nrow = 1, g1, g2)

