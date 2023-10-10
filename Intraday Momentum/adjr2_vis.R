rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(ggplot2)
library(cowplot)

data <- fread("/Users/luzhang/Desktop/wrap up/adjR2_ctr.csv")
data <- data.frame(data)
head(data)



p1 <- ggplot(data = data, aes(x = stock, y = adjr2, group = CTR))+
  geom_point(aes(shape=CTR,color = CTR, size = CTR))+
  scale_shape_manual(values = c(15,16))+
  scale_size_manual(values =c(2.5,2.5))+
  scale_color_manual(values = c("#4055B5","#FF3B3B"))+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "", y = "adjR2 (%)")

dat <- fread("/Users/luzhang/Desktop/wrap up/adjR2.csv")
data <- data.frame(dat)
head(dat)

p2<- ggplot(data = dat, aes(x = stock, y = adjR2, group = target))+
  geom_point(aes(shape=target,color = target, size = target))+
  scale_shape_manual(values = c(15,16,17,18))+
  scale_size_manual(values =c(2.5,2.5,2.5,2.5))+
  scale_color_manual(values = c("orchid4","#4055B5","#FF3B3B",'orange4'))+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "", y = "adjR2 (%)")

#+  
#  theme_bw()+
#  labs(x = "", y = "")

plot_grid(p2,p1, ncol = 2, nrow=1)


  
  

  
