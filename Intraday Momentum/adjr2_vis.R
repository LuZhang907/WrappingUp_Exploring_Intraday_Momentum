rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(ggplot2)

data <- fread("/Users/luzhang/Desktop/wrap up/adjR2_ctr.csv")
data <- data.frame(data)
head(data)



ggplot(data = data, aes(x = stock, y = adjr2, group = CTR))+
  geom_point(aes(shape=CTR,color = CTR, size = CTR))+
  scale_shape_manual(values = c(15,16))+
  scale_size_manual(values =c(2.5,2.5))+
  scale_color_manual(values = c("#4055B5","#FF3B3B"))+
  theme_bw()+
  labs(x = "", y = "adjR2 (%)")

#+  
#  theme_bw()+
#  labs(x = "", y = "")


  
  

  
