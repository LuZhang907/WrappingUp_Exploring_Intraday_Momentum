rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(ggplot2)
library(cowplot)

dat <- fread("/Users/luzhang/Desktop/wrap up/CTDvis.csv")
data <- data.frame(dat)
head(dat)


ggplot(dat)+
  geom_bar(aes(x = var, y = count), stat = "identity",color = "black", fill = "#80B8DF",show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(legend.title = element_blank(),
        legend.text = element_text(famil="serif"),
        #legend.position=c(3,10),
        #legend.direction = "vertical"
        axis.text =  element_text(color = "black",family = "serif"),
        axis.title = element_text(family = "serif",size=10,color = "black")
        
  )+
  labs(x = "", y = "significancy count")


  
 

  
  

  
