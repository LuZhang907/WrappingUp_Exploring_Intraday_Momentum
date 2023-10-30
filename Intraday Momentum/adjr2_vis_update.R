rm(list = setdiff(ls(),lsf.str()))
library(data.table)
library(ggplot2)
library(cowplot)

dat <- fread("/Users/luzhang/Desktop/wrap up/adjR2.csv")
data <- data.frame(dat)
head(dat)





ggplot(data = dat, aes(x = ticker, y = adjr2, group = target, color = target, shape = target))+
  geom_point(size=4)+
  geom_line(position = position_dodge(0.1),cex=1.3)+
  scale_shape_manual(values = c(16,16,17,18,17,16,16))+
  scale_color_manual(values = c("#EF3B3B","#070707","#FECE41","#80B8DF","#FFB8BB","#4055B5","#FE807D"))+
  labs(x='',y="adjust R2 (%)")+
  theme_test(base_size = 10)+
  theme(legend.title = element_blank(),
        legend.text = element_text(famil="serif"),
        #legend.position=c(3,10),
        #legend.direction = "vertical"
        axis.text =  element_text(color = "black",family = "serif"),
        axis.title = element_text(family = "serif",size=10,color = "black")
    
  )
  

  
  
  
 # scale_shape_manual(values = c(15,16,17,18))+
 # scale_size_manual(values =c(2.5,2.5,2.5,2.5))+
#  scale_color_manual(values = c("orchid4","#4055B5","#FF3B3B",'orange4'))+
 # theme_bw()+
 # theme(legend.position = "top")+
 # labs(x = "", y = "adjR2 (%)")

#+  
#  theme_bw()+
#  labs(x = "", y = "")




  
  

  
