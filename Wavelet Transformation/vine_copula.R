rm(list = setdiff(ls(),lsf.str()))
library("VineCopula")
library("rvinecopulib")
library("copula")
library("network")
library(data.table)
library(ensembleBMA)
library(network)

uscore <- function(data, aunif = -0.5)
{
  if (is.vector(data))
  {
    nr <- length(data)
    us <- ((1:nr) + aunif)/(nr + 1 + 2 * aunif)
    jj <- rank(data)
    out <- us[jj]
  } else
  {
    nc <- ncol(data)
    nr <- nrow(data)
    out <- matrix(0, nr, nc)
    us <- ((1:nr) + aunif)/(nr + 1 + 2 * aunif)
    for (j in 1:nc)
    {
      jj <- rank(data[, j])
      tem <- us[jj]
      out[, j] <- tem
    }
  }
  out
}

raw_res <-fread("/Volumes/SSD-2TB/AMZN/Data/AMZN_stdRes_sp_20180110.csv")
raw_res <- data.frame(raw_res)
head(raw_res)

uu_res <- uscore(raw_res)
qq_res <- qnorm(uu_res)

uu_res <- data.frame(uu_res)
qq_res <- data.frame(qq_res)

colnames(uu_res) <- colnames(raw_res)
colnames(qq_res) <- colnames(raw_res)

#head(uu_res)
#head(qq_res)
setwd("/Volumes/SSD-2TB/AMZN/pairplots")

pdf("AMZN_uu_resi_sp_scatterplot_20180110.pdf",30,30)
GGally::ggpairs(uu_res, progress=FALSE)
dev.off()

pdf("AMZN_qq_resi_sp_scatterplot_20180110.pdf",30,30)
GGally::ggpairs(qq_res, progress=FALSE)
dev.off()

#plot(uu_res$Z_SP1,uu_res$Z_SP2)
#plot(uu_res$Z_SP9, uu_res$Z_SP10)

##### dependence structure among two adjacent layers ####

uu_NB01 <- uu_res[c("Z_SP", "Z_SP1")]
uu_0102 <- uu_res[c("Z_SP1", "Z_SP2")]
uu_0203 <- uu_res[c("Z_SP2", "Z_SP3")]
uu_0304 <- uu_res[c("Z_SP3", "Z_SP4")]
uu_0405 <- uu_res[c("Z_SP4", "Z_SP5")]
uu_0506 <- uu_res[c("Z_SP5", "Z_SP6")]
uu_0607 <- uu_res[c("Z_SP6", "Z_SP7")]
uu_0708 <- uu_res[c("Z_SP7", "Z_SP8")]
uu_0809 <- uu_res[c("Z_SP8", "Z_SP9")]
uu_0910 <- uu_res[c("Z_SP9", "Z_SP10")]

qq_NB01 <- qq_res[c("Z_SP", "Z_SP1")]
qq_0102 <- qq_res[c("Z_SP1", "Z_SP2")]
qq_0203 <- qq_res[c("Z_SP2", "Z_SP3")]
qq_0304 <- qq_res[c("Z_SP3", "Z_SP4")]
qq_0405 <- qq_res[c("Z_SP4", "Z_SP5")]
qq_0506 <- qq_res[c("Z_SP5", "Z_SP6")]
qq_0607 <- qq_res[c("Z_SP6", "Z_SP7")]
qq_0708 <- qq_res[c("Z_SP7", "Z_SP8")]
qq_0809 <- qq_res[c("Z_SP8", "Z_SP9")]
qq_0910 <- qq_res[c("Z_SP9", "Z_SP10")]

sink(file = "/Volumes/SSD-2TB/AMZN/txt/sp_dep_20180110.txt")
fit_NB01 <- vinecop(data=uu_NB01, family_set = "all", keep_data = TRUE)
summary(fit_NB01)
#plot(fit,1:4)
#AIC(fit)
#BIC(fit)

fit_0102 <- vinecop(data=uu_0102, family_set = "all", keep_data = TRUE)
summary(fit_0102)

fit_0203 <- vinecop(data=uu_0203, family_set = "all", keep_data = TRUE)
summary(fit_0203)

fit_0304 <- vinecop(data=uu_0304, family_set = "all", keep_data = TRUE)
summary(fit_0304)

fit_0405 <- vinecop(data=uu_0405, family_set = "all", keep_data = TRUE)
summary(fit_0405)

fit_0506 <- vinecop(data=uu_0506, family_set = "all", keep_data = TRUE)
summary(fit_0506)

fit_0607 <- vinecop(data=uu_0607, family_set = "all", keep_data = TRUE)
summary(fit_0607)

fit_0708 <- vinecop(data=uu_0708, family_set = "all", keep_data = TRUE)
summary(fit_0708)

fit_0809 <- vinecop(data=uu_0809, family_set = "all", keep_data = TRUE)
summary(fit_0809)

fit_0910 <- vinecop(data=uu_0910, family_set = "all", keep_data = TRUE)
summary(fit_0910)

fit_all <- vinecop(data=uu_res, family_set = "all", keep_data = TRUE)
summary(fit_all)

sink(file = NULL)

# set.seed(1111)
# 
# ### pair NB01
# vstructure <- fit_NB01$structure
# pclist <- fit_NB01$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) # simulated
# GGally::ggpairs(qq_NB01, progress=FALSE) # real
# 
# 
# ### pair 0102
# vstructure <- fit_0102$structure
# pclist <- fit_0102$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0102, progress=FALSE) 
# 
# ### pair 0203
# vstructure <- fit_0203$structure
# pclist <- fit_0203$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0203, progress=FALSE) 
# 
# ### pair 0304
# vstructure <- fit_0304$structure
# pclist <- fit_0304$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0304, progress=FALSE) 
# 
# ### pair 0405
# vstructure <- fit_0405$structure
# pclist <- fit_0405$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0405, progress=FALSE) 
# 
# ### pair 0506
# vstructure <- fit_0506$structure
# pclist <- fit_0506$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0506, progress=FALSE)
# 
# ### pair 0607
# vstructure <- fit_0607$structure
# pclist <- fit_0607$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0607, progress=FALSE)
# 
# ### pair 0708
# vstructure <- fit_0708$structure
# pclist <- fit_0708$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0708, progress=FALSE)
# 
# ### pair 0809
# vstructure <- fit_0809$structure
# pclist <- fit_0809$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0809, progress=FALSE)
# 
# ### pair 0910
# vstructure <- fit_0910$structure
# pclist <- fit_0910$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(qq_0910, progress=FALSE)
# 
# 
# ##### dependence structure among all layers ####
# vstructure <- fit_all$structure
# pclist <- fit_all$pair_copulas
# vcop <- vinecop_dist(pclist,vstructure)
# contour(vcop)
# 
# # simulation
# UU <- rvinecop(2000, vcop, qrng=FALSE, cores=1)
# z <- qnorm(UU)
# GGally::ggpairs(as.data.frame(z), progress=FALSE) 
# GGally::ggpairs(as.data.frame(qq_res), progress=FALSE)
