library(ggplot2)
library(dplyr)
library(ggfortify)
library(readr)
library(lme4)
library(lmerTest)
library(ordinal)
library(lsmeans)
# library(lubridate)
# setwd("~/Dropbox/Gila Monster Data/GM_Study")


## LOAD DATASET FOR YEAR:
year <- read_csv("GM_Consolidated_ByYear.csv")

################################################################################################
##                                                                                            ##
##                                   ANALYSES BY YEAR                                         ##
##                                       100% MCPs                                            ##
################################################################################################
## ORIGINAL CODE: ANCOVA SEX:ENVIRONMENT, LSM:

# GMmodsex<-lm(Home_Range_100mcp~Sex*Environment,data=year)
# summary(GMmodsex)

## FIND LSMs FOR THE MODELS:
# marginal <- lsmeans(GMmodsex,
#                    ~ Environment)

# For Sex:
# refR_sex <- lsmeans(GMmodsex, specs = c("Sex","Environment"))
# ref_dfR_sex <- as.data.frame(summary(refR_sex))
# pd_red <- position_dodge(0.1)
# g4R_sex <- ggplot(ref_dfR_sex, aes(x=Sex, y=lsmean,group=Environment, colour=Environment))+
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
#   geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Sex")+ylab("Area (ha) using 100% MCP")+
#   theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# LSMgraph_sex<-print(g4R_sex)
# # ggsave("outputs/LSM_Year_plot.pdf")
LSMgraph_sex + theme(legend.title = element_blank(),
                     legend.justification=c(0,1),
                     legend.position=c(0.05, 0.95),
                     legend.background = element_blank(),
                     legend.key = element_blank(),
                     legend.box.background = element_rect(colour = "black"))



################################################################################################
## RMANOVA for Environment+Year+Sex+N:
RMmod.year<-lmer(Home_Range_100mcp~Environment+Year+Sex+N100+(1|Gila),data = year)
summary(RMmod.year)
anova(RMmod.year)


## FIND LSMs FOR THE MODELS:

# marginal <- lsmeans(GMmodsex,
#                    ~ Environment)
RM.marginal <- lsmeans(RMmod.year, 
                    ~ Environment)
RM.marginal


## CATAGORIZE LSM GRAPH BY SEX BETWEEN ENVIRONMENT:
refRM_sex <- lsmeans(RMmod.year, specs = c("Sex","Environment"))
refRM_sex
ref_dfRM_sex <- as.data.frame(summary(refRM_sex))
pd_RM <- position_dodge(0.1)
g4RM_sex <- ggplot(ref_dfRM_sex, aes(x=Sex, y=lsmean,group=Environment, colour=Environment))+
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd_RM) +
  geom_point(position=pd_RM)+theme_classic()+ggtitle("")+xlab("Sex")+ylab("Area (ha) using 100% MCP")+
  theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# RM_LSMgraph_sex<-print(g4RM_sex)
# ggsave("outputs/LSM_Year_plot.pdf")
RM_LSMgraph_sex + theme(legend.title = element_blank(),
                     legend.justification=c(0,1),
                     legend.position=c(0.05, 0.95),
                     legend.background = element_blank(),
                     legend.key = element_blank(),
                     legend.box.background = element_rect(colour = "black"))



################################################################################################


################################################################################################
## RMANOVA MIXED:

Mix.RMmod.year<-lmer(Home_Range_100mcp~Environment+Year+Sex+Environment:Sex+(1|Gila),
                 data = year)
summary(Mix.RMmod.year)
anova(Mix.RMmod.year)

# rm(Mix.RMmod.year2)

################################################################################################
##                                                                                            ##
##                                   ANALYSES BY SEASON                                       ##
##                                       100% MCPs                                            ##
################################################################################################
## SUMMARY

# seas.summ<-seasonal%>%group_by(Season,Environment)%>%summarise(meanHR=mean(Home_Range_100mcp,na.rm=TRUE))
# seas.summ

##############################################
## RMANOVA SEASON MIXED
seasonal<-read.csv("SC_Seasonal_Input.csv")

# View(seasonal)

# construct ANCOVA:
RM.mod.Season <- lmer(Home_Range_100mcp~Environment+Season+Sex+Environment:Season+(1|Gila),
                      data=seasonal)
anova(RM.mod.Season)
summary(RM.mod.Season)

# marginal.season <- lsmeans(RM.mod.Season, 
#                    ~ Environment)
# marginal.season

ref.season2 <- lsmeans(RM.mod.Season, specs = c("Season","Environment"))
ref.season2
ref_dfseason2 <- as.data.frame(summary(ref.season2))
pd.RMS <- position_dodge(0.1)
g4season2 <- ggplot(ref_dfseason2, aes(x=Season, y=lsmean,group=Environment, colour=Environment))+
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd.RMS) +
  geom_line(position=pd.RMS)+
  geom_point(position=pd.RMS)+theme_classic()+ggtitle("")+xlab("Season")+ylab("Area (ha) using 100% MCP")+theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# LSMgraphseason<-print(g4season)
g4season2<-g4season2 + theme(legend.title = element_blank(),
                           legend.justification=c(0,1),
                           legend.position=c(0.05, 0.95),
                           legend.background = element_blank(),
                           legend.key = element_blank(),
                           legend.box.background = element_rect(colour = "black"))
g4season2+scale_x_discrete(limits= c("Emergence","Dry","Monsoon","Post_Monsoon"))

################################################################################################
## RMANOVA MIXED:

Mix.RM.mod.Season <- lmer(Home_Range_100mcp~Environment+Season+Sex+
                            Environment:Season+(1|Gila), data=seasonal)
summary(Mix.RM.mod.Season)
anova(Mix.RM.mod.Season)

##Maximum likelihood-based mixed-effects models in R

mer1 <- lmer(Home_Range_100mcp~Sex+(1+Sex|Environment)+Season+(1+Sex|Season)+(1|Gila), 
             data= seasonal)
summary(mer1)
anova(mer1)




################################################################################################
##                                                                                            ##
##                                   ANALYSES BY YEAR                                         ##
##                                       95% MCPs                                             ##
################################################################################################
## RMANOVA for Year+Sex+Environment:
year <- read_csv("GM_Consolidated_ByYear_Input.csv")

RMmod.year95<-lmer(Home_Range_95mcp~Environment+Year+Sex+(1|Gila),data = year)
summary(RMmod.year95)
anova(RMmod.year95)


## FIND LSMs FOR THE MODELS:

# marginal <- lsmeans(GMmodsex,
#                    ~ Environment)
RM.marginal95 <- lsmeans(RMmod.year, 
                       ~ Environment)
RM.marginal95


## CATAGORIZE LSM GRAPH BY SEX BETWEEN ENVIRONMENT:
refRM_sex95 <- lsmeans(RMmod.year95, specs = c("Sex","Environment"))
refRM_sex95
ref_dfRM_sex95 <- as.data.frame(summary(refRM_sex95))
pd_RM95 <- position_dodge(0.1)
g4RM_sex95 <- ggplot(ref_dfRM_sex95, aes(x=Sex, y=lsmean,group=Environment, colour=Environment))+
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd_RM95) +
  geom_point(position=pd_RM95)+theme_classic()+ggtitle("")+xlab("Sex")+ylab("Area (ha) using 95% MCP")+
  theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# RM_LSMgraph_sex95<-print(g4RM_sex95)
# ggsave("outputs/LSM_Year_plot.pdf")
RM_LSMgraph_sex95 + theme(legend.title = element_blank(),
                        legend.justification=c(0,1),
                        legend.position=c(0.05, 0.95),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.box.background = element_rect(colour = "black"))



################################################################################################
## RMANOVA MINUS N100:
# RMmod.year.MINN100<-lmer(Home_Range_100mcp~Environment+Year+Sex+(1|Gila),data = year)
# # summary(RMmod.year.MINN100)
# anova(RMmod.year.MINN100)
# 
# ## FIND LSMs FOR THE MODELS:
# RM.marginal.MINN100 <- lsmeans(RMmod.year.MINN100, 
#                                ~ Environment)
# RM.marginal.MINN100


################################################################################################
## RMANOVA MIXED:

Mix.RMmod.year95<-lmer(Home_Range_95mcp~Environment+Year+Sex+Environment:Sex+(1|Gila),
                     data = year)
summary(Mix.RMmod.year95)
anova(Mix.RMmod.year95)


##################################
##                              ##
##      HR Overlap Analyses     ##
##                              ##
##################################

OL_Data = read.csv("HR_Overlap_Data.csv")
OL_Data

FF_OL<-slice(OL_Data,1:7)
MF_OL<-slice(OL_Data,8:20)
MM_OL<-slice(OL_Data,21:24)

### Calculate standard error manually

sd(FF_OL$ OL, na.rm=TRUE) /  
  sqrt(length(FF_OL$OL[!is.na(FF_OL$ OL)]))      # Standard error

sd(MF_OL$ OL, na.rm=TRUE) /  
  sqrt(length(MF_OL$OL[!is.na(MF_OL$ OL)]))

sd(MM_OL$ OL, na.rm=TRUE) /  
  sqrt(length(MM_OL$OL[!is.na(MM_OL$ OL)]))


################################################################################################
################################################################################################
################################################################################################

Refugia <- read.csv('./Refuge_Use/Refugia_Input.csv')
View(Refugia)


