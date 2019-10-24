library(ggplot2)
library(dplyr)
library(ggfortify)
library(readr)

#############################################
##               Home range                ##
#############################################


#############################################
## preliminary analyses before distillation
#############################################

##################
##  ANCOVA
# setwd("~/Dropbox/Gila Monster Data/GM_Study")

# library(readr)
# GM_ByYear <- read_csv("GM_ByYear.csv")
# View(GM_ByYear)
# 
# glimpse(GM_ByYear)
# data<-GM_ByYear
# 
# ggplot(data,aes(x=effort,y=area_mcp,colour=environment))+
#   geom_point()+scale_colour_manual(values = c(sub="green",non_sub="red"))+
#   theme_bw()
# 
# # construct ANCOVA:
# GMmod<-lm(area_mcp~effort*environment,data=data)
# names(GMmod)
# 
# # assumptions
# autoplot(GMmod,smooth.colour = NA)
# 
# # ANOVA table
# anova(GMmod)
# 
# #summary table
# summary(GMmod)
# 
# # new plot with lines
# ggplot(data,aes(x=effort,y=area_mcp,colour=environment))+
#   geom_point()+scale_colour_manual(values = c(sub="green",non_sub="blue"))+
#   geom_smooth(method = "lm",se=FALSE)+
#   theme_bw()
# 
# ##################
# mod2<-lm(area_mcp~effort+environment+sex,data=data)
# 
# autoplot(mod2)
# 
# anova(mod2)
# summary(mod2)

################################################
## Multiple Regression after first distillation
################################################

############################################################################################
## 100 Percent MCP

library(readr)
data2 <- read_csv("GM_Consolidated_ByYear.csv")
View(data2)

# quick plot
ggplot(data2,aes(x=N100,y=Home_Range_100mcp,colour=Environment))+
  geom_point()+scale_colour_manual(values = c(subsidized="green",nonsubsidized="purple"))+
  geom_smooth(method = "lm",se=FALSE)+
  theme_bw()

# box plot
ggplot(data2,aes(x=Environment,y=Home_Range_100mcp)) + geom_boxplot() +
  theme_bw()

# multiple Regression Model
mod3<-lm(Home_Range_100mcp~N100+Environment+Sex+Year,data=data2)
summary(mod3)
anova(mod3)

# quick check of assumptions
autoplot(mod3,smooth.colour = NA)

###################################
##  ANCOVA after distillation
###################################

############################
# ANCOVA: effort*environment
View(data2)

glimpse(data2)
 
ggplot(data2,aes(x=N100,y=Home_Range_100mcp,colour=Environment))+
  geom_point()+scale_colour_manual(values = c(sub="green",non_sub="red"))+
  theme_bw()

# construct ANCOVA:
GMmod2<-lm(Home_Range_100mcp~N100*Environment,data=data2)
names(GMmod2)

#assumptions
autoplot(GMmod2,smooth.colour = NA)
 
# ANOVA table
anova(GMmod2)
#summary table
summary(GMmod2)
 
############################
# ANCOVA: sex*environment
View(data2)

glimpse(data2)

ggplot(data2,aes(x=effort,y=area_mcp100,colour=environment))+
  geom_point()+scale_colour_manual(values = c(sub="green",non_sub="red"))+
  theme_bw()

# construct ANCOVA:
GMmodsex<-lm(area_mcp100~sex*environment,data=data2)
names(GMmodsex)

#assumptions
autoplot(GMmodsex,smooth.colour = NA)

# ANOVA table
anova(GMmodsex)
#summary table
summary(GMmodsex)

# new plot with lines
Graph1<-ggplot(data2,aes(x=effort,y=area_mcp100,colour=environment))+
  geom_point()+scale_colour_manual(values = c(subsidized="green",nonsubsidized="blue"))+
  xlab("Number of Relocations")+ylab("Area (ha) using 100% MCP")+
  geom_smooth(method = "lm",se=FALSE)+
  theme_bw()

Graph1<-Graph1+theme(axis.title=element_text(size = 14))

# legend at top-left, inside the plot
Graph1 + theme(legend.title = element_blank(),
           legend.justification=c(0,1), 
           legend.position=c(0.05, 0.95),
           legend.background = element_blank(),
           legend.key = element_blank(),
           legend.box.background = element_rect(colour = "black"))


###################################
## Repeated measures/mixed model
###################################

# ggplot(data2,aes(x=effort,y=area_mcp100,colour=environment))+
#   geom_point()+geom_text(aes(label=sex),hjust=0, vjust=0)+
#   scale_colour_manual(values = c(sub="green",non_sub="purple"))+
#   geom_smooth(method = "lm",se=FALSE)+
#   theme_bw()

library(lme4)
library(readr)
year <- read_csv("GM_Consolidated_ByYear.csv")
View(year)

lmemodel.year<-lmer(Home_Range_100mcp~Environment+N100+Year+(1|Gila),data = year)
summary(lmemodel.year)
# anova(lmemodel)
# summary(lmemodel)

# Get p-values from mixed model F values:
library(lmerTest)
#lmm <- lmer(value~status+(1|experiment))
lmepval<-lmer(Home_Range_100mcp~Environment+N100+Year+(1|Gila),data = GM_All_YearDist)
summary(lmepval)
anova(lmepval)

# rm(GM_All_YearDist)
########################################################
##              Find least square means
##                  ****WORKING****
########################################################

library(ordinal)
library(lsmeans)

marginal2 = lsmeans(GMmod2, 
                   ~ environment)
marginal2

CLD<-cld(marginal2,
    + alpha=0.05,
    + Letters=letters,      ### Use lower-case letters for .group
    + adjust="tukey")       ### Tukey-adjusted comparisons
CLD

refR <- lsmeans(GMmod2, specs = c("effort","environment"))
ref_dfR <- as.data.frame(summary(refR))
pd <- position_dodge(0.1)
g4R <- ggplot(ref_dfR, aes(x=effort, y=lsmean,group=environment, colour=environment))+
     geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
     geom_line(position=pd)+
     geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Effort")+ylab("Area (ha) using 100% MCP")+
     theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
LSMgraph<-print(g4R)

# For Sex:
refR <- lsmeans(GMmodsex, specs = c("sex","environment"))
ref_dfR <- as.data.frame(summary(refR))
pd <- position_dodge(0.1)
g4R <- ggplot(ref_dfR, aes(x=effort, y=lsmean,group=environment, colour=environment))+
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
  geom_line(position=pd)+
  geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Effort")+ylab("Area (ha) using 100% MCP")+
  theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
LSMgraph<-print(g4R)
LSMgraph
#################################
##    Graph Least Square Means:

# LSMgraph+theme(legend.title = element_blank(),
#                 legend.justification=c(0,1),
#                 legend.position=c(0.78, 0.95),
#                 legend.background = element_blank(),
#                 legend.key = element_blank(),
#                 legend.box.background = element_rect(colour = "black"))

# ggplot(CLD, aes(x=environment, y=lsmean,group=environment, colour=environment))+
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
#   geom_line(position=pd)+
#   geom_point(position=pd)+theme_classic()+xlab("environment")+
#   ylab("Least Square Mean")+
#   theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# 
# LSMgraph2<-LSMgraph2+theme(legend.title = element_blank(),
#                 legend.justification=c(0,1),
#                 legend.position=c(0.78, 0.95),
#                 legend.background = element_blank(),
#                 legend.key = element_blank(),
#                 legend.box.background = element_rect(colour = "black"))
LSMgraph2

# ggplot(CLD, aes(x=environment, y=lsmean)) +
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.2, position=pd)+
#   xlab("Environment")+ylab("Least Sqaure Means")+
#   geom_bar(stat="identity", fill="steelblue",position = pd)+
#   theme_minimal()

LSMbar<-ggplot(CLD, aes(x=environment, y=lsmean, fill=environment)) +
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.2, position=pd,color="black")+
  xlab("Environment")+ylab("Least Sqaure Means")+
  geom_bar(stat="identity",position = pd)+scale_fill_manual("legend", values = c("non-sub" = "blue", "sub" = "green"))+
  theme_minimal()

LSMbar<-LSMbar + theme(legend.position="none")
LSMbar<-LSMbar+theme(axis.title=element_text(size = 14))
LSMbar

############################################################################################
## 95 Percent MCP

library(ggplot2)
library(dplyr)
library(ggfortify)
library(readr)

data2 <- read_csv("GM_Consolidated_ByYear.csv")
View(data2)

# quick plot
ggplot(data2,aes(x=N95,y=Home_Range_95mcp,colour=Environment))+
  geom_point()+scale_colour_manual(values = c(subsidized="green",nonsubsidized="purple"))+
  geom_smooth(method = "lm",se=FALSE)+
  theme_bw()

# box plot
ggplot(data2,aes(x=Environment,y=Home_Range_95mcp)) + geom_boxplot() +
  theme_bw()

# multiple Regression Model
mod4<-lm(Home_Range_100mcp~N95+Environment+Sex+Year,data=data2)
summary(mod4)
anova(mod4)

# quick check of assumptions
autoplot(mod4,smooth.colour = NA)

# ###################################
# ##  ANCOVA 
# ###################################
# 
# ############################
# # ANCOVA: effort*environment
# View(data2)
# 
# glimpse(data2)
# 
# ggplot(data2,aes(x=N195,y=Home_Range_95mcp,colour=Environment))+
#   geom_point()+scale_colour_manual(values = c(sub="green",non_sub="red"))+
#   theme_bw()
# 
# construct ANCOVA:
GMmod3<-lm(Home_Range_95mcp~N95*Environment,data=data2)
names(GMmod3)

#assumptions
autoplot(GMmod3,smooth.colour = NA)

# ANOVA table
anova(GMmod3)
#summary table
summary(GMmod2)

############################
# ANCOVA: sex*environment
View(data2)

glimpse(data2)

# ggplot(data2,aes(x=N95,y=Home_Range_95mcp,colour=Environment))+
#   geom_point()+scale_colour_manual(values = c(sub="green",non_sub="red"))+
#   theme_bw()

# construct ANCOVA:
GMmodsex<-lm(Home_Range_95mcp~Sex*Environment,data=data2)
names(GMmodsex)

#assumptions
autoplot(GMmodsex,smooth.colour = NA)

# ANOVA table
anova(GMmodsex)
#summary table
summary(GMmodsex)

# new plot with lines
Graph1<-ggplot(data2,aes(x=N95,y=Home_Range_95mcp,colour=Environment))+
  geom_point()+scale_colour_manual(values = c(subsidized="green",nonsubsidized="blue"))+
  xlab("Number of Relocations")+ylab("Area (ha) using 95% MCP")+
  geom_smooth(method = "lm",se=FALSE)+
  theme_bw()

Graph1<-Graph1+theme(axis.title=element_text(size = 14))

# legend at top-left, inside the plot
Graph1 + theme(legend.title = element_blank(),
               legend.justification=c(0,1),
               legend.position=c(0.05, 0.95),
               legend.background = element_blank(),
               legend.key = element_blank(),
               legend.box.background = element_rect(colour = "black"))


###################################
## Repeated measures/mixed model
###################################

# ggplot(data2,aes(x=effort,y=area_mcp100,colour=environment))+
#   geom_point()+geom_text(aes(label=sex),hjust=0, vjust=0)+
#   scale_colour_manual(values = c(sub="green",non_sub="purple"))+
#   geom_smooth(method = "lm",se=FALSE)+
#   theme_bw()

library(lme4)
library(readr)
GM_All_95 <- read_csv("GM_Consolidated_ByYear.csv")
View(GM_All_95)

lmemodel2<-lmer(Home_Range_95mcp~Environment+N95+Year+(1|Gila),data = GM_All_95)
anova(lmemodel2)
summary(lmemodel2)

# # Get p-values from mixed model F values:
# library(lmerTest)
# #lmm <- lmer(value~status+(1|experiment))
# lmepval<-lmer(area_mcp100~environment+effort+year+sex+(1|liz_number),data = GM_All_YearDist)
# summary(lmepval)
# anova(lmepval)

# ########################################################
# ##              Find least square means
# ##                  ****WORKING****
# ########################################################
# 
# library(ordinal)
# library(lsmeans)
# 
# marginal = lsmeans(GMmod2, 
#                    ~ environment)
# marginal
# 
# CLD<-cld(marginal,
#          + alpha=0.05,
#          + Letters=letters,      ### Use lower-case letters for .group
#          + adjust="tukey")       ### Tukey-adjusted comparisons
# CLD
# 
# refR <- lsmeans(GMmod2, specs = c("effort","environment"))
# ref_dfR <- as.data.frame(summary(refR))
# pd <- position_dodge(0.1)
# g4R <- ggplot(ref_dfR, aes(x=effort, y=lsmean,group=environment, colour=environment))+
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
#   geom_line(position=pd)+
#   geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Effort")+ylab("Area (ha) using 100% MCP")+
#   theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# LSMgraph<-print(g4R)
# 
# # For Sex:
# refR <- lsmeans(GMmodsex, specs = c("sex","environment"))
# ref_dfR <- as.data.frame(summary(refR))
# pd <- position_dodge(0.1)
# g4R <- ggplot(ref_dfR, aes(x=effort, y=lsmean,group=environment, colour=environment))+
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
#   geom_line(position=pd)+
#   geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Effort")+ylab("Area (ha) using 100% MCP")+
#   theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# LSMgraph<-print(g4R)
# 
# #################################
# ##    Graph Least Square Means:
# 
# # LSMgraph+theme(legend.title = element_blank(),
# #                 legend.justification=c(0,1),
# #                 legend.position=c(0.78, 0.95),
# #                 legend.background = element_blank(),
# #                 legend.key = element_blank(),
# #                 legend.box.background = element_rect(colour = "black"))
# 
# # ggplot(CLD, aes(x=environment, y=lsmean,group=environment, colour=environment))+
# #   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
# #   geom_line(position=pd)+
# #   geom_point(position=pd)+theme_classic()+xlab("environment")+
# #   ylab("Least Square Mean")+
# #   theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# # 
# LSMgraph2<-LSMgraph2+theme(legend.title = element_blank(),
#                            legend.justification=c(0,1),
#                            legend.position=c(0.78, 0.95),
#                            legend.background = element_blank(),
#                            legend.key = element_blank(),
#                            legend.box.background = element_rect(colour = "black"))
# 
# # ggplot(CLD, aes(x=environment, y=lsmean)) +
# #   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.2, position=pd)+
# #   xlab("Environment")+ylab("Least Sqaure Means")+
# #   geom_bar(stat="identity", fill="steelblue",position = pd)+
# #   theme_minimal()
# 
# LSMbar<-ggplot(CLD, aes(x=environment, y=lsmean, fill=environment)) +
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.2, position=pd,color="black")+
#   xlab("Environment")+ylab("Least Sqaure Means")+
#   geom_bar(stat="identity",position = pd)+scale_fill_manual("legend", values = c("non-sub" = "blue", "sub" = "green"))+
#   theme_minimal()
# 
# LSMbar<-LSMbar + theme(legend.position="none")
# LSMbar<-LSMbar+theme(axis.title=element_text(size = 14))
# LSMbar

#############################################
##               Movement                  ##
#############################################

library(lubridate)
library(readr)
SC_movement <- read_csv("Movement/SC_movement.csv")
View(SC_movement)

names(SC_movement)

# convert dates into season categories
# Season_df<-cut(DATE, breaks = c(),labels = c(""))

# unique(SC_movement$DATE)
# 
# movtdata<-mutate(SC_movement, DATE=mdy(DATE))
# glimpse(movtdata)
# View(movtdata)
# 
# library(lubridate)
# 
# SC_movement <- read_csv("Movement/SC_movement.csv", header=TRUE, stringsAsFactors = FALSE)
# 
# #date format 2010-08-21 YYYY-MM-DD
# #create dates variable for your column that contains dates
# dates <- SC_movement$Date_Found
# #get the month of the date, create new column called month
# SC_movement$month<-(month(dates, label=TRUE))
# #new column called season
# season <- SC_movement$season
# #divide up your seasons by month
# winter <-c("Dec","Jan", "Feb")
# spring <- c("Apr", "May","Mar")
# summer <- c("Jun", "Jul", "Aug")
# fall <- c("Sep","Oct", "Nov")


################################################
##          SEASONAL HOME RANGE               ##
################################################

########################
##  ANCOVA

# setwd("~/Dropbox/Gila Monster Data/GM_Study")

seasonal<-read.csv("SC_Seasonal_Data.csv")
View(seasonal)

# construct ANCOVA:
ANCOVA.mod.NSeas<-lm(Home_Range_100mcp~Season*Environment,data=seasonal)
names(ANCOVA.mod.NSeas)

#assumptions
autoplot(ANCOVA.mod.NSeas,smooth.colour = NA)

# ANOVA table
anova(ANCOVA.mod.NSeas)
#summary table
summary(ANCOVA.mod.NSeas)

############################
# ANCOVA: sex*season

# glimpse(data_reduced)
# 
# ggplot(data_reduced,aes(x=effort,y=Home_Range_100mcp,colour=Environment))+
#   geom_point()+scale_colour_manual(values = c(sub="green",non_sub="red"))+
#   theme_bw()

# construct ANCOVA:
GMmodsex_red<-lm(Home_Range_100mcp~Sex*Environment,data=data_reduced)
names(GMmodsex_red)

#assumptions
autoplot(GMmodsex_red,smooth.colour = NA)

# ANOVA table
anova(GMmodsex_red)
#summary table
summary(GMmodsex_red)


###########################
##  LSM

library(ordinal)
library(lsmeans)

marginal = lsmeans(ANCOVA.mod.NSeas, 
                   ~ Environment)
marginal

CLD_red<-cld(marginal,alpha=0.05,Letters=letters,adjust="tukey")       ### Tukey-adjusted comparisons### Use lower-case letters for .group
CLD

ref.season <- lsmeans(ANCOVA.mod.NSeas, specs = c("Season","Environment"))
ref.season
ref_dfseason <- as.data.frame(summary(ref.season))
pd <- position_dodge(0.1)
g4season <- ggplot(ref_dfseason, aes(x=Season, y=lsmean,group=Environment, colour=Environment))+
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
  geom_line(position=pd)+
  geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Season")+ylab("Area (ha) using 100% MCP")+
  theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# LSMgraphseason<-print(g4season)
g4season+scale_x_discrete(limits= c("Emergence","Dry","Monsoon","Post_Monsoon"))

# For Sex:
# refR_red <- lsmeans(GMmodsex_red, specs = c("Sex","Environment"))
# ref_dfR_red <- as.data.frame(summary(refR_red))
# pd_red <- position_dodge(0.1)
# g4R_red <- ggplot(ref_dfR_red, aes(x=Sex, y=lsmean,group=Environment, colour=Environment))+
#   geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE), width=.1,position=pd) +
#   geom_point(position=pd)+theme_classic()+ggtitle("")+xlab("Sex")+ylab("Area (ha) using 100% MCP")+
#   theme(plot.title = element_text(hjust = 0.5, color="black", size=14, face="bold"))
# LSMgraph_red<-print(g4R_red)

###################################
## Seasonal Repeated measures/mixed model 


# ggplot(data2,aes(x=effort,y=area_mcp100,colour=environment))+
#   geom_point()+geom_text(aes(label=sex),hjust=0, vjust=0)+
#   scale_colour_manual(values = c(sub="green",non_sub="purple"))+
#   geom_smooth(method = "lm",se=FALSE)+
#   theme_bw()

library(lme4)
library(readr)
# seasonal<-read.csv("SC_Seasonal_Data.csv")
# View(GM_All_95)

RM.seasonal<-lmer(Home_Range_100mcp~Environment+N+Season+Sex+(1|Gila),data = seasonal)
anova(RM.seasonal)
summary(RM.seasonal)

# # Get p-values from mixed model F values:
# library(lmerTest)
# #lmm <- lmer(value~status+(1|experiment))
# lmepval<-lmer(area_mcp100~environment+effort+year+sex+(1|liz_number),data = GM_All_YearDist)
# summary(lmepval)
# anova(lmepval)

############################
##
##      HR Overlap Analyses
##
############################
setwd("~/Dropbox/Gila Monster Data/GM_Study") 

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

