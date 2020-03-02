# Load in library
library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(gridExtra)
library(ggthemes)
library(ggmap)
library(zipcode)
library(fiftystater)

# Load in Data and remove nas
#CrimeData <- read_csv("/Users/Leo/Personal/HU/ANLY 506-90 Exploratory Data Analysis/Group Project/crimedataset-clean.csv")
CrimeData <- read_csv("U:/R Projects/Crime Rate/crimedataset-clean.csv")
# CrimeData.clean <- na.omit(CrimeData)

# Descriptive Statistics
summary(CrimeData)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


# Map
data("fifty_states")
View(fifty_states)

state<-sapply(fifty_states$id,simpleCap)

fifty_states$state<-state

fifty_states$state<-state.abb[match(fifty_states$state,state.name)]

df<- CrimeData %>%
  group_by(state) %>%
  summarise (ViolentCrimesPerPop = mean(ViolentCrimesPerPop,na.rm = TRUE),
             nonViolPerPop = mean(nonViolPerPop,na.rm = TRUE))
df$state<-state.name[match(df$state,state.abb)]


df$state<-tolower(df$state)


# map_id creates the aesthetic mapping to the state name column in your data
vcr<-ggplot(df, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = ViolentCrimesPerPop), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

nvc<-ggplot(df, aes(map_id = state)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = nonViolPerPop), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

grid.arrange(vcr,nvc,nrow= 2)

# Create histogram of population
ggplot(data = CrimeData.clean,aes(x=CrimeData.clean$population))+
  geom_histogram(bins = 40,fill = "blue",color = "black",alpha = 0.2)+
  labs(x="Population",
       y="Count",
       title="Histogram of Population")+
  theme_classic()

# Create overlaid histogram for violent and non-violent crime rate
df<-na.omit(CrimeData[,c(1,145,146)])
df.melt<-melt(df,id = "communityname")


ggplot(data = df.melt,aes(x=df.melt$value,fill = variable,color = variable))+
  geom_histogram(aes(y=..density..),alpha = 0.5,position = "identity",bins = 30)+
  geom_density(alpha = 0.2)+
  labs(x="Crime per Population",
       y="Density",
       title="Distribution of Crime per Population")+
  scale_fill_discrete(name="",labels=c("Violent Crime per Population", "Non-violent Crime per Population"))+
  guides(colour=FALSE)+
  theme_classic()+
  theme(legend.position = c(0.85, 0.85))



#boxplot to compare overall violent crime per population and non violent crime per population
ggplot(df.melt,aes(x=variable,y=value,color = variable))+
  geom_point(data = df.melt,position = position_jitter(width = 0.05))+
  geom_boxplot(outlier.colour = NA,position = position_dodge(width=0.05))+
  scale_color_discrete(name="",
                      labels=c("Violent Crime per Population", "Non-vilent Crime per Population"))+
  theme_classic()+
  labs(x="",
       y="Value",
       title="Boxplot of Violent Crime per Population and Non-vilent Crime per Population")+
  scale_x_discrete(labels = c("Violent Crime per Population","Non-violent Crime per Population"))+
  theme(legend.position = "bottom")


#boxplot to compare the occcurance of different crime types
df<-CrimeData.clean[,c(1,129,131,133,135,137,139,141,143)]
df.melt<-melt(df,id = "communityname")

ggplot(df.melt,aes(x=variable,y=value,color=variable))+
  geom_boxplot()+
  scale_color_discrete(name="",
                       labels=c("Murders", "Rapes","Robberies","Assaults","Burglaries","Larcenies","Auto Theft","Arsons"))+
  theme_classic()+
  labs(x="",
       y="Value",
       title="")+
  scale_x_discrete(labels = c("Murders", "Rapes","Robberies","Assaults","Burglaries","Larcenies","Auto Theft","Arsons"))+
  theme(legend.position = "bottom")



#histogram to compare the distribution of the occcurance of different crime types
ggplot(df.melt,aes(x = df.melt$value))+
  geom_histogram(bins = 60)+
  facet_wrap(~variable,ncol = 4)

#anova test to check if the mean crime occurences are the same among different crime types
cmetpe.aov<-aov(data = df.melt,df.melt$value~df.melt$variable)
summary(cmetpe.aov)
TukeyHSD(cmetpe.aov)

#boxplot to compare violent crime rate and non violent crime rate by state
df<-na.omit(CrimeData[,c(2,145,146)])
df.melt<-melt(df,id = "state")
ggplot(data = df.melt,aes(x = df.melt$state,y = df.melt$value,fill = variable))+
  geom_boxplot()

#anova test to check if the mean violent crime rate and non violent crime rate for all the states 
#are statistically significantly different
df<- na.omit(CrimeData[,c(2,145,146)])
vcr.aov<-aov(data = df,df$ViolentCrimesPerPop~df$state)
summary(vcr.aov)
TukeyHSD(vcr.aov)

ncr.aov<-aov(data = df,df$nonViolPerPop~df$state)
summary(ncr.aov)
TukeyHSD(ncr.aov)


df<-subset(CrimeData.clean,CrimeData.clean$population<1500000)
ggplot(data = df,aes(x = df$population))+
  geom_histogram()

#Scatter Plot for violent crimeperpop vs population
sp1<-ggplot(data = df,aes(x = df$population,y=df$ViolentCrimesPerPop))+
  geom_point()+
  geom_smooth(method = lm)

#Scatter Plot for violent crimeperpop vs population
sp2<-ggplot(data = df,aes(x = df$population,y=df$nonViolPerPop))+
  geom_point()+
  geom_smooth(method = lm)

grid.arrange(sp1,sp2,ncol = 2)


# Multiple linear regression using a stepwise selection of independent variables to find the best fitted model for
# factors affecting violentcrimsperpop

model.vcr<- lm(data = CrimeData.clean, ViolentCrimesPerPop~ population +householdsize +
             racepctblack +racePctWhite +racePctAsian +racePctHisp +
             medIncome +
             agePct12t21 +agePct12t29 + agePct16t24 +agePct65up+
             NumUnderPov +
             PctLess9thGrade +PctNotHSGrad +PctBSorMore +
             PctUnemployed +
             TotalPctDiv +
             NumImmig +
             NumInShelters +NumStreet +
             LemasSwornFT +LemasSwFTFieldOps +PolicAveOTWorked +PolicBudgPerPop +LemasPctPolicOnPatr+ LemasGangUnitDeploy +
             LandArea +PopDens
             )
summary(model.vcr)

step(model.vcr,direction = "both")


summary(lm(data = CrimeData.clean, ViolentCrimesPerPop~ population +householdsize +
             racePctWhite +
             agePct12t21 +
             NumUnderPov +
             PctBSorMore +
             PctUnemployed +
             NumImmig +
             NumInShelters +NumStreet +
             LemasSwornFT +LemasSwFTFieldOps
)
)

# Test interaction effect
cor(CrimeData.clean[,c(5,6,8,11,33,36,37,56,96,97,103,105)])

# Multiple linear regression using a stepwise selection of independent variables to find the best fitted model for
# factors affecting nonviolentcrimsperpop

model.nvc<- lm(data = CrimeData.clean, nonViolPerPop~ population +householdsize +
                 racepctblack +racePctWhite +racePctAsian +racePctHisp +
                 medIncome +
                 agePct12t21 +agePct12t29 + agePct16t24 +agePct65up+
                 NumUnderPov +
                 PctLess9thGrade +PctNotHSGrad +PctBSorMore +
                 PctUnemployed +
                 TotalPctDiv +
                 NumImmig +
                 NumInShelters +NumStreet +
                 LemasSwornFT +LemasSwFTFieldOps +PolicAveOTWorked +PolicBudgPerPop +LemasPctPolicOnPatr+ LemasGangUnitDeploy +
                 LandArea +PopDens
)
summary(model.nvc)

step(model.nvc,direction = "both")

summary(lm(data = CrimeData.clean, nonViolPerPop~ population +
             racePctWhite + racePctHisp +
             agePct12t21 + agePct12t29 + agePct16t24 +agePct65up +
             PctLess9thGrade +PctNotHSGrad +PctBSorMore +
             TotalPctDiv +
             NumInShelters +
             LemasSwFTFieldOps +
             PolicOperBudg +
             LandArea +
             PopDens
)
)
