data <- read.csv("Tables/Preprocessed_dataset.csv", row.names = 1)
data <- data[data$AdjSalary!=0,] #Remove 71 entries where salary is reported as zero, which is impossible (?)

data <- data[data$Institution!= "University of Illinois Chicago",] # Remove as number of postdocs (69) is much lower than expected 
#With Chicago eliminated, I need to re-calculate the NIH/NSF award order

ne <-c("NY","MA","NJ")
s <-c("FL","NC","TX","MD","VA")
mw <-c("MN","IA","MI","OH","IL", "IN")
w <-c("WA","AZ","CA","CO","UT")

######################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
######################################################################

# A. Compare male/female salaries

temp <- na.omit(data[,c("AdjSalary","Genni","State","State", "Institution","PostdocNum" )])
colnames(temp)<- c("AdjSalary","Genni", "State", "Region", "Institution","PostdocNum" )

temp <- as.data.frame(temp)
temp <- transform(temp, State = as.character(State))
temp <- transform(temp, Region = as.character(Region))
temp <- transform(temp, AdjSalary = as.character(AdjSalary))
temp <- transform(temp, AdjSalary = as.numeric(AdjSalary))

for (i in 1:dim(temp)[1]){
  if (temp[i,"Genni"] == "False"){
    temp[i,"Genni"] <- "-"
  }
  if (length(which( ne == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "NE"
  }else if (length(which( s == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "S"
  }else if(length(which( mw == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "MW"
  }else if(length(which( w == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "W"
  }
}

#Figure 1

g <- ggplot(temp, aes(reorder(Institution, PostdocNum), fill = Genni)) +
  scale_fill_manual(values=c("grey50", "lightcoral", "seagreen3"))
g<- g +  geom_bar(position = "stack") + 
  facet_wrap(~Region, scales="free_x", strip.position = "bottom") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ylab("Postdoc number")+
  xlab("Complete dataset (by Institution and region)")
 #g+ guides(fill=guide_legend(title="Gender"))

ggsave("Figures/Figure1.tiff", width = 8, height = 8)


#Figure 2

#Include the Genni unassigned (not in paper)
g <- ggplot(temp) 
g + geom_boxplot(aes(x="All", y=AdjSalary, fill=Genni)) +
  geom_boxplot(aes(x=Region, y=AdjSalary, fill=Genni)) +
  ylab("Annual salary (USD)")  +
  xlab("Region") +
  scale_fill_manual(values=c( "grey50", "lightcoral", "seagreen3")) +
  guides(fill=guide_legend(title="Genni")) 

ggsave("Figures/Aux_Figure2.tiff", width = 8, height = 7)

#Only use Genni assignments (in paper)
a<- which(temp["Genni"]!="-")
tempa <-temp[a,]

g <- ggplot(tempa) 
g + geom_boxplot(aes(x="All", y=AdjSalary, fill=Genni)) +
  geom_boxplot(aes(x=Region, y=AdjSalary, fill=Genni)) +
  ylab("Annual salary (USD)")  +
  xlab("Region") +
  scale_fill_manual(values=c( "lightcoral", "seagreen3")) +
  guides(fill=guide_legend(title="Gender")) 

ggsave("Figures/Figure2.tiff", width = 8, height = 7)

#t-test

#National (no regional divide)
W <- which(temp["Genni"]=="F")
M <- which(temp["Genni"]=="M")
t.test(temp[W,"AdjSalary"],temp[M,"AdjSalary"])

'''	Welch Two Sample t-test

data:  temp[W, "AdjSalary"] and temp[M, "AdjSalary"]
t = -3.3125, df = 4334.1, p-value = 0.0009322
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1383.2070  -354.6563
sample estimates:
mean of x mean of y 
48098.13  48967.07 '''

#Northeast only
ne_F <- which(temp["Region"]=="NE" & temp["Genni"]=="F")
ne_M <- which(temp["Region"]=="NE" & temp["Genni"]=="M")
t.test(temp[ne_F,"AdjSalary"],temp[ne_M,"AdjSalary"])

'''	Welch Two Sample t-test

data:  temp[ne_F, "AdjSalary"] and temp[ne_M, "AdjSalary"]
t = -2.9103, df = 633.93, p-value = 0.003737
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-2754.0795  -534.8913
sample estimates:
mean of x mean of y 
46180.74  47825.23 '''

#West only
w_F <- which(temp["Region"]=="W" & temp["Genni"]=="F")
w_M <- which(temp["Region"]=="W" & temp["Genni"]=="M")
t.test(temp[w_F,"AdjSalary"],temp[w_M,"AdjSalary"])

'''	Welch Two Sample t-test

data:  temp[w_F, "AdjSalary"] and temp[w_M, "AdjSalary"]
t = -0.56349, df = 817.89, p-value = 0.5733
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1736.7773   962.0168
sample estimates:
mean of x mean of y 
54988.60  55375.98 '''

#Midwest only
mw_F <- which(temp["Region"]=="MW" & temp["Genni"]=="F")
mw_M <- which(temp["Region"]=="MW" & temp["Genni"]=="M")
t.test(temp[mw_F,"AdjSalary"],temp[mw_M,"AdjSalary"])

'''	Welch Two Sample t-test

data:  temp[mw_F, "AdjSalary"] and temp[mw_M, "AdjSalary"]
t = -1.3741, df = 1628.3, p-value = 0.1696
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1280.572   225.500
sample estimates:
mean of x mean of y 
46566.54  47094.08 '''

#South only
s_F <- which(temp["Region"]=="S" & temp["Genni"]=="F")
s_M <- which(temp["Region"]=="S" & temp["Genni"]=="M")
t.test(temp[s_F,"AdjSalary"],temp[s_M,"AdjSalary"])

'''	Welch Two Sample t-test

data:  temp[s_F, "AdjSalary"] and temp[s_M, "AdjSalary"]
t = -4.6206, df = 1331.4, p-value = 4.198e-06
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-2768.742 -1118.387
sample estimates:
mean of x mean of y 
46303.28  48246.84 '''

######################################################################

#B. Estimate the effect of different postdoc title descriptors in salary

temp <- na.omit(data[,c("AdjSalary","Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
                        "Clinical","Assistant", "Faculty")])

LM <- lm(AdjSalary ~ Intern + Teaching + Fellow + Associate + Scholar + Researcher + Trainee + Senior + Clinical + Assistant + Faculty, temp)

summary(LM)

'''Call:
lm(formula = AdjSalary ~ Intern + Teaching + Fellow + Associate + 
Scholar + Researcher + Trainee + Senior + Clinical + Assistant + 
Faculty, data = temp)

Residuals:
Min     1Q Median     3Q    Max 
-47238  -3978   -509   2608  64307 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  48019.1      248.8 192.985  < 2e-16 ***
Intern      -17445.9      406.8 -42.887  < 2e-16 ***
Teaching       812.0     2062.0   0.394  0.69372    
Fellow        -361.1      265.3  -1.361  0.17359    
Associate      684.7      250.5   2.733  0.00628 ** 
Scholar        487.5      382.3   1.275  0.20235    
Researcher    -266.0      162.8  -1.633  0.10239    
Trainee       1736.5      551.1   3.151  0.00163 ** 
Senior        6151.3      364.9  16.859  < 2e-16 ***
Clinical      9689.7     2377.3   4.076 4.61e-05 ***
Assistant    -2691.6     3180.6  -0.846  0.39743    
Faculty      26891.3     3872.3   6.944 3.97e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8627 on 13494 degrees of freedom
Multiple R-squared:  0.1933,	Adjusted R-squared:  0.1927 
F-statistic:   294 on 11 and 13494 DF,  p-value: < 2.2e-16'''

#Figure 3
for (i in 1:dim(temp)[1]){
  for (j in 2:dim(temp)[2]){
    temp[i,j] = temp[i,1]* temp[i,j]}
} 

temp[temp == 0] <- NA
library(reshape2)
t<- melt(temp[,2:12])

p <- ggplot(t,aes(x=variable, y=value)) + geom_violin(fill="grey60", width = 1.25)
p +  geom_boxplot(width=0.1, alpha=0.8,outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, outlier.color="lightskyblue4", fill = "lightskyblue3", color = "lightskyblue4") +
  xlab("Title descriptions")+ylab("Annual salary (USD)") 

ggsave("Figures/Figure3.tiff", width = 8, height = 6)

 
######################################################################
#C. Explore the effect of institutional funding to postdoc salaries

#Figure 4

#NIH
temp <- na.omit(data[,c("NIH_order","NIH_grants","AdjSalary")])

# i. Boxplots
temp_char <- transform(temp, NIH_order= as.character(NIH_order))

p1 <- ggplot(temp_char) +  
  geom_boxplot(aes(x=NIH_order, y=AdjSalary),width=0.8, alpha=0.8, color = "lightskyblue4",
                        outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, 
                        outlier.color="lightskyblue4", fill = "lightskyblue3")  +
  ylim(c(0,100000)) + 
  xlab("Descending order of NIH award amount (2017)" ) +
  scale_x_discrete(limits=as.character(c(1:range(temp[,"NIH_order"])[2]))) +
  theme(axis.text.x = element_text(size=9)) +
  ylab("Annual salary (USD)") 

# ii. CV loess
cv <- c()
for (j in 1:range(temp[,"NIH_order"])[2]){
  a<- which(temp$NIH_order == j)
  cv <- c(cv, sd(temp$AdjSalary[a], na.rm=T) /mean(temp$AdjSalary[a], na.rm=T) )
  }

p2 <- ggplot()+geom_smooth(aes(x=c(1:range(temp[,"NIH_order"])[2]), y=cv), colour="grey60") + 
  ylab("Coefficient of variation") + xlab("") +
  scale_x_discrete(limits=as.character(c(1:range(temp[,"NIH_order"])[2]))) +
  theme(axis.text.x = element_text(size=9))

require(cowplot)
p <- plot_grid(p1, p2, align = 'v',rel_heights = c(7,4), nrow=2)
ggsave("Figure4.tiff", plot = p, width = 8, height = 6) 


#Figure 4 auxiliary : NSF

temp <- na.omit(data[,c("NSF_order","NSF","AdjSalary")])

# i. Boxplots
temp_char <- transform(temp, NSF_order= as.character(NSF_order))

p1 <- ggplot(temp_char) +  
  geom_boxplot(aes(x=NSF_order, y=AdjSalary),width=0.8, alpha=0.8, color = "lightskyblue4",
               outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, 
               outlier.color="lightskyblue4", fill = "lightskyblue3")  +
  ylim(c(0,100000)) + 
  xlab("Descending order of NSF award amount (2017)" ) +
  scale_x_discrete(limits=as.character(c(1:range(temp[,"NSF_order"])[2]))) +
  theme(axis.text.x = element_text(size=9)) +
  ylab("Annual salary (USD)") 

# ii. CV loess
cv <- c()
for (j in 1:range(temp[,"NSF_order"])[2]){
  a<- which(temp$NSF_order == j)
  cv <- c(cv, sd(temp$AdjSalary[a], na.rm=T) /mean(temp$AdjSalary[a], na.rm=T) )
}

p2 <- ggplot()+geom_smooth(aes(x=c(1:range(temp[,"NSF_order"])[2]), y=cv), colour="grey60") + 
  ylab("Coefficient of variation") + xlab("") +
  scale_x_discrete(limits=as.character(c(1:range(temp[,"NSF_order"])[2]))) +
  theme(axis.text.x = element_text(size=9))

p <- plot_grid(p1, p2, align = 'v',rel_heights = c(7,4), nrow=2)
ggsave("Figure4_Aux1 .tiff", plot = p, width = 8, height = 6) 

#Figure 4 auxiliary : NSF + NIH

temp <- na.omit(data[,c("NIH_grants","NSF","AdjSalary")]) #NB some universities that may have NIH or NSF data, but not both, are not included
temp$Grants <-temp$NIH_grants + temp$NSF

# i. Boxplots
temp_char <- transform(temp, NSF_order= as.character(NSF_order))

p1 <- ggplot(temp_char) +  
  geom_boxplot(aes(x=NSF_order, y=AdjSalary),width=0.8, alpha=0.8, color = "lightskyblue4",
               outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, 
               outlier.color="lightskyblue4", fill = "lightskyblue3")  +
  ylim(c(0,100000)) + 
  xlab("Descending order of NSF award amount (2017)" ) +
  scale_x_discrete(limits=as.character(c(1:range(temp[,"NSF_order"])[2]))) +
  theme(axis.text.x = element_text(size=9)) +
  ylab("Annual salary (USD)") 

# ii. CV loess
cv <- c()
for (j in 1:range(temp[,"NSF_order"])[2]){
  a<- which(temp$NSF_order == j)
  cv <- c(cv, sd(temp$AdjSalary[a], na.rm=T) /mean(temp$AdjSalary[a], na.rm=T) )
}

p2 <- ggplot()+geom_smooth(aes(x=c(1:range(temp[,"NSF_order"])[2]), y=cv), colour="grey60") + 
  ylab("Coefficient of variation") + xlab("") +
  scale_x_discrete(limits=as.character(c(1:range(temp[,"NSF_order"])[2]))) +
  theme(axis.text.x = element_text(size=9))

p <- plot_grid(p1, p2, align = 'v',rel_heights = c(7,4), nrow=2)
ggsave("Figure4_Aux1 .tiff", plot = p, width = 8, height = 6) 



######################################################################
#Figure SUPPLEMENT 2B
#HUMANITIES VS STEM
sal <- read.csv("~/FoR/Info-Data/Salaries/salary_data_(non-stem_annot).csv")
a <- which(sal$NON.STEM == "Y")
universities <- unique(sal$Institution[a])
index<-c()
for (university in universities){ 
  index <- c(index, which(sal$Institution == university))}

tempS <- sal[index,]
tempS <- transform(tempS, AdjSalary = as.numeric(AdjSalary))
tempS <- transform(tempS, STEM = as.character(STEM))

ne <-c("NY","MA","NJ")
s <-c("FL","NC","TX","MD","VA")
mw <-c("MN","IA","MI","OH","IL", "IN")
w <-c("WA","AZ","CA","CO","UT")

tempS$Region <- "-"
tempS <- as.data.frame(tempS)
tempS <- transform(tempS, State = as.character(State))
tempS <- transform(tempS, Region = as.character(Region))
for (i in 1:dim(tempS)[1]){
  if (length(which( ne == tempS[i,"State"])) > 0){
    tempS[i,"Region"] <- "NE"
  }else if (length(which( s == tempS[i,"State"])) > 0){
    tempS[i,"Region"] <- "S"
  }else if(length(which( mw == tempS[i,"State"])) > 0){
    tempS[i,"Region"] <- "MW"
  }else if(length(which( w == tempS[i,"State"])) > 0){
    tempS[i,"Region"] <- "W"
  }
}

p <- ggplot(tempS) 
p + geom_boxplot(aes(x=Institution, y=AdjSalary, fill=STEM)) +
                   ylab("Annual salary (USD)") +
                   scale_fill_manual(values=c( "grey60","lightblue", "coral")) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  facet_wrap(~Region, scales="free_x", strip.position = "bottom")
                 
ggsave("Suppl2B_stem.tiff", width = 8, height = 6) 

#Figure 1 Supplement
#3. US cultural geography - salary

ne <-c("NY","MA","NJ")
s <-c("FL","NC","TX","MD","VA")
mw <-c("MN","IA","MI","OH","IL", "IN")
w <-c("WA","AZ","CA","CO","UT")

temp <- as.matrix(na.omit(data[,c("AdjSalary", "State", "State")]))
colnames(temp)<- c("AdjSalary", "Region",   "State")
for (i in 1:dim(temp)[1]){
  if (length(which( ne == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "NE"
  }else if (length(which( s == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "S"
  }else if(length(which( mw == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "MW"
  }else if(length(which( w == temp[i,"State"])) > 0){
    temp[i,"Region"] <- "W"
  }
}
temp <- as.data.frame(temp)
temp <- transform(temp, AdjSalary = as.character(AdjSalary))
temp <- transform(temp, AdjSalary = as.numeric(AdjSalary))

g <- ggplot(temp) 
g + geom_boxplot(aes(x=Region, y=AdjSalary))

ggsave("Suppl1_stem.tiff", width = 8, height = 6) 

