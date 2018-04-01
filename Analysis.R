data <- read.csv("Tables/Preprocessed_dataset.csv", row.names = 1)
data <- data[which(data$AdjSalary>=23660),] #Remove entries where salary is reported as less than 23660

data <- data[data$Institution!= "University of Illinois Chicago",] # Remove as number of postdocs (69) is much lower than expected 


ne <-c("NY","MA","NJ")
s <-c("FL","NC","TX","MD","VA")
mw <-c("MN","IA","MI","OH","IL", "IN", "WI")
w <-c("WA","AZ","CA","CO","UT")

######################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
######################################################################
# A. Analysis of salaries by region

temp <- na.omit(data[,c("AdjSalary","State","State", "Institution","PostdocNum" )])
colnames(temp)<- c("AdjSalary", "State", "Region", "Institution","PostdocNum" )

temp <- as.data.frame(temp)
temp <- transform(temp, State = as.character(State))
temp <- transform(temp, Region = as.character(Region))

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

#Aux Figure
options(scipen=5)
tiff("Figures/AuxFigure1.tiff")
boxplot(temp$AdjSalary[which(temp$Region=="MW")], temp$AdjSalary[which(temp$Region=="NE")],
        temp$AdjSalary[which(temp$Region=="S")], temp$AdjSalary[which(temp$Region=="W")],
        pch=20, names = c("MW","NE","S","W"), xlab="Region", ylab="Salaries" )
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# B. STEM non-STEM
temp <-  na.omit(data[,c("Institution","AdjSalary", "Department" , "State")])
temp <- temp[which(temp$Department!=""),]

stem <- read.csv("Tables/STEM.csv", col.names=c("Department","is_stem"))

temp$stem <- ""
  for (i in 1:dim(temp)[1]){
    if (temp$Department[i] %in% stem$Department[which(stem$is_stem=="Y")]){
      temp$stem[i] <- "Yes"
    } else if (temp$Department[i] %in% stem$Department[which(stem$is_stem=="N")]){
      temp$stem[i] <- "No"
    }else{
      temp$stem[i] <- "-"
    }
  }

universities <- intersect(temp$Institution[which(temp$stem=="No")],
                          temp$Institution[which(temp$stem=="Yes")])

index<-c()
for (university in universities){ 
  index <- c(index, which(temp$Institution == university))}
temp <- temp[index,]

options(scipen=5)
tiff("Figures/Supplementary Figure 2A.tiff")
hist(temp$AdjSalary[which(temp$stem=="No")], breaks=10, col="blue4", 
     ylim=c(0,100), xlab="Postdoc salaries (USD)", main="")
dev.off()

temp <- as.data.frame(temp)
temp <- transform(temp, State = as.character(State))
temp <- transform(temp, Region = as.character(Region))

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

p <- ggplot(temp) 
p + geom_boxplot(aes(x=Institution, y=AdjSalary, fill=stem)) +
  ylab("Annual salary (USD)") +
  scale_fill_manual(values=c( "grey60","lightblue", "coral")) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  facet_wrap(~Region, scales="free_x", strip.position = "bottom")

ggsave("Figures/Supplementary Figure 2B.tiff", width = 8, height = 6) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# C. Compare male/female salaries

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

#fix number of postdocs for the purpose of ordering the plot
#(postdocs with salaries below the national minimum were eliminated, but still counted in the original data)

institutions <- unique(temp$Institution)
for (ins in institutions){
  a <- which(temp$Institution==ins)
  temp$PostdocNum[a] <- length(a)
  }

#Figure 2

g <- ggplot(temp, aes(reorder(Institution, PostdocNum), fill = Genni)) +
  scale_fill_manual(values=c("grey50", "lightcoral", "seagreen3"))
g<- g +  geom_bar(position = "stack") + 
  facet_wrap(~Region, scales="free_x", strip.position = "bottom") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ylab("Postdoc number")+
  xlab("Complete dataset (by Institution and region)")
 #g+ guides(fill=guide_legend(title="Gender"))

ggsave("Figures/Figure2.tiff", width = 8, height = 8)




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

ggsave("Figures/Figure3.tiff", width = 8, height = 7)

#t-test

#National (no regional divide)
W <- which(temp["Genni"]=="F")
M <- which(temp["Genni"]=="M")
t.test(temp[W,"AdjSalary"],temp[M,"AdjSalary"])

'''		Welch Two Sample t-test

data:  temp[W, "AdjSalary"] and temp[M, "AdjSalary"]
t = -3.7716, df = 4644.6, p-value = 0.0001642
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1307.7340  -413.1981
sample estimates:
mean of x mean of y 
48587.42  49447.89 '''

#Northeast only
ne_F <- which(temp["Region"]=="NE" & temp["Genni"]=="F")
ne_M <- which(temp["Region"]=="NE" & temp["Genni"]=="M")
t.test(temp[ne_F,"AdjSalary"],temp[ne_M,"AdjSalary"])

'''		Welch Two Sample t-test

data:  temp[ne_F, "AdjSalary"] and temp[ne_M, "AdjSalary"]
t = -3.0959, df = 628.52, p-value = 0.002049
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-2792.6471  -624.8997
sample estimates:
mean of x mean of y 
46262.53  47971.30  '''

#West only
w_F <- which(temp["Region"]=="W" & temp["Genni"]=="F")
w_M <- which(temp["Region"]=="W" & temp["Genni"]=="M")
t.test(temp[w_F,"AdjSalary"],temp[w_M,"AdjSalary"])

''' 	Welch Two Sample t-test

data:  temp[w_F, "AdjSalary"] and temp[w_M, "AdjSalary"]
t = -0.56349, df = 817.89, p-value = 0.5733
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1736.7773   962.0168
sample estimates:
mean of x mean of y 
54988.60  55375.98  '''

#Midwest only
mw_F <- which(temp["Region"]=="MW" & temp["Genni"]=="F")
mw_M <- which(temp["Region"]=="MW" & temp["Genni"]=="M")
t.test(temp[mw_F,"AdjSalary"],temp[mw_M,"AdjSalary"])

'''		Welch Two Sample t-test

data:  temp[mw_F, "AdjSalary"] and temp[mw_M, "AdjSalary"]
t = -1.9012, df = 1996.7, p-value = 0.05742
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
-1119.00028    17.37373
sample estimates:
mean of x mean of y 
47815.46  48366.28  '''

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#D. Estimate the effect of different postdoc title descriptors in salary

temp <- na.omit(data[,c("AdjSalary","Institution", "Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
                        "Clinical","Assistant", "Faculty")])
titles <- c("Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
           "Clinical","Assistant", "Faculty")

#Print-out # of postdocs per title
for (title in titles){
  print(title)
  print(sum(temp[,title]))
}

#Print-out # of institutions using the title
for (title in titles){
  print(title)
  print(length(unique(temp$Institution[which(temp[,title]==1)])))
}

LM <- lm(AdjSalary ~ Intern + Teaching + Fellow + Associate + Scholar + Researcher + Trainee + Senior + Clinical + Assistant + Faculty, temp)

summary(LM)

'''Call:
lm(formula = AdjSalary ~ Intern + Teaching + Fellow + Associate + 
Scholar + Researcher + Trainee + Senior + Clinical + Assistant + 
Faculty, data = temp)

Residuals:
Min     1Q Median     3Q    Max 
-25280  -4018   -926   2133  64307 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  48104.2      217.2 221.442  < 2e-16 ***
Intern       -9621.4      416.3 -23.114  < 2e-16 ***
Teaching       701.2     1818.2   0.386  0.69976    
Fellow         298.1      232.9   1.280  0.20061    
Associate      914.0      217.2   4.208 2.60e-05 ***
Scholar        513.2      337.0   1.523  0.12780    
Researcher    -285.7      140.4  -2.035  0.04190 *  
Trainee       1608.7      435.3   3.695  0.00022 ***
Senior        5468.3      317.8  17.206  < 2e-16 ***
Clinical      9231.2     2096.4   4.403 1.07e-05 ***
Assistant    -2525.6     2804.7  -0.900  0.36788    
Faculty      26767.0     3414.8   7.839 4.89e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7607 on 13911 degrees of freedom
Multiple R-squared:  0.08917,	Adjusted R-squared:  0.08845 
F-statistic: 123.8 on 11 and 13911 DF,  p-value: < 2.2e-16'''

#Figure 4
for (i in 1:dim(temp)[1]){
  for (j in 3:dim(temp)[2]){
    temp[i,j] = temp[i,1]* temp[i,j]}
} 

temp[temp == 0] <- NA
library(reshape2)
t<- melt(temp[,3:13])

options(scipen=5)
p <- ggplot(t,aes(x=variable, y=value)) + geom_violin(fill="grey60", width = 1.25)
p +  geom_boxplot(width=0.1, alpha=0.8,outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, outlier.color="lightskyblue4", fill = "lightskyblue3", color = "lightskyblue4") +
  xlab("Title descriptions")+ylab("Annual salary (USD)") 

ggsave("Figures/Figure4.tiff", width = 8, height = 6)

 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#E. Explore the effect of institutional funding to postdoc salaries
#NIH

#create summary Table 
temp <- na.omit(data[,c("NIH_order","NIH_grants", "PostdocNum")])
temp <- unique(temp)

NIH_total <- sum(temp$NIH_grants)
temp$pct_in_set <- round((temp$NIH_grants/NIH_total)*100,2)
temp <- temp[order(temp$NIH_order),]
temp <- cbind(temp$NIH_order,temp$pct_in_set,temp$PostdocNum)
colnames(temp)<-c("Order","pct_NIH_USD","no_postdocs")

tiff("Figures/Aux_Figure 4.tiff")
plot(temp[,2],temp[,3],pch=20, ylab= "Postdoc number", xlab="% NIH $ in set")
abline(lm(temp[,3]~temp[,2]), lty=2)
dev.off()

cor.test(temp[,2], temp[,3])

# Pearson's product-moment correlation
# 
# data:  temp[, 2] and temp[, 3]
# t = 7.0242, df = 43, p-value = 0.00000001187
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.5569473 0.8435208
# sample estimates:
# cor 
# 0.7309787 

write.csv(temp, file="Figures/TableNIH.csv" )
#Figure 5

# i. Boxplots
temp <- na.omit(data[,c("NIH_order","NIH_grants","AdjSalary")])
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
ggsave("Figures/Figure5.tiff", plot = p, width = 8, height = 6) 


#Supplementary Figure 3 : NSF

temp <- na.omit(data[,c("NSF_order","NSF","AdjSalary")])

# i. Boxplots
temp_char <- transform(temp, NSF_order= as.character(NSF_order))

p1 <- ggplot(temp_char) +  
  geom_boxplot(aes(x=NSF_order, y=AdjSalary),width=0.8, alpha=0.8, color = "lightskyblue4",
               outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, 
               outlier.color="lightskyblue4", fill = "lightskyblue3")  +
  ylim(c(0,100000)) + 
  xlab("Descending order of NSF R&D award amount (2017)" ) +
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
ggsave("Figures/Supplementary Figure3 .tiff", plot = p, width = 8, height = 6) 

