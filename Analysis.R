data <- read.csv("~/FoR/Info-Data/Salaries/Postdoc_salary_Working.csv")


library(dplyr)
library(tidyr)
library(ggplot2)
######################################################################
#Figure 1


temp <- na.omit(data[,c("Genni","State", "State","Institution","PostdocNum" )])
colnames(temp)<- c("Genni", "Region",   "State", "Institution","PostdocNum" )

ne <-c("NY","MA","NJ")
s <-c("FL","NC","TX","MD","VA")
mw <-c("MN","IA","MI","OH","IL", "IN")
w <-c("WA","AZ","CA","CO","UT")

temp <- as.data.frame(temp)
temp <- transform(temp, State = as.character(State))
temp <- transform(temp, Region = as.character(Region))
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

g <- ggplot(temp, aes(reorder(Institution, PostdocNum), fill = Genni)) +
  scale_fill_manual(values=c("grey50", "lightcoral", "seagreen3"))
g<- g +  geom_bar(position = "stack") + 
  facet_wrap(~Region, scales="free_x", strip.position = "bottom") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ylab("Postdoc number")+
  xlab("Complete dataset (by Institution and region)")
 g+ guides(fill=guide_legend(title="Gender"))

ggsave("Figure1.tiff", width = 8, height = 8)

######################################################################
#Figure 2

ne <-c("NY","MA","NJ")
s <-c("FL","NC","TX","MD","VA")
mw <-c("MN","IA","MI","OH","IL", "IN")
w <-c("WA","AZ","CA","CO","UT")

temp <- as.matrix(na.omit(data[,c("AdjSalary", "State", "State", "Genni")]))
colnames(temp)<- c("AdjSalary", "Region",   "State", "Genni")
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
temp <- as.data.frame(temp)
temp <- transform(temp, AdjSalary = as.character(AdjSalary))
temp <- transform(temp, AdjSalary = as.numeric(AdjSalary))

a<- which(temp["Region"]!="")
temp <-temp[a,]
a<- which(temp["Genni"]!="-")
temp <-temp[a,]

g <- ggplot(temp) 
g + geom_boxplot(aes(x=Region, y=AdjSalary, fill=Genni))+ylab("Annual salary (USD)") +
  scale_fill_manual(values=c( "lightcoral", "seagreen3")) + guides(fill=guide_legend(title="Gender"))

ggsave("Figure2.tiff", width = 8, height = 7)

#t-test

ne_F <- which(temp["Region"]=="NE" & temp["Genni"]=="F")
ne_M <- which(temp["Region"]=="NE" & temp["Genni"]=="M")
t.test(temp[ne_F,"AdjSalary"],temp[ne_M,"AdjSalary"])

w_F <- which(temp["Region"]=="W" & temp["Genni"]=="F")
w_M <- which(temp["Region"]=="W" & temp["Genni"]=="M")
t.test(temp[w_F,"AdjSalary"],temp[w_M,"AdjSalary"])

mw_F <- which(temp["Region"]=="MW" & temp["Genni"]=="F")
mw_M <- which(temp["Region"]=="MW" & temp["Genni"]=="M")
t.test(temp[mw_F,"AdjSalary"],temp[mw_M,"AdjSalary"])

s_F <- which(temp["Region"]=="S" & temp["Genni"]=="F")
s_M <- which(temp["Region"]=="S" & temp["Genni"]=="M")
t.test(temp[s_F,"AdjSalary"],temp[s_M,"AdjSalary"])


######################################################################
#Figure 3

temp <- na.omit(data[,c("AdjSalary","Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
                        "Clinical","Assistant", "Faculty")])
#a) LM
LM <- lm(AdjSalary ~ Intern + Teaching + Fellow + Associate + Scholar + Researcher + Trainee + Senior + Clinical + Assistant + Faculty, temp)

for (i in 1:dim(temp)[1]){
  for (j in 2:length(colnames(temp))){
    temp[i,j] = temp[i,1]* temp[i,j]}
}
temp[temp == 0] <- NA
library(reshape2)
t<- melt(temp[,2:12])

p <- ggplot(t,aes(x=variable, y=value)) + geom_violin(fill="grey60", width = 1.25)
p +  geom_boxplot(width=0.1, alpha=0.8,outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, outlier.color="lightskyblue4", fill = "lightskyblue3", color = "lightskyblue4") +
  xlab("Title descriptions")+ylab("Annual salary (USD)") 
ggsave("Figure3.tiff", width = 8, height = 6)

 

#b)Get min salaries
a <-na.omit(temp$Teaching)
min(a)
median(a)
mean(a)
######################################################################
#Figure 4

temp <- na.omit(data[,c("NIH_order","NIH_grants","AdjSalary")])

vector <- c()
for (j in 1:38){
  a<- which(temp$NIH_order == j)
  vector <- c(vector, sd(temp$AdjSalary[a], na.rm=T) /mean(temp$AdjSalary[a], na.rm=T) ) 
}

temp2 <- transform(temp, NIH_order= as.character(NIH_order))
temp1 <- transform(temp, NIH_grants= as.numeric(temp$NIH_grants))


p <- ggplot(temp2) #+ geom_violin(fill="grey60", width = 1.25)
p1 <- p +  geom_boxplot(aes(x=NIH_order, y=AdjSalary),width=0.8, alpha=0.8, color = "lightskyblue4",
                        outlier.size = 1, outlier.shape = 3, outlier.alpha = 0.8, outlier.color="lightskyblue4", fill = "lightskyblue3")  +ylim(c(0,100000)) + 
  xlab("Descending order of NIH award amount (2017)" ) +
  scale_x_discrete(limits=as.character(c(1:38)))+
  ylab("Annual salary (USD)") 

p2 <- ggplot()+geom_smooth(aes(x=c(1:38), y=vector), colour="grey60") + 
  ylab("Coefficient of variation") + xlab("")
#xlab("NIH award amount (2017) in descending order" ) +
scale_x_discrete(limits=as.character(c(1:38)))+ theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

require(cowplot)
plot_grid(p1, p2, labels = "AUTO", label_x=0, label_y=c(1,1.3), ncol = 1, align = 'v',rel_heights = c(7,4))

ggsave("Figure4.tiff", width = 8, height = 6) 


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

