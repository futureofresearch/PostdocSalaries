data <- read.csv("~/FoR/Info-Data/Salaries/Postdoc_salary_Working.csv")

######################################################################
library(dplyr)
library(tidyr)
library(ggplot2)

#data <- tbl_df(data)

#Boxplots
  
  #1. Gender - salary
temp <- na.omit(data[,c("Genni","AdjSalary")])
for (i in 1:dim(temp)[1]){
  if (temp[i,"Genni"] == "False"){
    temp[i,"Genni"] <- "-"
  }
}

p <- ggplot(temp) 
p + geom_boxplot(aes(x=Genni, y=AdjSalary))

#boxplot(data["AdjSalary"], )

  #2. Urban/rural - salary

      #a) Categorical
temp <- na.omit(data[,c("Metro_code","AdjSalary")])
p <- ggplot(temp) 
p + geom_boxplot(aes(x=Metro_code, y=AdjSalary))

      #b) Percent urban
temp <- na.omit(data[,c("pct_urban","AdjSalary", "Institution")])
p <- ggplot(temp) + 
  theme(legend.position="none") +
  geom_point(data = temp, mapping = aes(x=pct_urban, y=AdjSalary, color=Institution), size=0.4) +
  geom_smooth(data = temp, mapping = aes(x=pct_urban, y=AdjSalary))
p

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


    #a) Together with gender

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

g <- ggplot(temp) 
g + geom_boxplot(aes(x=Region, y=AdjSalary, fill=Genni))

    #3. Title word - salary
temp <- na.omit(data[,c("AdjSalary","Professor","Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
                        "Clinical","Assistant", "Faculty")])
for (i in 1:dim(temp)[1]){
  for (j in 2:length(colnames(temp))){
    temp[i,j] = temp[i,1]* temp[i,j]}
}
temp[temp == 0] <- NA
library(reshape2)
t<- melt(temp[,2:13])

p <- ggplot(t) 
p + geom_boxplot(aes(x=variable, y=value))

        #a) Add institution information
temp <- na.omit(data[,c("Institution","Professor","Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
                        "Clinical","Assistant", "Faculty")])
temp[temp == 0] <- NA
temp<- melt(temp,id.vars = "Institution", measure.vars = colnames(temp[,2:13]), na.rm=T)
temp <- transform(temp, Institution = as.character(Institution))
temp <- transform(temp, variable = as.character(variable))

val <- unique(temp["variable"])[,1]
for (i in 1:length(val)){
  a<-which(temp["variable"]==val[i])
  print(val[i])
  print(unique(temp[a,1]))
}
        #b) LM
LM <- lm(AdjSalary ~ Professor+Intern + Teaching + Fellow + Associate + Scholar + Researcher + Trainee + Senior + Clinical + Assistant + Faculty, temp)


        #c) Title gender for salary
temp <- na.omit(data[,c("AdjSalary", "Genni","Professor","Intern","Teaching","Fellow","Associate","Scholar","Researcher", "Trainee","Senior",
                        "Clinical","Assistant", "Faculty")])
for (i in 1:dim(temp)[1]){
  if (temp[i,"Genni"] == "False"){
    temp[i,"Genni"] <- "-"
  }
  for (j in 3:length(colnames(temp))){
    temp[i,j] = temp[i,1]* temp[i,j]}
}
temp[temp == 0] <- NA
temp<- melt(temp,id.vars = "Genni", measure.vars = colnames(temp[,3:14]), na.rm=T)

g <- ggplot(temp) 
g + geom_boxplot(aes(x=variable, y=value, fill=Genni))

  #4. NIH order - salary
temp <- na.omit(data[,c("NIH_order","AdjSalary")])
temp <- transform(temp, NIH_order= as.character(NIH_order))
p <- ggplot(temp) 
p + geom_boxplot(aes(x=NIH_order, y=AdjSalary)) + 
  scale_x_discrete(limits=as.character(c(1:38)))#helps with proper order

    #a) step detection
library("tsoutliers")
temp.ts<- ts(temp$AdjSalary,frequency=1)
temp.ts.outliers <- tso(temp.ts)
temp.ts.outliers


  #5. Postoc_student ratio - salary
temp <- na.omit(data[,c("postdocs_per_1000stdduents","AdjSalary")])
p <- ggplot(temp) + 
  geom_point(data = temp, mapping = aes(x=postdocs_per_1000stdduents, y=AdjSalary), size=0.4) +
  geom_smooth(data = temp, mapping = aes(x=postdocs_per_1000stdduents, y=AdjSalary))+
  xlim(0, 15)
p

#5a. Postoc_student ratio - salary
temp <- na.omit(data[,c("no_students","AdjSalary")])
p <- ggplot(temp) + 
  geom_point(data = temp, mapping = aes(x=no_students, y=AdjSalary), size=0.4) +
  geom_smooth(data = temp, mapping = aes(x=no_students, y=AdjSalary))+
  xlim(0, 15)
p

#5a. Postoc number - salary
temp <- na.omit(data[,c("PostdocNum","AdjSalary")])
p <- ggplot(temp) + 
  geom_point(data = temp, mapping = aes(x=PostdocNum, y=AdjSalary), size=0.4) +
  geom_smooth(data = temp, mapping = aes(x=PostdocNum, y=AdjSalary))+
  xlim(0, 15)
p

  #6. Ethnicity word - salary
temp <- na.omit(data[,c("AdjSalary",'ROMANIAN', 'ENGLISH', 'BALTIC', 'HISPANIC', 'DUTCH', 'GERMAN',
                        'NORDIC', 'INDIAN', 'VIETNAMESE', 'HUNGARIAN', 'JAPANESE', 'ISRAELI',
                        'SLAV', 'CHINESE', 'TURKISH', 'THAI', 'MONGOLIAN', 'AFRICAN', 'GREEK',
                        'FRENCH', 'KOREAN', 'ITALIAN', 'CARIBBEAN', 'INDONESIAN', 'ARAB')])
for (i in 1:dim(temp)[1]){
  for (j in 2:length(colnames(temp))){
    temp[i,j] = temp[i,1]* temp[i,j]}
}
temp[temp == 0] <- NA
library(reshape2)
t<- melt(temp[,2:26])

p <- ggplot(t) 
p + geom_boxplot(aes(x=variable, y=value)) + 
  scale_x_discrete(limits=order)#helps with proper order



