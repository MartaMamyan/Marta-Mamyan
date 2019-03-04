library('ggplot2')
library('magrittr')
library('RCurl')
library('scales')
library('RCurl')

x <- getURL("https://raw.githubusercontent.com/MartaMamyan/Marta-Mamyan/master/purpose.csv")
data <- read.csv(text = x)
head(dat)

data <- na.omit(dat) #drop missing values

summary(data$Period)
#Our data includes monthly data in years from 2011 to 2018

#Lets visually see from which regions Taiwan gets most of its tourists
data["total"]=(data$Business+data$Pleasure+data$Visit.Relatives+data$Conference+data$Study+data$Exhibition+
                 data$Medical.Treatment+data$Others+data$Unstated)/1000
data
b=ggplot(data, aes(Region, total, fill=Region, label = total))+geom_bar(stat="identity")+
  geom_text(label=data$total, size = 3, position = position_stack(vjust = 0.5))+theme_classic()+
  ggtitle("Visits to Taiwan based on Region (in Tousands)")
b

#We see that most of the visitors to Taiwan are from Asia (about 60mln visitors in 7 years), the second comes America, which is considerably lower than Asia


#Now we will explore the main reasons for the visits
sum(data$Business)
sum(data$Pleasure)
sum(data$Visit.Relatives)
sum(data$Conference)
sum(data$Study)
sum(data$Exhibition)
sum(data$Medical.Treatment)
sum(data$Others)
sum(data$Unstated)

#We see now that during the last 7 years the most pupolar reason for visiting Taiwan is Pleasure, then comes the option "Other". 
#Quite a lot of people visit Taiwan for business reseaons. Another big reason is visiting relatives, which is quite logical as Taiwan has a big diaspora
#Academic reasons such as Study and Conference have very close values, that is why I will check if there is a correlation between two academic variables

C<- ggplot(data, aes(Study, Conference))+geom_jitter(col="navy blue")+theme_classic()+
  ggtitle("Correlation between Study and Conference visits") + xlab("Study Purpose") + ylab("Conference Purpose")

C #We see no linear relationship between academic travel reasons

t.test(data$Study, data$Conference) #The t-test repeats the findings of bthe previous scatterplot with a p-value of 0.7288


C2<- ggplot(data, aes(Business, Pleasure))+geom_jitter(col="purple")+
  theme_classic()+ggtitle("Correlation of Business and Pleasure Purposes")
C2 #Here too we do not see any linear relationship



barP=ggplot(data, aes(Residence, Pleasure/1000)) + geom_bar(stat='identity') + coord_flip() + theme_classic() + facet_grid(. ~ Region) + ggtitle('Visits for Pleasure Purposes (in thousand units) for each country in each Region')
barP
#The plot shows that the most visits for pleasure purposes Taiwan gets from Asia, specifically from Mainland China, Japan and HongKong. Makao, Korean Republic and Malaysia
#Now lets repeat the same procedure for all the other purposes 

barB=ggplot(data, aes(Residence, Business/1000)) + geom_bar(stat='identity') + coord_flip() + theme_classic() + facet_grid(. ~ Region) + ggtitle('Visits for Business (in thousand units) for each country in each Region')
barB
#The barchart that appeared shows an interesting result; Although Asia is again ahead of all the other regions (especially Japan), the US get a big portion in business visits. Also, we some some activeness from Europe

barB=ggplot(data, aes(Residence, Visit.Relatives/1000)) + geom_bar(stat='identity') + coord_flip() + theme_classic() + facet_grid(. ~ Region) + ggtitle('Visits for Seeing Relativs (in thousand units) for each country in each Region')
barB
#We see where the Taiwanise diaspora is accumulated; the main country is the USA, then comes Mainland China, HongKong. Macao and Japan


theme_set(theme_classic())

Buble <- data[data$Region]

theme_set(theme_test())
g <- ggplot(data, aes(Visit.Relatives/1000, Pleasure/1000)) + 
labs(subtitle="data: How do the visits to relatives relate to visits for pleasure (in thousands)",
title="Bubble chart", x="Visits to Relatives", y="Visits for Pleasure")

g + geom_jitter(aes(col=Region, size=total)) +

geom_smooth(aes(col=Region), method="lm", se=F) 

#Here we see that the visits from Asia for these 2 categories are heteroscedastic, so there is no specific linear relationship
#Nonetheless, the visitors from the US for visiting the relatives and just for pleasure are a bit lineary correlated
#This means that if in a month there were more visits to relatives, the pleasure visits increased too. This can be related to economic situation 
#For instance if there was an increase in income of American-Taiwanise, both types of visits increase. The geographical distance may also be a cause that income fluctuations affect visiting patterns (it is more expensive to fly from the US to Taiwan)

#Let's see how the trend of visiting to Japan has changed for various purposes over time
Japan <- subset(data, Residence=="Japan") #we take only time periods from Japan
Japan  

par(mfrow=c(1,4))

plot.ts(Japan$Business, ylab='Business Visits') #The business purposes are slowing down and the variance becomes bigger
title("Business visits from Japan")

plot.ts(Japan$Study)
title(' Study visits from Japan') #We see quite constant study travel purposes from Japan to Taiwan with noticeable seasonality

plot.ts(Japan$Pleasure, ylab='Pleasure Visits') #The pleasure purposes are gradualy increasing (almost twice compared to the beginning of 2011)
title("Pleasure visits from Japan")

plot.ts(Japan$Visit.Relatives, ylab='Visits to the relatives') #The visits to the relatives have dropped drastically
title("Visiting Relatives from Japan")

