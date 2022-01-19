
# Packages & Data Loading
setwd("E:/STOCK")

library("tidyverse")

data <- read_csv("SPY.csv")

# Looking for missing variables (highly doubt)

colSums(is.na(data))

# Creating Plots for distribution & Probability

# Daily price movement
data$daily.delta <-append((data$Close-lag(data$Close))[-1],0,after=0)

############################################################################################################################################

#****** Probability Estimation ********

# **Histogram**
bin.width <- sd(data$daily.delta) * 3.5 * (nrow(data)^(-1/3))
histogram <- ggplot(data = data, aes( x=daily.delta.percent ))+geom_histogram(binwidth = bin.width, aes(y=..density..))+
  geom_density(col="red")+
  geom_vline(xintercept = c(-2.5,2.5), color="blue")+
  scale_x_continuous(limits = c(min(data$daily.delta.percent), max(data$daily.delta.percent)),
                     breaks = c(seq(-10,10,by =5),-2.5,2.5,15))+
  labs(title="Distribution of SPY's daily percentage Movement", x="Daily Percentage Movement")+theme_classic()+
  theme(plot.title=element_text(hjust=0.5))


# % of movement in [-2.5,2.5]
histogram_den <- ggplot_build(histogram)
movement.binwidth <- histogram_den$data[[1]][,c("xmin","xmax","density")]
sum(movement.binwidth[which(movement.binwidth$xmin>=-2.5 & movement.binwidth$xmax<=2.5),"density"] * bin.width)



# **Normal Distribution**

qqnorm(data$daily.delta.percent)
qqline(data$daily.delta.percent)

pnorm(2.5,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)) - pnorm(-2.5,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)) 

# ** KDE ** #
kde <- density(data$daily.delta.percent, bw=(1.06*sd(data$daily.delta.percent)*(nrow(data)^(-1/5))),
               kernel= "gaussian")
x.grid <- seq(min(data$daily.delta.percent),max(data$daily.delta.percent), by=0.0035)
kde.df <- approx(kde$x,kde$y,xout=x.grid)

plot(kde.df, xlab="Percentage Movement", ylab="Density", main="KDE of Daily SPY Movement")


# Probability of lieing between -2.5 and 2.5
sum(kde.df$y[which(kde.df$x>=-2.5 & kde.df$x<=2.5)]*0.0035)

#############################################################################################################################################

#** Modelling**

# probability of days ending with gains or loses
sum(data$daily.delta.percent >0)/nrow(data)*100 # pr daily % return >0
sum(data$daily.delta.percent <0)/nrow(data)*100 # pr daily % return <0
sum(data$daily.delta.percent ==0)/nrow(data)*100 # pr daily % return =0





## Checking by percentage movement

data$daily.delta.percent <- append( ((data$daily.delta/lag(data$Close))[-1])*100,0,after=0)

# Probability of seeing movements equal to greater than 2 sd from the mean in both directions


#***************************************************#

# Return Test

#******** 2.5% **********#

data.3yrs <- data[which(data$Date>= "2019-01-01" & data$Date <= "2021-12-31"),] 

# 5 day average return
day2.5<- data.3yrs[which(data.3yrs$daily.delta.percent>=2.5),"daily.delta.percent"] # buying day
day2.5.5d <- data.3yrs[which(data.3yrs$daily.delta.percent>=2.5)+5,"daily.delta.percent"] # holding for 5 day
mean(unlist((c(((day2.5.5d-day2.5)/(day2.5)))))) # mean return

# 10 day average return
day2.5.10d <- data.3yrs[which(data.3yrs$daily.delta.percent>=2.5)+10,"daily.delta.percent"]
mean(unlist((c(((day2.5.10d-day2.5)/(day2.5)))))) # mean return

# 20 day average return
day2.5.20d <- data.3yrs[which(data.3yrs$daily.delta.percent>=2.5)+20,"daily.delta.percent"]
mean(unlist((c(((day2.5.20d-day2.5)/(day2.5)))))) # mean return

#empirical probability
sum(data.3yrs$daily.delta.percent>=2.5)/nrow(data.3yrs)*100

# probability based on the density estimation

#hist
sum(movement.binwidth[39:74,"density"]*bin.width) *100

#normal

(1-pnorm(2.5,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)))*100

#kde
sum(kde.df$y[which(kde.df$x>=2.5)]*0.0035)*100

#******** 3.0% **********#
# 5 day average return
day3.0<- data.3yrs[which(data.3yrs$daily.delta.percent>=3.0),"daily.delta.percent"] # buying day
day3.0.5d <- data.3yrs[which(data.3yrs$daily.delta.percent>=3.0)+5,"daily.delta.percent"] # holding for 5 day
return.5d3per <- unlist((c(((day3.0.5d-day3.0)/(day3.0))))) # returns

# 10 day average return
day3.0.10d <- data.3yrs[which(data.3yrs$daily.delta.percent>=3.0)+10,"daily.delta.percent"]
mean(unlist((c(((day3.0.10d-day3.0)/(day3.0)))))) # mean return

# 20 day average return
day3.0.20d <- data.3yrs[which(data.3yrs$daily.delta.percent>=3.0)+20,"daily.delta.percent"]
mean(unlist((c(((day3.0.20d-day3.0)/(day3.0)))))) # mean return

#empirical probability
sum(data.3yrs$daily.delta.percent>=3.0)/nrow(data.3yrs)*100

# probability based on the density estimation

#hist
sum(movement.binwidth[41:74,"density"]*bin.width) *100

#normal

(1-pnorm(3.0,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)))*100

#kde
sum(kde.df$y[which(kde.df$x>=3)]*0.0035)*100

#******** 3.5% ***********#
day3.5<- data.3yrs[which(data.3yrs$daily.delta.percent>=3.5),"daily.delta.percent"] # buying day
day3.5.5d <- data.3yrs[which(data.3yrs$daily.delta.percent>=3.5)+5,"daily.delta.percent"] # holding for 5 day
mean(unlist((c(((day3.5.5d-day3.5)/(day3.5)))))) # mean return

# 10 day average return
day3.5.10d <- data.3yrs[which(data.3yrs$daily.delta.percent>=3.5)+10,"daily.delta.percent"]
mean(unlist((c(((day3.5.10d-day3.5)/(day3.5)))))) # mean return

# 20 day average return
day3.5.20d <- data.3yrs[which(data.3yrs$daily.delta.percent>=3.5)+20,"daily.delta.percent"]
mean(unlist((c(((day3.5.20d-day3.5)/(day3.5)))))) # mean return

#empirical probability
sum(data.3yrs$daily.delta.percent>=3.5)/nrow(data.3yrs)*100

# probability based on the density estimation

#hist
sum(movement.binwidth[42:74,"density"]*bin.width) *100

#normal

(1-pnorm(3.5,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)))*100

#kde
sum(kde.df$y[which(kde.df$x>=3.5)]*0.0035)*100


#*************** NEGATIVE ************************$#

#************-2.5%**************#

# 5 day average return
day2.5neg<- data.3yrs[which(data.3yrs$daily.delta.percent<=-2.5),"daily.delta.percent"] # buying day
day2.5.5dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-2.5)+5,"daily.delta.percent"] # holding for 5 day
mean(unlist((c(((day2.5.5dneg-day2.5neg)/(day2.5neg)))))) # mean return

# 10 day average return
day2.5.10dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-2.5)+10,"daily.delta.percent"]
mean(unlist((c(((day2.5.10dneg-day2.5neg)/(day2.5neg)))))) # mean return

# 20 day average return
day2.5.20dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-2.5)+20,"daily.delta.percent"]
mean(unlist((c(((day2.5.20dneg-day2.5neg)/(day2.5neg)))))) # mean return

#empirical probability
sum(data.3yrs$daily.delta.percent<=-2.5)/nrow(data.3yrs)*100

# probability based on the density estimation

#hist
sum(movement.binwidth[1:25,"density"]*bin.width) *100

#normal

(pnorm(-2.5,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)))*100

#kde
sum(kde.df$y[which(kde.df$x<=-2.5)]*0.0035)*100



#****************** -3.0%***********************#
# 5 day average return
day3.0neg<- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.0),"daily.delta.percent"] # buying day
day3.0.5dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.0)+5,"daily.delta.percent"] # holding for 5 day
mean(unlist((c(((day3.0.5dneg-day3.0neg)/(day3.0neg)))))) # returns

# 10 day average return
day3.0.10dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.0)+10,"daily.delta.percent"]
mean(unlist((c(((day3.0.10dneg-day3.0neg)/(day3.0neg)))))) # mean return

# 20 day average return
day3.0.20dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.0)+20,"daily.delta.percent"]
mean(unlist((c(((day3.0.20dneg-day3.0neg)/(day3.0neg)))))) # mean return

#empirical probability
sum(data.3yrs$daily.delta.percent<=-3.0)/nrow(data.3yrs)*100

# probability based on the density estimation

#hist
sum(movement.binwidth[1:23,"density"]*bin.width) *100

#normal

(pnorm(-3.0,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)))*100

#kde
sum(kde.df$y[which(kde.df$x<=-3)]*0.0035)*100

#******** - 3.5% ***********#
day3.5neg<- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.5),"daily.delta.percent"] # buying day
day3.5.5dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.5)+5,"daily.delta.percent"] # holding for 5 day
mean(unlist((c(((day3.5.5dneg-day3.5neg)/(day3.5neg)))))) # mean return

# 10 day average return
day3.5.10dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.5)+10,"daily.delta.percent"]
mean(unlist((c(((day3.5.10dneg-day3.5neg)/(day3.5neg)))))) # mean return

# 20 day average return
day3.5.20dneg <- data.3yrs[which(data.3yrs$daily.delta.percent<=-3.5)+20,"daily.delta.percent"]
mean(unlist((c(((day3.5.20dneg-day3.5neg)/(day3.5neg)))))) # mean return

#empirical probability
sum(data.3yrs$daily.delta.percent<=-3.5)/nrow(data.3yrs)*100

# probability based on the density estimation

#hist
sum(movement.binwidth[1:22,"density"]*bin.width) *100

#normal

(pnorm(-3.5,mean=mean(data$daily.delta.percent),sd=sd(data$daily.delta.percent)))*100

#kde
sum(kde.df$y[which(kde.df$x<=-3.5)]*0.0035)*100





