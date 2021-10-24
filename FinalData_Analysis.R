#Load data
data = read.csv("FINAL_DATA.csv")

summary(data)
library(corrgram)


names(data)
tavg = data$AVG_TEMPARATURE
prcp = data$PERCIPITATION
infr = data$INFECTION_RATE_PER_100/100
vacr = data$FULLY_VACCINATED/data$POPULATION

par(mfrow=c(1,1))

colnames(data) = c("code","name","population","infections","deaths","total_vac","1dose_vac","fully_vac","firstdate","lastdate","stringency","urbanisation_R","urbanisation_YC","GDP","density","temp","prcp","infection_R","death_R","total_vac_R","OECD")
colnames(data) = c("code","name","pop","cases","deaths","tvac","1dvac","fvac","firstdate","lastdate","sindex","urbR","urbYC","GDP","density","temp","prcp","infrR","deathR","tvacR","oecd")
corrgram(data, 
         order="OLO",
         abs=TRUE, 
         cex=1.2,
         main="Country Covid Data Correlation Chart")

par(mfrow=c(3,2))

plot(tavg,infr,main="Infection Rate ~ Temperature",ylab="Infection Rate (Per Person)",xlab="Temperature Mean (Celsius)")
plot(prcp,infr,main="Infection Rate ~ Precipitation",ylab="Infection Rate (Per Person)",xlab="Precipitation Daily Mean (Millimeters)")
plot(tavg,vacr,main="Fully Vaccinated Rate ~ Temperature",ylab="Fully Vaccinated Rate (Per Person)",xlab="Temperature Mean (Celsius)")
plot(prcp,vacr,main="Fully Vaccinated Rate ~ Precipitation",ylab="Fully Vaccinated Rate (Per Person)",xlab="Precipitation Daily Mean (Millimeters)")

#Started with: tavg * prcp * I(tavg**2) * I(prcp**2)
#Only tavg + prcp was statistically significant.

m3 = lm(infr ~ tavg + prcp)
summary(m3)
plot(m3,which=1)

data[219,c(3,8,16,17)] #Very low total vacs per 1000 (1)

par(mfrow=c(2,2))

hist(tavg,main="Temperature",xlab="Temperature Mean (Celsius) 2020-09/2021",breaks=10)
hist(prcp,main="Precipitation",xlab="Mean Daily Precipitation (Millimeters) 2020-09/2021",breaks=10)
hist(infr,main="Infection Rate",xlab="Infection Rate (Per Person)",breaks=10)
hist(vacr,main="Fully Vaccinated Rate",xlab="Fully Vaccinated Rate (Per Person)",breaks=10)

par(mfrow=c(1,1))

#Vaccine rate


m = lm(vacr ~ tavg + prcp)

plot(m,which=1)
summary(m)

data[210,c(3,8,16,17)] #Has very high fully vaccinated rate.



