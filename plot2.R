library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")

## turn off scientific notation like 1e+06
options(scipen=999) 

## total emissions by year in Baltimore FIPS==24510
pm25_year_bmore <- NEI %>% filter(fips == "24510") %>% group_by(year) %>% summarise(total_pm25 = sum(Emissions,na.rm=TRUE))

png(file="plot2.png")

## Plot sum of emissions and date. 
## Since the sum is a larger number, we divide by 1000 to display as kilo-tons
plot(pm25_year_bmore$year,pm25_year_bmore$total_pm25,type="l",pch=8,
	xlab="year",ylab="Total Emissions",main = "Baltimore - Total Emissions",
	ylim=range(pm25_year_bmore$total_pm25))

model <- lm(pm25_year_bmore$total_pm25 ~ pm25_year_bmore$year,pm25_year_bmore)
abline(model,lwd=2,col="red",lty=2)

dev.off()