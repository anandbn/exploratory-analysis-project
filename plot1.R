library(dplyr)

## turn off scientific notation like 1e+06
options(scipen=999) 

NEI <- readRDS("summarySCC_PM25.rds")

## total emissions by year
emissions_by_year <- NEI %>% group_by(year) %>% summarise(total_pm25 = sum(Emissions,na.rm=TRUE))

png(file="plot1.png")
## Plot sum of emissions and date. 
## Since the sum is a larger number, we'll use the total_pm25_million to display
plot(emissions_by_year$year,emissions_by_year$total_pm25,
	xlab="year",ylab="Total Emissions",main = "Total Emissions by year",pch=8,
	ylim=range(emissions_by_year$total_pm25))


## Draw a trend line based on the linear model
model <- lm(emissions_by_year$total_pm25 ~ emissions_by_year$year,emissions_by_year)
abline(model,lwd=2,col="red",lty=2)

dev.off()