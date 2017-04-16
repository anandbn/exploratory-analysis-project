library(dplyr)
library(ggplot2)

## turn off scientific notation like 1e+06
options(scipen=999) 

NEI <- readRDS("summarySCC_PM25.rds")

## total emissions by year and type in Baltimore FIPS==24510
pm25_year_bmore <- NEI %>% filter(fips == "24510") %>% group_by(year,type) %>% summarize(total_pm25 = sum(Emissions,na.rm=TRUE))

png("plot3.png",width=600,height=600)
## Plot sum of emissions and date using ggplot with:
#    - Trend line using linear model
#    - Labels for X,Y and Title 
g <- ggplot(pm25_year_bmore,aes(year,total_pm25))
g <- g + geom_point() +  theme_bw(base_family = "Times") + facet_grid(. ~ type) 
g <- g + geom_smooth(method = "lm", linetype = 3,color="red",se=FALSE,show.legend=TRUE) + ylim(range(pm25_year_bmore$total_pm25))
g <- g + ylab("Total Emissions") + ggtitle("Baltimore Emissions - by Type") + theme(plot.title = element_text(hjust = 0.5))
plot(g)

dev.off()