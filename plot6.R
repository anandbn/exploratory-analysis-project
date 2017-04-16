library(dplyr)
library(ggplot2)
library(grid)

## turn off scientific notation like 1e+06
options(scipen=999) 

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Merge the datasets 
merged_NEI <- merge (NEI, SCC)

## Get Onroad data points in Baltimore
onroad_bmore <- merged_NEI %>% filter(Data.Category == "Onroad" & fips == "24510")

## Get Onroad data points in LA County
onroad_LA <- merged_NEI %>% filter(Data.Category == "Onroad" & fips == "06037")


## Create a function that summarizes the data by "year" and adds the City name as a variable
summarizedAndYoY <- function(theData,city){
	pm25_year <- 	theData %>% 
					group_by(year) %>%  
					summarise(total_pm25 = sum(Emissions,na.rm=TRUE))

	pm25_year$prev_yr_emissions <- lag(pm25_year$total_pm25,1,default=0)

	pm25_year$YoY <- pm25_year$total_pm25 - pm25_year$prev_yr_emissions

	pm25_year$yoy_trend <- ifelse(pm25_year$YoY > 0, "increased", "decreased") 

	pm25_year$city <- rep(city,nrow(pm25_year))

	return (pm25_year)
}

## Combine the results of summarizedAndYoY for Baltimore & LA data sets
onroad_bmore_LA <- rbind(summarizedAndYoY(onroad_bmore),summarizedAndYoY(onroad_LA))

png("plot6.png",width=700)

## Plot Total Emissions faceted by City
g <- ggplot(onroad_bmore_LA,aes(year,total_pm25))
g <- g + geom_point(size=5) + facet_grid(. ~ city) + geom_smooth(method="lm",linetype = 3,color="red",se=FALSE)
g <- g+ labs(title="Onroad Emission Trend",y="Total Emissions")

## Plot YoY change faceted by City
g1 <- ggplot(onroad_bmore_LA,aes(year,YoY,label=YoY))
g1 <- g1 + facet_grid(. ~ city) 
g1 <- g1 + geom_bar(stat='identity', aes(fill=yoy_trend), width=.5)  

g1 <- g1+ scale_fill_manual(name="Change from Previous Year", 
                            labels = c("Increased", "Decreased"), 
                            values = c("decreased"="#00ba38", "increased"="#f8766d")) 

g1 <- g1+ labs(title="Onroad Emission Trend YoY",y="Change from previous year")


g1 <- g1 + theme(legend.position = c(0.90, 0.78))

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))

print(g, vp = vplayout(1,1))
print(g1, vp = vplayout(2,1))


dev.off()
