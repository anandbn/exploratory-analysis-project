library(dplyr)
library(ggplot2)
library(grid)

options(scipen=999) # turn off scientific notation like 1e+06

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Merge the datasets 
merged_NEI <- merge (NEI, SCC)

## Get Onroad data points in Baltimore
onroad_bmore <- merged_NEI %>% filter(Data.Category == "Onroad" & fips == "24510")

## Remove unused factors from the subset
onroad_bmore$SCC.Level.Three <- factor(onroad_bmore$SCC.Level.Three)


## Summarize by Year
pm25_year <- 	onroad_bmore %>% 
				group_by(year) %>%  
				summarise(total_pm25 = sum(Emissions,na.rm=TRUE))

## Copy over the previous year value using lag()
pm25_year$prev_yr_emissions <- lag(pm25_year$total_pm25,1,default=0)

## Calculate YoY change comparing to previous year 
pm25_year$YoY <- pm25_year$total_pm25 - pm25_year$prev_yr_emissions

## Bucket the YoY as "increasing"/"decreasing" based on the change
pm25_year$yoy_trend <- ifelse(pm25_year$YoY > 0, "increased", "decreased") 

png("plot5.png",width=700)

## Plot the total emissions
g <- ggplot(pm25_year,aes(year,total_pm25))
g <- g + geom_point()
g <- g + geom_smooth(method="lm",linetype = 3,color="red")
g <- g+ labs(title="Baltimore Onroad Emission Trend",y="Total Emissions")


## plot the YoY change
g1 <- ggplot(pm25_year,aes(year,YoY,label=YoY))
g1 <- g1 + geom_bar(stat='identity', aes(fill=yoy_trend), width=.5)  
g1 <- g1+ scale_fill_manual(name="Change from Previous Year", 
                   labels = c("Decreased", "Increased"), 
                   values = c("decreased"="#00ba38", "increased"="#f8766d"))
g1 <- g1 + labs(y="Change from previous year") 

g1 <- g1+ labs(title="Baltimore Onroad Emission YoY Trend") 
  
g1 <- g1 + theme(legend.position = c(0.75, 0.85))


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))

print(g, vp = vplayout(1,1))
print(g1, vp = vplayout(1,2))

dev.off()

