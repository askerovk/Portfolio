if(!require(dplyr)){
    install.packages("dplyr")
}

if(!require(reshape2)){
    install.packages("reshape2")
}

if(!require(ggplot2)){
    install.packages("ggplot2")
}

library(dplyr)

library(reshape2)

library(ggplot2)

URL<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

download.file(url = URL, "PMdata.zip")

unzip("PMdata.zip")

pmdata<- readRDS("summarySCC_PM25.rds")

scc<- readRDS("Source_Classification_Code.rds")

code1<- scc[scc$SCC.Level.One=="Mobile Sources",]

l1<- unique(code1$SCC.Level.Two)[-c(2, 9, 10, 11, 12, 13, 14, 15, 16)] #exclude Border Crossings, aircraft, marine vessels, railroad equipmen and unknown. 

code2<-code1[code1$SCC.Level.Two %in% l1,]

l2<-unique(code2$SCC.Level.Three)[-c(4, 21, 9, 19, 22)]    ##exclude commercial, lawn and garden, logging, as these were pertrol powered devices, not vehicles.

code3<-code2[code2$SCC.Level.Three %in% l2,]

baltimore<- pmdata[pmdata$fips == "24510",]

LA<- pmdata[pmdata$fips == "06037",]

PM25a<- baltimore[baltimore$SCC %in% code3$SCC,]

PM25b<- LA[LA$SCC %in% code3$SCC,]

PM25a<- group_by(PM25a, year)

PM25b<- group_by(PM25b, year)

plota<-summarise(PM25a, PM25=sum(Emissions))

plotb<-summarise(PM25b, PM25=sum(Emissions))

plot<- merge(plota, plotb, by = "year")

final<-melt(plot, id.vars = "year")

final

ggplot(final, aes(year, value)) + geom_point(aes(col = variable), size=3)+
    
    geom_line(aes(col = variable), size=1.5)+
    
    labs(title="Total Annual PM25 Emissions in Baltimore and LA.", subtitle="From Motor Vechicle Sources.")+
    
    xlab("Year") + ylab("Emissions (tons)") + scale_colour_discrete(name="City", labels = c("Baltimore", "Los Angeles"))

