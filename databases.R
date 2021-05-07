##Prepare databases

rm(list=ls())


#climatic data
Precipitation<-read.csv("climate data/Counties_MeanPrecip_PRISM_V4_Aug312020.csv")
Temp<-read.csv("climate data/Counties_MeanTemp_PRISM_V4_Aug312020.csv")

#convert wide to long format (individual database)
library(dplyr)
listNames <- as.data.frame(colnames(Temp))
library(stringr)
listNames$`colnames(Temp)` <- as.character(listNames$`colnames(Temp)`)
listNames$`colnames(Temp)` <- gsub(".","-", listNames$`colnames(Temp)`, fixed = TRUE)
listNames$namedate <- NA
listNames$namedate[str_detect(listNames$`colnames(Temp)`,"X")]<-"dates"
listNames$newname <- ifelse(listNames$namedate == "dates", paste0("day.",listNames$`colnames(Temp)`),NA)
listNames$newname <- gsub("X","", listNames$newname)
listNames$newname[is.na(listNames$namedate)] <- as.character(listNames$`colnames(Temp)`)

names(Temp) <- listNames$newname

listNames <- as.data.frame(colnames(Precipitation))
listNames$`colnames(Precipitation)` <- as.character(listNames$`colnames(Precipitation)`)
listNames$`colnames(Precipitation)` <- gsub(".","-", listNames$`colnames(Precipitation)`, fixed = TRUE)
listNames$namedate <- NA
listNames$namedate[str_detect(listNames$`colnames(Precipitation)`,"X")]<-"dates"
listNames$newname <- ifelse(listNames$namedate == "dates", paste0("day.",listNames$`colnames(Precipitation)`),NA)
listNames$newname <- gsub("X","", listNames$newname)
listNames$newname[is.na(listNames$namedate)] <- as.character(listNames$`colnames(Precipitation)`)

names(Precipitation) <- listNames$newname

library(splitstackshape)
Precip.long <-merged.stack(Precipitation, id.vars=c("GEOID"), var.stubs=c("day"), sep = "day.")
colnames(Precip.long)[which(names(Precip.long) == "day")] <- "precipitation"
colnames(Precip.long)[which(names(Precip.long) == ".time_1")] <- "day"
Precip.long$rain <- ifelse(Precip.long$precipitation>2.54,1,0)

Temp.long <-merged.stack(Temp, id.vars=c("GEOID"), var.stubs=c("day"), sep = "day.")
colnames(Temp.long)[which(names(Temp.long) == "day")] <- "temp"
colnames(Temp.long)[which(names(Temp.long) == ".time_1")] <- "day"

climate.DB <- merge(Temp.long, Precip.long, by.x = c("GEOID","day"), by.y =c( "GEOID","day"))
climate.DB <- select(climate.DB, GEOID, day, temp, precipitation, rain, REGION)

#add rural area
library(openxlsx)
ruralurban <- read.xlsx("rural-urban/NCHSURCodes2013.xlsx")

final.DB <- merge(climate.DB, ruralurban, by.x = "GEOID", by.y = "FIPS.code", all.x)

final.DB <- select(final.DB, GEOID, day, temp, precipitation, rain, REGION, State.Abr.,`2013.code`)

colnames(final.DB)[which(names(final.DB) == "State.Abr.")] <- "state"
colnames(final.DB)[which(names(final.DB) == "2013.code")] <- "ruralurban_cat"

final.DB$ruralurban_cat <- ifelse(final.DB$ruralurban_cat == 1 | final.DB$ruralurban_cat == 2, "large metro", ifelse(final.DB$ruralurban_cat == 3 | final.DB$ruralurban_cat == 4, "medium/small metro", "rural"))

#add covid data per 100
filenames <- list.files(path = "C:/Users/pilar.fernandez/Google Drive/Tick app/Tick App Analysis - Git/Current/Dailylog analysis/JAMA paper/Covid data/cases per 100k", pattern='*.csv')
fullpath=file.path("C:/Users/pilar.fernandez/Google Drive/Tick app/Tick App Analysis - Git/Current/Dailylog analysis/JAMA paper/Covid data/cases per 100k",filenames)
DB_covid <- do.call("rbind",lapply(fullpath,FUN=function(files){read.csv(files)}))

library(stringr)
DB_covid$month <- str_extract(DB_covid$Year,"(?<=\\d{4}).{2}")
DB_covid$day <- str_extract(DB_covid$Year,"(?<=\\d{6}).{2}")
DB_covid$year <- "2020"
DB_covid$date <- as.Date(paste0(DB_covid$year,"-",DB_covid$month,"-",DB_covid$day))
DB_covid <- select(DB_covid, -stateFIPS, -Year, -Data.Comment, -Quarter, -X, -day, -month, -year)

library(lubridate)
final.DB$day <- as.Date(final.DB$day, "%m-%d-%Y")

final.DB <- merge(final.DB,DB_covid, by.x = c("GEOID","day"), by.y = c("countyFIPS","date"), all.x = TRUE)

final.DB$Value <- as.numeric(final.DB$Value)
names(final.DB)[names(final.DB) == "Value"] <- "covidper100"

#add covid data total
filenames <- list.files(path = "C:/Users/pilar.fernandez/Google Drive/Tick app/Tick App Analysis - Git/Current/Dailylog analysis/JAMA paper/Covid data/cases total", pattern='*.csv')
fullpath=file.path("C:/Users/pilar.fernandez/Google Drive/Tick app/Tick App Analysis - Git/Current/Dailylog analysis/JAMA paper/Covid data/cases total",filenames)
DB_covid <- do.call("rbind",lapply(fullpath,FUN=function(files){read.csv(files)}))

library(stringr)
DB_covid$month <- str_extract(DB_covid$Year,"(?<=\\d{4}).{2}")
DB_covid$day <- str_extract(DB_covid$Year,"(?<=\\d{6}).{2}")
DB_covid$year <- "2020"
DB_covid$date <- as.Date(paste0(DB_covid$year,"-",DB_covid$month,"-",DB_covid$day))
DB_covid <- select(DB_covid, -stateFIPS, -Year, -Data.Comment, -Quarter, -X, -day, -month, -year)

DB_covid$Value <- gsub(",", "", DB_covid$Value)

DB_covid$Value <- as.numeric(DB_covid$Value)

DB_covid_US <- dplyr::summarize(group_by(DB_covid, date),
                                covidtot = sum(Value))
library(zoo)
DB_covid_US$covidtot_7 <- rollmean(DB_covid_US$covidtot, k = 7, align = "right", fill = NA)
DB_covid_US$covidtot_7 <- round(as.numeric(format(DB_covid_US$covidtot_7, scientific=FALSE)))

pop_US2020 <- 330052960
DB_covid_US$covidtot_7per100 <- DB_covid_US$covidtot_7/pop_US2020*100000
DB_covid_US$covidtot_7per100 <- round(as.numeric(format(DB_covid_US$covidtot_7per100, scientific=FALSE)), digits = 2)

DB_covid_US <- dplyr::select(DB_covid_US, -covidtot, -covidtot_7)

final.DB <- merge(final.DB,DB_covid_US, by.x = "day", by.y = "date", all.x = TRUE)
final.DB <- dplyr::select(final.DB, -County, -State)

write.csv(final.DB,"extData.csv", row.names = FALSE)