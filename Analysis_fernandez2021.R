# Empty workspace
rm(list=ls())

## Set WD
getwd() # has to be Cleanup files
#setwd("Dailylog analysis/JAMA paper")
#setwd("C:/Users/mpf2131/Google Drive/Tick app/Tick App Analysis - Git/Current/Dailylog analysis/JAMA paper")
library(here)
here()

##move columns (function)
options(na.action= na.omit)
moveMe <- function(data, tomove, where = "last", ba = NULL) {
    temp <- setdiff(names(data), tomove)
    x <- switch(
        where,
        first = data[c(tomove, temp)],
        last = data[c(temp, tomove)],
        before = {
            if (is.null(ba)) stop("must specify ba column")
            if (length(ba) > 1) stop("ba must be a single character string")
            data[append(temp, values = tomove, after = (match(ba, temp)-1))]
        },
        after = {
            if (is.null(ba)) stop("must specify ba column")
            if (length(ba) > 1) stop("ba must be a single character string")
            data[append(temp, values = tomove, after = (match(ba, temp)))]
        })
    x
}

# Create database #####################

DL_all <- read.csv("Cleanup files/Dailylog_merged_181920.csv")

library(lubridate)
DL_all$day <- as.Date(DL_all$day, "%Y-%m-%d")


SumDL.users <- DL_all %>% filter((DL_all$day > "2019-04-01" & DL_all$day < "2019-08-01") | (DL_all$day > "2020-02-01" & DL_all$day < "2020-08-01")) %>%
    dplyr::group_by(user.id) %>% dplyr::summarise(Midwest = n_distinct(user.id[region == "MidWest"]),
                                           Northeast =  n_distinct(user.id[region == "Northeast"]),
                                           South =  n_distinct(user.id[region == "South"]),
                                           West =  n_distinct(user.id[region == "West"]))

sum(SumDL.users$Midwest)
sum(SumDL.users$Northeast)
sum(SumDL.users$South)
sum(SumDL.users$West)

DL_all.2 <- subset(DL_all, (DL_all$day > "2019-04-01" & DL_all$day < "2019-08-01") | (DL_all$day > "2020-02-01" & DL_all$day < "2020-08-01") )
table(DL_all.2$region)


library(dplyr)
#restrict it only to the NE and MW: majority of users
DL_all <- subset(DL_all, DL_all$region == "MidWest" | DL_all$region == "Northeast")

DL.covid <- select(DL_all, user.id, gender, age, zipcode, county, GEOID, state, region, CDCregion, latitude, longitude, day, year, month, week, Outdoor_YN, Tick_self, Out_Peridom, Out_Natarea, Out_Notick, Out_Unknownrisk, COVID_outdooract, COVID_how, Why_no_outdoorrec, Why_no_outdoorrec_other, Feel, Feel_COVIDimpact, Out_Garden,Out_Mow, Out_PicnicYard,Out_YardPlay,Out_Brush,Out_Job,Out_ParkPlay,Out_PicnicPark, Out_Camp, Out_Hike, Out_Bike, Out_HikeWalk)

#add external data (climate and rural-urban)
extData.tomerge <- read.csv("Dailylog analysis/JAMA paper/extData.csv")

class(DL.covid$day)
class(extData.tomerge$day)

library(lubridate)
DL.covid$day <- as.Date(DL.covid$day, "%Y-%m-%d")
extData.tomerge$day <- as.Date(extData.tomerge$day, "%Y-%m-%d")

DL.covid <- merge(DL.covid,extData.tomerge, by.x = c("GEOID","day"), by.y = c("GEOID","day"), all.x = TRUE)

#days of rain
DL.covid.rain <- subset(DL.covid, DL.covid$rain ==1)
DL.covid.rain <- dplyr::summarize(group_by(DL.covid, year, week,GEOID,day),rainy.day = sum(rain, na.rm = TRUE))
DL.covid.rain$rainy.day[DL.covid.rain$rainy.day>=1]<-1
DL.covid.rain.week <- dplyr::summarize(group_by(DL.covid.rain, year, week, GEOID),rain = sum(rainy.day, na.rm = TRUE))

DL.covid <- merge(DL.covid,DL.covid.rain, by.x = c("GEOID","day","year","week"), by.y = c("GEOID","day","year","week"), all.x = TRUE)

#add covid data
covid.tomerge <- read.csv("Dailylog analysis/JAMA paper/Covid data/DB_vf.csv")

DL.covid <- merge(DL.covid, covid.tomerge, by.x = c("state.x", "county", "day"), by.y = c("statename", "countyname", "date_range_end"), all.x = TRUE)

summary(DL.covid$ruralurban_cat)
DL.covid$ruralurban_cat <- as.factor(DL.covid$ruralurban_cat)

#rerun with OHIO! -->original database

#users 2019/2020
SumDL.users <- dplyr::summarize(group_by(DL.covid, user.id),
                                Users.2018 = n_distinct(user.id[year=="2018"]),
                                Users.2019 = n_distinct(user.id[year=="2019"]),
                                Users.2020 = n_distinct(user.id[year=="2020"]))

SumDL.users$return.1920 <- ifelse(SumDL.users$Users.2019 == 1 & SumDL.users$Users.2020 == 1 , "both", ifelse(SumDL.users$Users.2019 == 1 & SumDL.users$Users.2020 == 0, "2019", "2020"))

table(SumDL.users$return.1920)

SumDL.users <- select(SumDL.users, user.id, return.1920)

DL.covid <- merge(DL.covid,SumDL.users, by.x = "user.id", by.y = "user.id", all.x = TRUE)

DL.covid <- moveMe(DL.covid, "return.1920", where = "after", ba= "year")
write.csv(DL.covid, "Dailylog analysis/JAMA paper/DL_covid.csv",row.names=F)

#Daily summary
SumDL.tot.day <- dplyr::summarize(group_by(DL.covid, year, week, day),
                                  N = n(),
                                  Users = n_distinct(user.id),
                                  AnyOutdoor = length(which(Outdoor_YN == "Yes")),
                                  num.male = length(which(gender == "Male")),
                                  num.female = length(which(gender == "Female")),
                                  age.median = median(age, na.rm = TRUE),
                                  age.IQR = IQR (age, na.rm = TRUE),
                                  Rec = sum(Out_Natarea, na.rm = TRUE),
                                  Peri = sum(Out_Peridom, na.rm = TRUE),
                                  Other_outdoor = sum(Out_Notick, na.rm = TRUE),
                                  Unknown_risk = sum(Out_Unknownrisk, na.rm = TRUE),
                                  median.temp = median(temp, na.rm = TRUE),
                                  num.rainy = length(which(rainy.day == 1)),
                                  rural = length(which(ruralurban_cat == "rural")),
                                  med.small.metro = length(which(ruralurban_cat == "medium/small metro")),
                                  large.metro = length(which(ruralurban_cat == "large metro")),
                                  median.SH.index = median(SH_index, na.rm = TRUE),
                                  median.cases = median(covid.cases, na.rm = TRUE),
                                  Garden = sum(Out_Garden, na.rm = TRUE),
                                  Mow_lawn = sum(Out_Mow, na.rm = TRUE),
                                  PicnicYard = sum(Out_PicnicYard, na.rm = TRUE),
                                  YardPlay = sum(Out_YardPlay, na.rm = TRUE),
                                  Brush_clear = sum(Out_Brush, na.rm = TRUE),
                                  Job = sum(Out_Job, na.rm = TRUE),
                                  ParkPlay = sum(Out_ParkPlay, na.rm = TRUE),
                                  PicnicPark = sum(Out_PicnicPark, na.rm = TRUE),
                                  Camp = sum(Out_Camp, na.rm = TRUE),
                                  Hike = sum(Out_Hike, na.rm = TRUE),
                                  Bike = sum(Out_Bike, na.rm = TRUE),
                                  Hike_Walk = sum(Out_HikeWalk, na.rm = TRUE))


#Daily summary daily (return users)
DL.covid.return <- subset(DL.covid, DL.covid$return.1920 == "both")

SumDL.tot.day.return <- dplyr::summarize(group_by(DL.covid.return, year, week, day),
                                         N = n(),
                                         Users = n_distinct(user.id),
                                         AnyOutdoor = length(which(Outdoor_YN == "Yes")),
                                         num.male = length(which(gender == "Male")),
                                         num.female = length(which(gender == "Female")),
                                         age.median = median(age, na.rm = TRUE),
                                         age.IQR = IQR (age, na.rm = TRUE),
                                         Rec = sum(Out_Natarea, na.rm = TRUE),
                                         Peri = sum(Out_Peridom, na.rm = TRUE),
                                         Other_outdoor = sum(Out_Notick, na.rm = TRUE),
                                         Unknown_risk = sum(Out_Unknownrisk, na.rm = TRUE),
                                         median.temp = median(temp, na.rm = TRUE),
                                         num.rainy = length(which(rainy.day == 1)),
                                         rural = length(which(ruralurban_cat == "rural")),
                                         med.small.metro = length(which(ruralurban_cat == "medium/small metro")),
                                         large.metro = length(which(ruralurban_cat == "large metro")),
                                         median.SH.index = median(SH_index, na.rm = TRUE),
                                         median.cases = median(covid.cases, na.rm = TRUE),
                                         Garden = sum(Out_Garden, na.rm = TRUE),
                                         Mow_lawn = sum(Out_Mow, na.rm = TRUE),
                                         PicnicYard = sum(Out_PicnicYard, na.rm = TRUE),
                                         YardPlay = sum(Out_YardPlay, na.rm = TRUE),
                                         Brush_clear = sum(Out_Brush, na.rm = TRUE),
                                         Job = sum(Out_Job, na.rm = TRUE),
                                         ParkPlay = sum(Out_ParkPlay, na.rm = TRUE),
                                         PicnicPark = sum(Out_PicnicPark, na.rm = TRUE),
                                         Camp = sum(Out_Camp, na.rm = TRUE),
                                         Hike = sum(Out_Hike, na.rm = TRUE),
                                         Bike = sum(Out_Bike, na.rm = TRUE),
                                         Hike_Walk = sum(Out_HikeWalk, na.rm = TRUE))

#Weekly Summary (for graph)
SumDL.tot <- dplyr::summarize(group_by(DL.covid, year, week),
                              N = n(),
                              N.return = length(which(return.1920 == "both")),
                              Users = n_distinct(user.id),
                              AnyOutdoor = length(which(Outdoor_YN == "Yes")),
                              num.male = length(which(gender == "Male")),
                              num.female = length(which(gender == "Female")),
                              age.median = median(age, na.rm = TRUE),
                              age.IQR = IQR (age, na.rm = TRUE),
                              Rec = sum(Out_Natarea, na.rm = TRUE),
                              Peri = sum(Out_Peridom, na.rm = TRUE),
                              Other_outdoor = sum(Out_Notick, na.rm = TRUE),
                              Unknown_risk = sum(Out_Unknownrisk, na.rm = TRUE),
                              median.temp = median(temp, na.rm = TRUE),
                              num.rainy = length(which(rainy.day == 1)),
                              rural = length(which(ruralurban_cat == "rural")),
                              med.small.metro = length(which(ruralurban_cat == "medium/small metro")),
                              large.metro = length(which(ruralurban_cat == "large metro")),
                              median.SH.index = median(SH_index, na.rm = TRUE),
                              median.cases = median(covid.cases, na.rm = TRUE),
                              Garden = sum(Out_Garden, na.rm = TRUE),
                              Mow_lawn = sum(Out_Mow, na.rm = TRUE),
                              PicnicYard = sum(Out_PicnicYard, na.rm = TRUE),
                              YardPlay = sum(Out_YardPlay, na.rm = TRUE),
                              Brush_clear = sum(Out_Brush, na.rm = TRUE),
                              Job = sum(Out_Job, na.rm = TRUE),
                              ParkPlay = sum(Out_ParkPlay, na.rm = TRUE),
                              PicnicPark = sum(Out_PicnicPark, na.rm = TRUE),
                              Camp = sum(Out_Camp, na.rm = TRUE),
                              Hike = sum(Out_Hike, na.rm = TRUE),
                              Bike = sum(Out_Bike, na.rm = TRUE),
                              Hike_Walk = sum(Out_HikeWalk, na.rm = TRUE),
                              ticks = length(which(Tick_self == "Yes, I found one tick")))

SumDL.tot$id <- paste(SumDL.tot$year,SumDL.tot$week)
SumDL.tot$Hike_Walk_Play <- SumDL.tot$Hike_Walk + SumDL.tot$ParkPlay

SumDL.tot <- merge(SumDL.tot,DL.covid.rain.week, by.x = c("year","week"), by.y = c("year","week"), all.x = TRUE)

##generate complete dataset for dates
ts <- seq.POSIXt(as.POSIXct("2018-04-20",'%Y/%m/%d'), as.POSIXct("2020-07-31",'%Y/%m/%d'), by="day")
ts <- seq.POSIXt(as.POSIXlt("2018-04-20"), as.POSIXlt("2020-07-31"), by="day")
ts <- format.POSIXct(ts,'%Y/%m/%d')
df <- data.frame(day=ts)
df$day <- as.character.POSIXt(df$day)
library(lubridate)
df$day <- as.Date(df$day, "%Y/%m/%d")
df$`CDCregion_Reg 1` <- NA
df$`CDCregion_Reg 2` <- NA
df$`CDCregion_Reg 3` <- NA
df$`CDCregion_Reg 5` <- NA
df$year <- year(df$day)
df$month <- month(df$day)
df$week <- week(df$day)
df <- data.frame(df)
df$id <- paste(df$year,df$week)
df <- moveMe(df, "id", where = "first", ba= NULL)

#remove duplicates
df.week <- subset(df[!duplicated(df$id),])
SumDL.tot <- subset(SumDL.tot[!duplicated(SumDL.tot$id),])

##merge with the summary database

SumDL.covidtot <- merge(SumDL.tot,df.week, by.x = c("id"), by.y = c("id"), all.y = TRUE)

#merge and order year and week columns
SumDL.covidtot$year.x <- ifelse(is.na(SumDL.covidtot$year.x), SumDL.covidtot$year.y, SumDL.covidtot$year.x)
SumDL.covidtot$week.x <- ifelse(is.na(SumDL.covidtot$week.x), SumDL.covidtot$week.y, SumDL.covidtot$week.x)
SumDL.covidtot <- SumDL.covidtot[order(SumDL.covidtot$year.x, SumDL.covidtot$week.x),]

#rename columns
colnames(SumDL.covidtot)[which(names(SumDL.covidtot) == "CDCregion.x")] <- "CDCregion"
colnames(SumDL.covidtot)[which(names(SumDL.covidtot) == "week.x")] <- "week"
colnames(SumDL.covidtot)[which(names(SumDL.covidtot) == "year.x")] <- "year"
SumDL.covidtot <- select(SumDL.covidtot, -week.y, -year.y, -id)

#create new variables: daily proportions for all activities
SumDL.covidtot$prop.Outdoor <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$AnyOutdoor/SumDL.covidtot$N)

SumDL.covidtot$prop.Rec <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Rec/SumDL.covidtot$N)

SumDL.covidtot$prop.Peri <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Peri/SumDL.covidtot$N)

SumDL.covidtot$prop.Other <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N),
                                    NA, SumDL.covidtot$Other_outdoor/SumDL.covidtot$N)
SumDL.covidtot$prop.Other[SumDL.covidtot$day < "2020-04-29"]<-NA

SumDL.covidtot$prop.Garden <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Garden/SumDL.covidtot$N)

SumDL.covidtot$prop.Mow <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Mow_lawn/SumDL.covidtot$N)

SumDL.covidtot$prop.PicnicYard <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$PicnicYard/SumDL.covidtot$N)

SumDL.covidtot$prop.YardPlay <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$YardPlay/SumDL.covidtot$N)

SumDL.covidtot$prop.Brush <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Brush_clear/SumDL.covidtot$N)

SumDL.covidtot$prop.Job <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Job/SumDL.covidtot$N)

SumDL.covidtot$prop.ParkPlay <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$ParkPlay/SumDL.covidtot$N)

SumDL.covidtot$prop.PicnicPark <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$PicnicPark/SumDL.covidtot$N)

SumDL.covidtot$prop.Camp <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Camp/SumDL.covidtot$N)

SumDL.covidtot$prop.Hike <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Hike/SumDL.covidtot$N)

SumDL.covidtot$prop.Bike <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Bike/SumDL.covidtot$N)

SumDL.covidtot$prop.HikeWalk <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Hike_Walk/SumDL.covidtot$N)

SumDL.covidtot$prop.HikeWalkPlay <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$Hike_Walk_Play/SumDL.covidtot$N)

SumDL.covidtot$prop.ticks <- ifelse(SumDL.covidtot$N<10 | is.na(SumDL.covidtot$N), NA, SumDL.covidtot$tick/SumDL.covidtot$N)

#move variables and reorder database
SumDL.covidtot <- moveMe(SumDL.covidtot, "day", where = "after", ba="week")
SumDL.covidtot <- moveMe(SumDL.covidtot, "month", where = "after", ba= "year")

###chequear estas lineas de codigo
SumDL.covidtot$num.rainy[SumDL.covidtot$year == "2018"] <- NA
SumDL.covidtot$num.rai[SumDL.covidtot$year == "2018"] <- NA
SumDL.covidtot$rural[SumDL.covidtot$year == "2018"] <- NA

SumDL.covidtot$prop.rainy <- SumDL.covidtot$num.rainy/SumDL.covidtot$N

write.csv(SumDL.covidtot, "Dailylog analysis/JAMA paper/SumCovid_total.csv",row.names=F)

#subset to 2019-20 data only
SumDL.covidtot.1920 <- subset(SumDL.covidtot, SumDL.covidtot$day > "2019-04-01")

#subset to 2020 data only
SumDL.covidtot.20 <- subset(SumDL.covidtot, SumDL.covidtot$day > "2020-01-01")
write.csv(SumDL.covidtot, "Dailylog analysis/JAMA paper/Outdoor_TickApp/OutdoorTickApp_2020.csv",row.names=F)


# Graphs #####################


##weekly surveys and users-----
library(ggplot2)
coeff <-1
plotje <- ggplot(data = SumDL.covidtot.1920, aes(x = day))+
    geom_line(aes(y=Users/coeff, linetype = "Total active users")) +
    geom_line(aes(y=N, color = "Total surveys")) +
    geom_line(aes(y=N.return, color = "Total surveys (return users)")) +
    #geom_line(aes(y=AnyOutdoor, color= "Any Outdoor act.")) +
    #geom_line(aes(y=Rec, color = "Recreational act.")) +
    #geom_line(aes(y=Peri, color = "Peridomestic act.")) +
    ylab("Number of surveys") +
    xlab("date") +
    scale_x_date(date_breaks = "2 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    scale_linetype_manual("",
                          breaks = c("Total active users"),
                          values = c("dashed")) +
    scale_colour_manual("",
                        breaks = c("Total surveys", "Total surveys (return users)", "Any Ooutdoor act."),
                        values = c("red", "blue", "darkgreen", "gray12", "gray0")) +
    scale_y_continuous(

        # Features of the first axis
        name = "Number of surveys per week",

        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Number of active users per week")
    ) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    theme(axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)))
plotje
#ggsave("DL_count_time.png")

##weekly weather data-------
library(ggplot2)
coeff <- 35
plotje <- ggplot(data = SumDL.covidtot.1920, aes(x = day))+
    geom_line(aes(y=prop.rainy, color = "rainy days")) +
    geom_line(aes(y=median.temp/coeff, color = "median temp")) +
    xlab("date")+
    scale_x_date(date_breaks = "2 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    scale_colour_manual("",
                        breaks = c("rainy days", "median temp"),
                        values = c("red","cyan")) +
    scale_y_continuous(

        # Features of the first axis
        name = "% of surveys completed on a rainy day",

        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="median temp experienced by users (°C)")
    ) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    theme(axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0)))+
    theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)))
plotje

#tables: stay at home measures/periods analyzed---------
covid_table <- read.table(text =
                              "  Start        End
                          1 2020-03-21 2020-04-02
                          ", header = TRUE,
                          stringsAsFactors = FALSE)
covid_table$Start <- as.Date(covid_table$Start)
covid_table$End <- as.Date(covid_table$End)

period_2019 <- read.table(text =
                              "  Start        End
                          1 2019-04-01 2019-07-31
                          ", header = TRUE,
                          stringsAsFactors = FALSE)
period_2019$Start <- as.Date(period_2019$Start)
period_2019$End <- as.Date(period_2019$End)

period_2020 <- read.table(text =
                              "  Start        End
                          1 2020-04-03 2020-07-31
                          ", header = TRUE,
                          stringsAsFactors = FALSE)
period_2020$Start <- as.Date(period_2020$Start)
period_2020$End <- as.Date(period_2020$End)

#covid measures-------
coeff <- 100
plotje <- ggplot(data = SumDL.covidtot.20, aes(x = day))+
    geom_line(aes(y=median.SH.index, color = "Stay at home index")) +
    geom_line(aes(y=median.cases/coeff, color = "COVID-19 cases")) +
    xlab("date")+
    scale_x_date(date_breaks = "1 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    scale_colour_manual("",
                        breaks = c("Stay at home index", "COVID-19 cases"),
                        values = c("red","blue")) +
    scale_y_continuous(

        # Features of the first axis
        name = "Stay at home index (median value per week)",

        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Median number of cases per week per county")
    ) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    theme(axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0)))+
    theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)))+
    geom_rect(data= covid_table, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="Effective dates"),colour=NA, alpha=0.5)+
    scale_fill_manual('Statewide stay-at-home orders
                      in the Northeast and Midwest',
                      values = c('pink','grey'))
plotje


##weekly activities--------
plotje <- ggplot(data = SumDL.covidtot.1920, aes(x = day))+
    geom_line(aes(y=prop.Outdoor, color= "Any Ooutdoor act.")) +
    geom_line(aes(y=prop.Rec, color = "Recreational act. in green spaces")) +
    geom_line(aes(y=prop.Peri, color = "Peridomestic act.")) +
    #geom_line(aes(y=prop.Other, color = "Other outdoor act.")) +
    ylab("Proportion of surveys") +
    xlab("date") +
    scale_colour_manual("Reported outdoor activities",
                        breaks = c("Any Ooutdoor act.", "Recreational act. in green spaces", "Peridomestic act.", "Other outdoor act.*"),
                        values = c("red","blue", "darkgreen","gray0")) +
    scale_x_date(date_breaks = "2 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    geom_rect(data= covid_table, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="Effective dates"),colour=NA, alpha=0.5)+
    scale_fill_manual('Statewide stay-at-home orders
                      in the Northeast and Midwest',
                      values = c('pink','grey')) +
    geom_rect(data= period_2019, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="period compared"),colour=NA, alpha=0.2)+
    geom_rect(data= period_2020, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="period compared"),colour=NA, alpha=0.2)
#                      guide = guide_legend(override.aes = list(alpha = 1)))
plotje

#per activity detail--------
plotje <- ggplot(data = SumDL.covidtot.1920, aes(x = day))+
    geom_line(aes(y=prop.Other, color = "Outdoor activities in grey spaces*")) +
    geom_line(aes(y=prop.Garden, color= "Gardening")) +
    geom_line(aes(y=prop.Mow, color = "Mowing the lawn")) +
    geom_line(aes(y=prop.PicnicYard, color = "Grilling / sitting in Yard")) +
    geom_line(aes(y=prop.HikeWalkPlay, color= "Hiking, Walking, Runing or Playing in green spaces")) +
    geom_line(aes(y=prop.Bike, color = "Biking in green spaces")) +
    #geom_line(aes(y=prop.YardPlay, color = "Playing in the Yard")) +
    #geom_line(aes(y=prop.Brush, color = "Removing brush")) +
    ylab("Proportion of surveys") +
    xlab("date") +
    scale_colour_manual("Reported yard activities",
                        breaks = c("Gardening", "Mowing the lawn", "Grilling / sitting in Yard", "Hiking, Walking, Runing or Playing in green spaces","Biking in green spaces","Outdoor activities in grey spaces*"),
                        values = c("red","blue", "darkgreen","purple", "green","gray0")) +
    scale_x_date(date_breaks = "2 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    geom_rect(data= covid_table, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="Effective dates"),colour=NA, alpha=0.5)+
    scale_fill_manual('Statewide stay-at-home orders
                      in the Northeast and Midwest',
                      values = 'pink')
#                      guide = guide_legend(override.aes = list(alpha = 1)))
plotje

#ticks--------
coeff <- 2
plotje <- ggplot(data = SumDL.covidtot.1920, aes(x = day))+
    stat_smooth(aes(y=prop.ticks, color = "Self-reported tick encounters"),
        geom = 'area', method = 'loess', span = 1/6,
        alpha = 1/2, fill = "cyan")+
    geom_line(aes(y=prop.Outdoor/coeff, color = "Any Outdoor activity")) +
    xlab("date") +
    scale_colour_manual("",
                        breaks = c("Any Outdoor activity", "Self-reported tick encounters"),
                        values = c("red","cyan")) +
    scale_x_date(date_breaks = "2 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    scale_y_continuous(

        # Features of the first axis
        name = "Proportion of surveys reporting
    tickencounters",

        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Proportion of surveys reporting
    any outdoor activities")
    )+
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    geom_rect(data= covid_table, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="Effective dates"),colour=NA, alpha=0.5)+
    scale_fill_manual('Statewide stay-at-home orders
    in the Northeast and Midwest',
                      values = c('pink','grey')) +
    geom_rect(data= period_2019, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="period compared"),colour=NA, alpha=0.2)+
    geom_rect(data= period_2020, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="period compared"),colour=NA, alpha=0.2)+
    theme(axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10, b = 0, l = 0)))+
    theme(axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)))
#                      guide = guide_legend(override.aes = list(alpha = 1)))
plotje




# Generate databases for models###############################

#2019 vs 2020
DB_analysis <- subset(SumDL.tot.day, SumDL.tot.day$day > "2019-04-01" & SumDL.tot.day$day < "2019-07-31" | SumDL.tot.day$day > "2020-04-01" & SumDL.tot.day$day < "2020-07-31")

DB_analysis$month <- month(DB_analysis$day)
DB_analysis$year <- as.factor(DB_analysis$year)
DB_analysis$month <- as.factor(DB_analysis$month)

DB_analysis$percent.rural <- DB_analysis$rural/DB_analysis$N
DB_analysis$percent.med.small.metro <- DB_analysis$med.small.metro/DB_analysis$N
DB_analysis$percent.large.metro <- DB_analysis$large.metro/DB_analysis$N
DB_analysis$percent.female <- DB_analysis$num.female/DB_analysis$N
DB_analysis$percent.rainy <- DB_analysis$num.rainy/DB_analysis$N

DB_analysis$percent.rural[DB_analysis$N < 5]<- NA
DB_analysis$percent.med.small.metro[DB_analysis$N < 5]<- NA
DB_analysis$percent.large.metro[DB_analysis$N < 5]<- NA
DB_analysis$percent.female[DB_analysis$N < 5]<- NA
DB_analysis$percent.rainy[DB_analysis$N < 5]<- NA

write.csv(DB_analysis,"Dailylog analysis/JAMA paper/DB_analysis.csv",row.names = FALSE)

#2020
DB_analysis2020 <- subset(SumDL.tot.day, SumDL.tot.day$day > "2020-02-01" & SumDL.tot.day$day < "2020-07-31")

DB_analysis2020$month <- month(DB_analysis2020$day)
DB_analysis2020$year <- as.factor(DB_analysis2020$year)

DB_analysis2020$covid <- NA
DB_analysis2020$covid[DB_analysis2020$month<4] <- "1pre"
DB_analysis2020$covid[DB_analysis2020$month>=4] <- "2post"
DB_analysis2020$month <- as.factor(DB_analysis2020$month)

DB_analysis2020$percent.rural <- DB_analysis2020$rural/DB_analysis2020$N
DB_analysis2020$percent.med.small.metro <- DB_analysis2020$med.small.metro/DB_analysis2020$N
DB_analysis2020$percent.large.metro <- DB_analysis2020$large.metro/DB_analysis2020$N
DB_analysis2020$percent.rainy <- DB_analysis2020$num.rainy/DB_analysis2020$N
DB_analysis2020$percent.female <- DB_analysis2020$num.female/DB_analysis2020$N

DB_analysis2020$percent.rural[DB_analysis2020$N < 5]<- NA
DB_analysis2020$percent.med.small.metro[DB_analysis2020$N < 5]<- NA
DB_analysis2020$percent.large.metro[DB_analysis2020$N < 5]<- NA
DB_analysis2020$percent.female[DB_analysis2020$N < 5]<- NA
DB_analysis2020$percent.rainy[DB_analysis2020$N < 5]<- NA

write.csv(DB_analysis2020,"Dailylog analysis/JAMA paper/DB_analysis2020.csv",row.names = FALSE)

#2019 vs 2020 return
DB_analysis.return <- subset(SumDL.tot.day.return, SumDL.tot.day.return$day > "2019-04-01" & SumDL.tot.day.return$day < "2019-07-31" | SumDL.tot.day.return$day > "2020-04-01" & SumDL.tot.day.return$day < "2020-07-31")

DB_analysis.return$month <- month(DB_analysis.return$day)
DB_analysis.return$year <- as.factor(DB_analysis.return$year)
DB_analysis.return$month <- as.factor(DB_analysis.return$month)

DB_analysis.return$percent.rural <- DB_analysis.return$rural/DB_analysis.return$N
DB_analysis.return$percent.med.small.metro <- DB_analysis.return$med.small.metro/DB_analysis.return$N
DB_analysis.return$percent.large.metro <- DB_analysis.return$large.metro/DB_analysis.return$N
DB_analysis.return$percent.female <- DB_analysis.return$num.female/DB_analysis.return$N
DB_analysis.return$percent.rainy <- DB_analysis.return$num.rainy/DB_analysis.return$N

DB_analysis.return$percent.rural[DB_analysis.return$N < 5]<- NA
DB_analysis.return$percent.med.small.metro[DB_analysis.return$N < 5]<- NA
DB_analysis.return$percent.large.metro[DB_analysis.return$N < 5]<- NA
DB_analysis.return$percent.female[DB_analysis.return$N < 5]<- NA
DB_analysis.return$percent.rainy[DB_analysis.return$N < 5]<- NA

write.csv(DB_analysis.return,"Dailylog analysis/JAMA paper/DB_analysis_return.csv",row.names = FALSE)



# Descriptive analyses - 2019/2020###########################

#demographic characteristics

#create a database per user
DL.covid.demog <- subset(DL.covid, DL.covid$day > "2019-04-01" & DL.covid$day < "2019-07-31" | DL.covid$day > "2020-04-01" & DL.covid$day < "2020-07-31")
DL.covid.demog$year <- as.factor(DL.covid.demog$year)
DL.covid.demog$month <- as.factor(DL.covid.demog$month)

#Summary_users
SumDL.users <- dplyr::summarize(group_by(DL.covid.demog, user.id),
                                Users.2019 = n_distinct(user.id[year=="2019"]),
                                Users.2020 = n_distinct(user.id[year=="2020"]))
SumDL.users$year <- ifelse(SumDL.users$Users.2019 == 1 & SumDL.users$Users.2020 == 1 , "both", ifelse(SumDL.users$Users.2019 == 1 & SumDL.users$Users.2020 == 0, "2019", "2020"))

table(SumDL.users$year)

#summary surveys
DL.covid.surveys <- subset(DL.covid, DL.covid$day > "2019-04-01" & DL.covid$day < "2019-07-31" | DL.covid$day > "2020-04-01" & DL.covid$day < "2020-07-31")
DL.covid.surveys$year <- as.factor(DL.covid.surveys$year)
table(DL.covid.surveys$year)
table(DL.covid.surveys$return.1920)

#summary demographics
SumDL.users <- dplyr::summarize(group_by(DL.covid.demog, year, user.id),
                                N = n(),
                                age.2019 = first(age[year=="2019"]),
                                age.2020 = first(age[year=="2020"]),
                                gender.2019 = first(gender[year=="2019"]),
                                gender.2020 = first(gender[year=="2020"]),
                                rural.2019 = first(ruralurban_cat[year=="2019"]),
                                rural.2020 = first(ruralurban_cat[year=="2020"]))

SumDL.users$age <- ifelse(!is.na(SumDL.users$age.2019),SumDL.users$age.2019,SumDL.users$age.2020)

#craete one gender variable
SumDL.users$gender.2020 <- as.character(SumDL.users$gender.2020)
SumDL.users$gender.2019 <- as.character(SumDL.users$gender.2019)
SumDL.users$gender.2019[is.na(SumDL.users$gender.2019)] <-""
SumDL.users$gender.2020[is.na(SumDL.users$gender.2020)] <-""
SumDL.users$gender <- paste0(SumDL.users$gender.2019, SumDL.users$gender.2020)

#create one rural variable
SumDL.users$rural.2020 <- as.character(SumDL.users$rural.2020)
SumDL.users$rural.2019 <- as.character(SumDL.users$rural.2019)
SumDL.users$rural.2019[is.na(SumDL.users$rural.2019)] <-""
SumDL.users$rural.2020[is.na(SumDL.users$rural.2020)] <-""
SumDL.users$rural <- paste0(SumDL.users$rural.2019, SumDL.users$rural.2020)

SumDL.users <- select(SumDL.users, year, age, gender, rural)

#Age
plot.age <- ggplot(SumDL.users, aes(x=year, y=age)) +
    geom_violin()+
    #geom_jitter(shape=16, position=position_jitter(0.4))+
    geom_boxplot(width=0.1)
plot.age
kruskal.test(age ~ year, data = SumDL.users)

#gender
table.gender <- table(SumDL.users$gender,SumDL.users$year)
SumDL.users$gender.2 <- ifelse(SumDL.users$gender == "Female", "Female", ifelse(SumDL.users$gender == "Male", "Male", NA))
table.gender.2 <- table(SumDL.users$gender.2,SumDL.users$year)
prop.table(table.gender.2, margin = 2)
chisq.test(SumDL.users$gender.2,SumDL.users$year)

#urbanicity
table.rural <- table(SumDL.users$rural,SumDL.users$year)
prop.table(table.rural, margin = 2)
chisq.test(SumDL.users$rural,SumDL.users$year)
library(chisq.posthoc.test)
chisq.posthoc.test(table.rural, method = "fdr")

sum.dem <-(dplyr::summarize(group_by(SumDL.users, year),
                            age.median = median(age, na.rm = TRUE),
                            age.IQR = IQR(age, na.rm = TRUE)))

#changes over time of the demographic variables -- cleanup-------
DB_analysis$day.graph <- as.Date(format(DB_analysis$day, format="%m-%d"))
plotje <- ggplot(data = DB_analysis, aes(x = day.graph))+
    geom_point(aes(y=percent.rural, color = "percent rural")) +
    geom_point(aes(y=percent.large.metro, color= "percent large metro")) +
    geom_point(aes(y=percent.med.small.metro, color = "percent med/small metro")) +
    ylab("Proportion of surveys") +
    xlab("date") +
    scale_colour_manual("Urbanization",
                        breaks = c("percent rural", "percent large metro", "percent med/small metro"),
                        values = c("red","blue", "darkgreen")) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    facet_grid(year~., scales="free_y")
plotje

plot.rural <- ggplot(DB_analysis, aes(x=day)) +
    geom_line(aes(y=percent.rural, color = "green")) +
    #geom_line(y=) +
    #geom_line(y=percent.med.small.metro) +
    #geom_line(y=percent.large.metro)
    plot.rural

plot.metroA <- ggplot(DB_analysis, aes(x=day,y=percent.med.small.metro)) +
    geom_point()
plot.metroA

plot.metroB <- ggplot(DB_analysis, aes(x=day,y=large.metro)) +
    geom_point()
plot.metroB

plot.female <- ggplot(DB_analysis, aes(x=day,y=percent.female)) +
    geom_point()
plot.female

##correlation urbanicity --> clean up-------

#install.packages("Hmisc")
library("Hmisc")
urban <- data.frame(rural=DB_analysis$percent.rural,med.small=DB_analysis$percent.med.small.metro, large=DB_analysis$percent.large.metro)

mydata.cor = cor(urban, use = "complete.obs")
mydata.cor
plot.cor <- corrplot(mydata.cor)

urban <- subset(urban, !is.na(urban))
urban.cor <- rcorr(as.matrix(urban))
urban.cor
#install.packages("corrplot")
library(corrplot)
urban.cor.r <- as.data.frame(urban.cor$r)
write.table(urban.cor.r, "clipboard", sep="\t")
urban.cor$P
#------
#proportion of activities -- summary
DB_analysis$prop.Outdoor <- ifelse(DB_analysis$N<10 | is.na(DB_analysis$N), NA, DB_analysis$AnyOutdoor/DB_analysis$N)

DB_analysis$prop.Rec <- ifelse(DB_analysis$N<10 | is.na(DB_analysis$N), NA, DB_analysis$Rec/DB_analysis$N)

DB_analysis$prop.Peri <- ifelse(DB_analysis$N<10 | is.na(DB_analysis$N), NA, DB_analysis$Peri/DB_analysis$N)

outdoo.act.year <-(dplyr::summarize(group_by(DB_analysis, year),
                                    daily.prop.Outdoor = median(prop.Outdoor, na.rm = TRUE),
                                    Outdoor.IQR = IQR(prop.Outdoor, na.rm = TRUE),
                                    daily.prop.Peri = median(prop.Peri, na.rm = TRUE),
                                    peri.IQR = IQR(prop.Peri, na.rm = TRUE),
                                    daily.prop.rec = median(prop.Rec, na.rm = TRUE),
                                    rec.IQR = IQR(prop.Rec, na.rm = TRUE)))


#boxplots dependent variables
DB_analysis$AnyOutdoor.percent <- DB_analysis$AnyOutdoor/DB_analysis$N*100
DB_analysis$Peri.percent <- DB_analysis$Peri/DB_analysis$N*100
DB_analysis$Rec.percent <- DB_analysis$Rec/DB_analysis$N*100

par(mfrow=c(1,3))
boxplot(DB_analysis$AnyOutdoor.percent, main = "% surveys reporting
        any outdoor activity")
boxplot(DB_analysis$Peri.percent, main = "% surveys reporting
        peridomestic activities")
boxplot(DB_analysis$Rec.percent, main = "% surveys reporting
        recreational activities in parks")

#boxplots: outliers?
par(mfrow=c(2,3))
boxplot(DB_analysis$median.temp, main = "median temperature")
boxplot(DB_analysis$percent.rainy, main = "% users reporting
        on a rainy day")
boxplot(DB_analysis$percent.rural, main = "% users in
        rural counties")
boxplot(DB_analysis$percent.large.metro, main = "% users in large
        metro counties")
boxplot(DB_analysis$percent.female, main = "% females")
boxplot(DB_analysis$age.median, main = "median age")


#if N>5
DB_analysis.5 <- subset(DB_analysis, DB_analysis$N > 5)

#remove outliers
DB_analysis <- subset(DB_analysis, DB_analysis$AnyOutdoor.percent>0 & DB_analysis$Peri.percent>0 & DB_analysis$Peri.percent<100)

#graphs--------
library(tidyr)
g1 <- DB_analysis %>%
    gather(var1, value1, c(median.temp,
                           percent.rainy,
                           percent.rural
    )) %>%
    gather(var2, value2, c(AnyOutdoor.percent,
                           Peri.percent,
                           Rec.percent)) %>%
    ggplot(aes(x = value1, y = value2)) +
    geom_point(aes(color = year), pch=1, size = 0.75) +
    geom_smooth(method =loess, se=FALSE, color="black", size = 0.25) +
    geom_smooth(aes(color = year), method=lm, se=FALSE, size=0.45)+
    ylab("Proportion of surveys") +
    xlab("")+
    scale_colour_manual("year",
                        breaks = c("Pre", "post"),
                        values = c("dodgerblue3","red2"),
                        labels=c("Pre(Feb-Mar)", "Post(April-July"))+
    facet_grid(var2~var1, scale = "free", switch = "y",
               labeller = as_labeller(c(AnyOutdoor.percent = "Any outdoor activity",
                                        Peri.percent = "Peridomestic activities",
                                        Rec.percent = "Recreational activities",
                                        median.temp = "Median temp.",
                                        percent.rainy = "% users on a
rainy day",
                                        percent.rural ="% users in
rural areas" ))) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_blank())

g2 <- DB_analysis %>%
    gather(var1, value1, c(percent.large.metro,
                           percent.female,
                           age.median
    )) %>%
    gather(var2, value2, c(AnyOutdoor.percent,
                           Peri.percent,
                           Rec.percent)) %>%
    ggplot(aes(x = value1, y = value2)) +
    geom_point(aes(color = year), pch=1, size = 0.75) +
    geom_smooth(method =loess, se=FALSE, color="black", size = 0.25) +
    geom_smooth(aes(color = year), method=lm, se=FALSE, size = 0.45)+
    ylab("Proportion of surveys") +
    xlab("")+
    scale_colour_manual("year",
                        breaks = c("Pre", "post"),
                        values = c("dodgerblue3","red2"),
                        labels=c("Pre(Feb-Mar)", "Post(April-July"))+
    facet_grid(var2~var1, scale = "free", switch = "y",
labeller = as_labeller(c(AnyOutdoor.percent = "Any outdoor activity",
                         Peri.percent = "Peridomestic activities",
                         Rec.percent = "Recreational activities",
                         percent.large.metro = "%users in large
metro areas",
                         percent.female = "% females",
                         age.median = "Median age"))) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_blank())

grid.arrange(g1,g2)



# 2019/2020 models################

#any outdoor activity-----
fitglm.null <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ 1, family = binomial(logit), data = DB_analysis)
summary(fitglm.null)

fitglm <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ year + month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial(logit), data = DB_analysis)
summary(fitglm)
library(rms)
vif(fitglm)

fitglm.int <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ year*month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis)
summary(fitglm.int)

fitglm.quad <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ year + month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis)
summary(fitglm.quad)

ci<-confint(fitglm.quad)
pval<-as.numeric(format(round((coef(summary(fitglm.quad))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.quad),ci)), pval)
print(table.coef, digits = 4)
write.table(table.coef, "clipboard", sep="\t")

rms::lrtest(fitglm.null, fitglm)
rms::lrtest(fitglm.quad, fitglm)

pR2 <- 1- fitglm$deviance / fitglm$null.deviance
pR2

pR2 <- 1- fitglm.quad$deviance / fitglm.quad$null.deviance
pR2

#Peridomestic-------
DB_analysis$year <- relevel(DB_analysis$year, ref = "2019")
fitglm.peri <- glm(cbind(Peri, N - Peri) ~ year + month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis)
summary(fitglm.peri)
acf(fitglm.peri$residuals)

ci<-confint(fitglm.peri)
pval<-as.numeric(format(round((coef(summary(fitglm.peri))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.peri),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

fitglm.peri.null <- glm(cbind(Peri, N - Peri) ~ 1, family = binomial, data = DB_analysis)
summary(fitglm.peri.null)

rms::lrtest(fitglm.peri.null, fitglm.peri)
pR2 <- 1 - fitglm.peri$deviance / fitglm.peri$null.deviance
pR2

fitglm.peri.quad <- glm(cbind(Peri, N - Peri) ~ year + month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis)
summary(fitglm.peri.quad)

rms::lrtest(fitglm.peri.quad, fitglm.peri)

#interaction
DB_analysis$year <- relevel(DB_analysis$year, ref = "2020")
fitglm.peri.int <- glm(cbind(Peri, N - Peri) ~ year*month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis)
summary(fitglm.peri.int.1)

fitglm.peri.int.2 <- glm(cbind(Peri, N - Peri) ~ year*month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis)
summary(fitglm.peri.int.2)

rms::lrtest(fitglm.peri.int, fitglm.peri.int.2)

rms::lrtest(fitglm.peri.int, fitglm.peri)
rms::lrtest(fitglm.peri.null, fitglm.peri.int)

ci<-confint(fitglm.peri.int)
pval<-as.numeric(format(round((coef(summary(fitglm.peri.int.1))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.peri.int.1),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

pR2 <- 1- fitglm.peri.int.1$deviance / fitglm.peri.int.1$null.deviance
pR2

#Rec---------
DB_analysis$year <- relevel(DB_analysis$year, ref = "2019")
#DB_analysis$month <- relevel(DB_analysis$month, ref = "4")

fitglm.rec <- glm(cbind(Rec, N - Rec) ~ year + month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial(logit), data = DB_analysis)
summary(fitglm.rec)
acf(fitglm.rec$residuals)
vif(fitglm.rec)
ci<-confint(fitglm.rec)
pval<-as.numeric(format(round((coef(summary(fitglm.rec))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.rec),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

fitglm.rec.quad <- glm(cbind(Rec, N - Rec) ~ year + month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial(logit), data = DB_analysis)
summary(fitglm.rec.quad)

lrtest(fitglm.rec, fitglm.rec.quad)

#int
DB_analysis$year <- relevel(DB_analysis$year, ref = "2020")
fitglm.rec.int <- glm(cbind(Rec, N - Rec) ~ year*month + median.temp + percent.rainy + percent.med.small.metro + percent.rural + percent.female + age.median, family = binomial(logit), data = DB_analysis)
summary(fitglm.rec.int)
ci<-confint(fitglm.rec.int)
pval<-as.numeric(format(round((coef(summary(fitglm.rec.int))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.rec.int),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

fitglm.rec.null <- glm(cbind(Rec, N - Rec) ~ 1, family = binomial(logit), data = DB_analysis)
summary(fitglm.rec.null)

fitglm.rec.int.1 <- glm(cbind(Rec, N - Rec) ~ year*month + median.temp + I(median.temp^2) + percent.rainy + percent.med.small.metro + percent.rural + percent.female + age.median, family = binomial(logit), data = DB_analysis)
summary(fitglm.rec.int.1)

lrtest(fitglm.rec, fitglm.rec.int)
lrtest(fitglm.rec.int, fitglm.rec.int.1)
lrtest(fitglm.null, fitglm.rec.int)

pR2 <- 1 - fitglm.rec$deviance / fitglm.rec$null.deviance
pR2

pR2 <- 1 - fitglm.rec.int$deviance / fitglm.rec.int$null.deviance
pR2


#acf graph, vif tables QQplot--------------
par(mfrow=c(3,3))
library("car")
plot(fitglm.quad,which=1)
qqPlot(fitglm.quad$residuals, ylab = "Residuals")
acf(fitglm.quad$residuals, main = "")

plot(fitglm.peri.int, which=1)
qqPlot(fitglm.peri.int$residuals, ylab = "Residuals")
acf(fitglm.peri.int$residuals, main = "")

plot(fitglm.rec.int,which=1)
qqPlot(fitglm.rec.int$residuals, ylab = "Residuals")
acf(fitglm.rec.int$residuals, main = "")


vif(fitglm)
vif(fitglm.peri)
vif(fitglm.rec)
names.vif <- list("Year", "Month", "Temp", "Rain", "% rural", "% large metro", "% female", "Median Age")
GVIF <-(as.numeric(format(round((vif(fitglm)[,'GVIF^(1/(2*Df))']),2), scientific = FALSE)))^2
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p1 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 4),
                   breaks = c(1, 2, 3),
                   labels = c("1", "2", "3"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.45))

GVIF <-(as.numeric(format(round((vif(fitglm.peri)[,'GVIF^(1/(2*Df))']),2), scientific = FALSE)))^2
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p2 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3), linetype = 3)+
    xlab("") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 4),
                        breaks = c(1, 2, 3),
                        labels = c("1", "2", "3"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.45))


GVIF <-(as.numeric(format(round((vif(fitglm.rec)[,'GVIF^(1/(2*Df))']),2), scientific = FALSE)))^2
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p3 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3), linetype = 3)+
    xlab("") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 4),
                        breaks = c(1, 2, 3),
                        labels = c("1", "2", "3"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.45))

grid.arrange(p1,p2,p3, ncol = 3)


# 2019/2020 models - return users##################################
#any outdoor activity----------
fitglm.null <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ 1, family = binomial(logit), data = DB_analysis.return)
summary(fitglm.null)

fitglm <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ year + month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial(logit), data = DB_analysis.return)
summary(fitglm)
acf(fitglm$residuals)
library(rms)
vif(fitglm)

ci<-confint(fitglm)
pval<-as.numeric(format(round((coef(summary(fitglm))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

rms::lrtest(fitglm.null, fitglm)
pR2 <- 1- fitglm$deviance / fitglm$null.deviance
pR2

fitglm.quad <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ year + month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis.return)
summary(fitglm.quad)

rms::lrtest(fitglm.quad, fitglm)

#Peridomestic-------
DB_analysis$year <- relevel(DB_analysis$year, ref = "2019")
fitglm.peri <- glm(cbind(Peri, N - Peri) ~ year + month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis.return)
summary(fitglm.peri)

ci<-confint(fitglm.peri)
pval<-as.numeric(format(round((coef(summary(fitglm.peri))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.peri),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

fitglm.peri.null <- glm(cbind(Peri, N - Peri) ~ 1, family = binomial, data = DB_analysis.return)
summary(fitglm.peri.null)

pR2 <- 1 - fitglm.peri$deviance / fitglm.peri$null.deviance
pR2

fitglm.peri.quad <- glm(cbind(Peri, N - Peri) ~ year + month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.med.small.metro + percent.female + age.median, family = binomial, data = DB_analysis.return)
summary(fitglm.peri.quad)

rms::lrtest(fitglm.peri.quad, fitglm.peri)

#int
DB_analysis.return$year <- relevel(DB_analysis.return$year, ref = "2019")
fitglm.peri.int <- glm(cbind(Peri, N - Peri) ~ year*month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis.return)
summary(fitglm.peri.int)

rms::lrtest(fitglm.peri, fitglm.peri.int)

ci<-confint(fitglm.peri.int)
pval<-as.numeric(format(round((coef(summary(fitglm.peri.int))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.peri.int),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

pR2 <- 1 - fitglm.peri.int$deviance / fitglm.peri.int$null.deviance
pR2

rms::lrtest(fitglm.peri.null, fitglm.peri.int)

fitglm.peri.int.2 <- glm(cbind(Peri, N - Peri) ~ year*month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial, data = DB_analysis.return)
summary(fitglm.peri.int.2)

rms::lrtest(fitglm.peri.int.2, fitglm.peri.int)

#Rec----
DB_analysis.return$year <- relevel(DB_analysis.return$year, ref = "2019")
fitglm.rec <- glm(cbind(Rec, N - Rec) ~ year + month + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median, family = binomial(logit), data = DB_analysis.return)
summary(fitglm.rec)
acf(fitglm.rec$residuals)
vif(fitglm.rec)
ci<-confint(fitglm.rec)
pval<-as.numeric(format(round((coef(summary(fitglm.rec))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.rec),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

#quad
fitglm.rec.quad <- glm(cbind(Rec, N - Rec) ~ year + month + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.med.small.metro + percent.female + age.median, family = binomial, data = DB_analysis.return)
summary(fitglm.rec.quad)

lrtest(fitglm.rec, fitglm.rec.quad)

#int
DB_analysis.return$year <- relevel(DB_analysis.return$year, ref = "2020")
fitglm.rec.int <- glm(cbind(Rec, N - Rec) ~ year*month + median.temp + percent.rainy + percent.med.small.metro + percent.rural + percent.female + age.median, family = binomial(logit), data = DB_analysis.return)
summary(fitglm.rec.int)
ci<-confint(fitglm.rec.int)
pval<-as.numeric(format(round((coef(summary(fitglm.rec.int))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.rec.int),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

fitglm.rec.null <- glm(cbind(Rec, N - Rec) ~ 1, family = binomial(logit), data = DB_analysis.return)
summary(fitglm.rec.null)
DB_analysis$year <- relevel(DB_analysis$year, ref = "2019")

lrtest(fitglm.rec, fitglm.rec.int)
lrtest(fitglm.null, fitglm.rec.int)

pR2 <- 1 - fitglm.rec$deviance / fitglm.rec$null.deviance
pR2

pR2 <- 1 - fitglm.rec.int$deviance / fitglm.rec.int$null.deviance
pR2

fitglm.rec.int.2 <- glm(cbind(Rec, N - Rec) ~ year*month + median.temp + I(median.temp^2) + percent.rainy + percent.med.small.metro + percent.rural + percent.female + age.median, family = binomial(logit), data = DB_analysis.return)
summary(fitglm.rec.int.2)

lrtest(fitglm.rec.int, fitglm.rec.int.2)


# 2020 descriptive##################################

#boxplots dependent variables
DB_analysis2020$AnyOutdoor.percent <- DB_analysis2020$AnyOutdoor/DB_analysis2020$N*100
DB_analysis2020$Peri.percent <- DB_analysis2020$Peri/DB_analysis2020$N*100
DB_analysis2020$Rec.percent <- DB_analysis2020$Rec/DB_analysis2020$N*100

par(mfrow=c(1,3))
boxplot(DB_analysis2020$AnyOutdoor.percent, main = "% surveys reporting
        any outdoor activity")
boxplot(DB_analysis2020$Peri.percent, main = "% surveys reporting
        peridomestic activities")
boxplot(DB_analysis2020$Rec.percent, main = "% surveys reporting
        recreational activities in parks")

#boxplots: outliers?
par(mfrow=c(2,4))
boxplot(DB_analysis2020$median.temp, main = "median temperature")
boxplot(DB_analysis2020$percent.rainy, main = "% users reporting
        on a rainy day")
boxplot(DB_analysis2020$percent.rural, main = "% users in
        rural counties")
boxplot(DB_analysis2020$percent.large.metro, main = "% users in large
        metro counties")
boxplot(DB_analysis2020$percent.female, main = "% females")
boxplot(DB_analysis2020$age.median, main = "median age")
boxplot(DB_analysis2020$median.SH.index, main = "median Shelter in
        Place index")
boxplot(DB_analysis2020$median.cases, main = "median COVID 19 cases
        per day per county")

#if N>5
DB_analysis2020.5 <- subset(DB_analysis2020, DB_analysis2020$N > 5)

#if remove outliers
DB_analysis2020.outliers <- subset(DB_analysis2020, DB_analysis2020$percent.rainy < 0.8 | DB_analysis2020$percent.rural < 0.7 | DB_analysis2020$percent.female > 0.2 | DB_analysis2020$median.SH.index < 20 | DB_analysis2020$age.median > 40 )

#rescale covid variables
DB_analysis2020$SH.index10 <- DB_analysis2020$median.SH.index/10
DB_analysis2020$cases100 <- DB_analysis2020$median.cases/1000

#graphs--------
DB_analysis2020$covid.col <- ifelse(DB_analysis2020$covid == "1pre","blue","red")

g1 <- DB_analysis2020 %>%
    gather(var1, value1, c(median.temp,
                           percent.rainy,
                           median.SH.index,
                           median.cases
                           )) %>%
    gather(var2, value2, c(AnyOutdoor.percent,
                           Peri.percent,
                           Rec.percent)) %>%
    ggplot(aes(x = value1, y = value2)) +
    geom_point(aes(color = covid), pch=1, size = 0.75) +
    geom_smooth(method =loess, se=FALSE, color="black", size = 0.25) +
    geom_smooth(aes(color = covid), method=lm, se=FALSE, size=0.45)+
    ylab("Proportion of surveys") +
    xlab("")+
    scale_colour_manual("covid",
                        breaks = c("Pre", "post"),
                        values = c("dodgerblue3","red2"),
                        labels=c("Pre(Feb-Mar)", "Post(April-July"))+
    facet_grid(var2~var1, scale = "free", switch = "y", labeller = as_labeller(c(AnyOutdoor.percent = "Any outdoor activity",                 Peri.percent = "Peridomestic activities",                        Rec.percent = "Recreational activities",                         median.temp = "Median temp.",
            percent.rainy = "% users on a
rainy day", median.SH.index = "SIP Index",
            median.cases = "Median COVID
cases per county"))) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_blank())

g2 <- DB_analysis2020 %>%
    gather(var1, value1, c(percent.rural,
                           percent.large.metro,
                           percent.female,
                           age.median
    )) %>%
    gather(var2, value2, c(AnyOutdoor.percent,
                           Peri.percent,
                           Rec.percent)) %>%
    ggplot(aes(x = value1, y = value2)) +
    geom_point(aes(color = covid), pch=1, size = 0.75) +
    geom_smooth(method =loess, se=FALSE, color="black", size = 0.25) +
    geom_smooth(aes(color = covid), method=lm, se=FALSE, size = 0.45)+
    ylab("Proportion of surveys") +
    xlab("")+
    scale_colour_manual("covid",
                        breaks = c("Pre", "post"),
                        values = c("dodgerblue3","red2"),
                        labels=c("Pre(Feb-Mar)", "Post(April-July"))+
    facet_grid(var2~var1, scale = "free", switch = "y", labeller = as_labeller(c(AnyOutdoor.percent = "Any outdoor activity",                 Peri.percent = "Peridomestic activities",                        Rec.percent = "Recreational activities",                         percent.rural = "% users in
rural areas",
            percent.large.metro = "%users in large
metro areas",
            percent.female = "% females",
            age.median = "Median age"))) +
    theme_bw() +
    theme(strip.placement = "outside",
          strip.background = element_blank())

grid.arrange(g1,g2)


#2020 models##############

#any outdoor activities------
fitglm <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ covid + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm)

fitglm.quad <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ covid + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.quad)
acf(fitglm$residuals)
vif(fitglm)

fitglm.quad.2 <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ covid + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + + I(SH.index10^2) + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.quad.2)
acf(fitglm$residuals)
vif(fitglm)

fitglm.null <- glm(cbind(AnyOutdoor, N - AnyOutdoor) ~ 1, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.null)

lrtest(fitglm, fitglm.null)
lrtest(fitglm, fitglm.quad)
lrtest(fitglm, fitglm.quad.2)

ci<-confint(fitglm)
pval<-as.numeric(format(round((coef(summary(fitglm))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

pR2 <- 1 - fitglm$deviance / fitglm$null.deviance
pR2

#Rec--------
fitglm.rec <- glm(cbind(Rec, N - Rec) ~ covid + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.rec)

fitglm.rec.quad <- glm(cbind(Rec, N - Rec) ~ covid + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.rec.quad)

fitglm.rec.quad.2 <- glm(cbind(Rec, N - Rec) ~ covid + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + I(SH.index10^2) + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.rec.quad.2)

fitglm.rec.null <- glm(cbind(Rec, N - Rec) ~ 1, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.rec.null)

lrtest(fitglm.rec.quad, fitglm.rec.null)
lrtest(fitglm.rec, fitglm.rec.quad)
lrtest(fitglm.rec, fitglm.rec.quad.2)
lrtest(fitglm.rec.quad, fitglm.rec.quad.2)

ci<-confint(fitglm.rec.quad)
pval<-as.numeric(format(round((coef(summary(fitglm.rec.quad))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.rec.quad),ci)), pval)
print(table.coef, digits = 4)
write.table(table.coef, "clipboard", sep="\t")

pR2 <- 1 - fitglm.rec.quad$deviance / fitglm.rec.quad$null.deviance
pR2

#Peri----------
fitglm.peri <- glm(cbind(Peri, N - Peri) ~ covid + median.temp + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.peri)

fitglm.peri.quad <- glm(cbind(Peri, N - Peri) ~ covid + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.peri.quad)

fitglm.peri.quad.2 <- glm(cbind(Peri, N - Peri) ~ covid + median.temp + I(median.temp^2) + percent.rainy + percent.rural + percent.large.metro + percent.female + age.median + SH.index10 + I(SH.index10^2) + cases100, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.peri.quad.2)

fitglm.peri.null <- glm(cbind(Peri, N - Peri) ~ 1, family = binomial(logit), data = DB_analysis2020)
summary(fitglm.peri.null)

lrtest(fitglm.peri, fitglm.peri.null)
lrtest(fitglm.peri, fitglm.peri.quad)
lrtest(fitglm.peri.quad.2, fitglm.peri.quad)

ci<-confint(fitglm.peri.quad)
pval<-as.numeric(format(round((coef(summary(fitglm.peri.quad))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(fitglm.peri.quad),ci)), pval)
print(table.coef, digits = 4)
write.table(table.coef, "clipboard", sep="\t")

pR2 <- 1 - fitglm.peri.quad$deviance / fitglm.peri.quad$null.deviance
pR2

#acf graph, vif tables QQplot--------------
par(mfrow=c(3,3))
library("car")
plot(fitglm,which=1)
qqPlot(fitglm$residuals, ylab = "Residuals")
acf(fitglm$residuals, main = "")

plot(fitglm.peri.quad, which=1)
qqPlot(fitglm.peri.quad$residuals, ylab = "Residuals")
acf(fitglm.peri.quad$residuals, main = "")

plot(fitglm.rec.quad,which=1)
qqPlot(fitglm.rec.quad$residuals, ylab = "Residuals")
acf(fitglm.rec.quad$residuals, main = "")

names.vif <- list("SH meassures", "Temp", "Rain", "% rural", "% large metro", "% female", "Median Age", "SH index", "Covid cases")
GVIF <- vif(fitglm)
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p1 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3,4), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 5),
                        breaks = c(1, 2, 3, 4),
                        labels = c("1", "2", "3", "4"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.45))

GVIF <- vif(fitglm.peri)
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p2 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3,4), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 5),
                        breaks = c(1, 2, 3, 4),
                        labels = c("1", "2", "3", "4"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.45))

GVIF <- vif(fitglm.rec)
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p3 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3,4), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 5),
                        breaks = c(1, 2, 3, 4),
                        labels = c("1", "2", "3", "4"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.45))

grid.arrange(p1,p2,p3, ncol = 3)





#Covid impact##############

##razones tablas
table(DL.covid$COVID_outdooract) ##separar lo de covid

#no impact
DL.covid$COVID_noimpact <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("No impact", DL.covid$COVID_outdooract)), "COVID_noimpact"] <- 1
table(DL.covid$COVID_noimpact)

#more time in yard
DL.covid$COVID_time.yard <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I spent more time in my yard", DL.covid$COVID_outdooract)), "COVID_time.yard"] <- 1
table(DL.covid$COVID_time.yard)

#more time in parks
DL.covid$COVID_moretime.parks <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I spent more time in public parks and natural areas", DL.covid$COVID_outdooract)), "COVID_moretime.parks"] <- 1
table(DL.covid$COVID_moretime.parks)

#less time in parks
DL.covid$COVID_lesstime.parks <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I shortened time spent in public parks and natural areas", DL.covid$COVID_outdooract)), "COVID_lesstime.parks"] <- 1
table(DL.covid$COVID_lesstime.parks)

#avoid parks
DL.covid$COVID_avoid.parks <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I avoided going to public natural areas and parks", DL.covid$COVID_outdooract)), "COVID_avoid.parks"] <- 1
table(DL.covid$COVID_avoid.parks)

#less crowded parks
DL.covid$COVID_lesscrowded.parks <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I chose to visit a less crowded park / natural area", DL.covid$COVID_outdooract)), "COVID_lesscrowded.parks"] <- 1
table(DL.covid$COVID_lesscrowded.parks)

#out of mantained paths
DL.covid$COVID_unmantainedpaths.park <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I spent time outside of maintained paths, areas I would normally avoid", DL.covid$COVID_outdooract)), "COVID_unmantainedpaths.park"] <- 1
table(DL.covid$COVID_unmantainedpaths.park)

#more frequently outside
DL.covid$COVID_morefreq.outside <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("I went outside more often", DL.covid$COVID_outdooract)), "COVID_morefreq.outside"] <- 1
table(DL.covid$COVID_morefreq.outside)

#other
DL.covid$COVID_other <- ifelse(is.na(DL.covid$COVID_outdooract), NA, 0)
DL.covid[which(grepl("Other", DL.covid$COVID_outdooract)), "COVID_other"] <- 1
table(DL.covid$COVID_other)

#if they ddin't go outdoors, why?

##razones tablas
table(DL.covid$Why_no_outdoorrec)

##recode variable
#DL.covid$Why_no_outdoorrec <- gsub("Avoid mosquitoes","Avoid vectors",DL.covid$Why_no_outdoorrec)
#DL.covid$Why_no_outdoorrec <- gsub("Avoid ticks","Avoid vectors",DL.covid$Why_no_outdoorrec)

##CORREGIR OTHERS
#write.csv(DL.covid, "DL.covid_Othertofix.csv",row.names=F)
DL.covid.2 <- read.csv("Dailylog analysis/JAMA paper/DL.covid_Othertofix.csv")

##razones tablas
table(DL.covid.2$Why_no_outdoorrec)

#impact and covid
DL.covid <- subset(DL.covid, DL.covid$day < "2020-07-31")
DL.covid$SH.index10 <- DL.covid$SH_index/10
DL.covid$cases100 <- DL.covid$covid.cases/1000
DL.covid$age_r <- DL.covid$age/5
DL.covid$month <- as.factor(DL.covid$month)
DL.covid$month.recode <- DL.covid$month
DL.covid$month.recode[DL.covid$month.recode=="4"]<-"5"

DL.covid$COVID.impact <- ifelse(is.na(DL.covid$COVID_noimpact), NA, ifelse(DL.covid$COVID_noimpact == 0, 1, 0))

DL.covid.impact <- subset(DL.covid, !is.na(DL.covid$COVID.impact) & !is.na(DL.covid$SH.index10) & !is.na(DL.covid$cases100) & !is.na(DL.covid$ruralurban_cat) &
!is.na(DL.covid$gender) & !is.na(DL.covid$age_r))

DL.covid$ruralurban_cat <- relevel(DL.covid$ruralurban_cat, ref = "rural")

covid.glm <- glm(COVID.impact ~ month.recode + SH.index10 + cases100 + ruralurban_cat+ gender + age_r, family = binomial(logit), data = DL.covid.impact)
summary(covid.glm)
vif(covid.glm)

ci<-confint(covid.glm)
pval<-as.numeric(format(round((coef(summary(covid.glm))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=coef(covid.glm),ci)), pval)
print(table.coef, digits = 4)
write.table(table.coef, "clipboard", sep="\t")

library(lme4)
covid.glm.2 <- glmer(COVID.impact ~ month.recode + SH.index10 + cases100+ ruralurban_cat+ gender + age_r + (1|user.id), family = binomial, data = DL.covid.impact ,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(covid.glm.2)

anova(covid.glm.2, covid.glm)

ci<-confint(covid.glm.2, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(covid.glm.2))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(covid.glm.2),ci)), pval)
print(table.coef, digits = 4)
write.table(table.coef, "clipboard", sep="\t")

covid.glmm.null <- glmer(COVID.impact ~ 1 + (1|user.id), family = binomial, data = DL.covid.impact ,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(covid.glmm.null)

anova(covid.glm.2, covid.glmm.null)

#auc, vif tables QQplot--------------
library(DHARMa)
par(mfrow=c(1,3))
set.seed(123)
simulationOutput <- simulateResiduals(fittedModel = covid.glm.2)
res2 <- recalculateResiduals(simulationOutput, group = DL.covid.impact$user.id)
plot(res2, quantreg = F)

plotQQunif(res2)
plotResiduals(res2, quantreg = F)
testDispersion(res2)

names.vif <- list("Month", "SH index", "Covid cases", "Urbanicity", "Gender", "Age")
GVIF <- vif(covid.glm)
GVIF
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p4 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3,4), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 2),
                        breaks = c(1, 2),
                        labels = c("1", "2"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.25))

p4

#labels es el valor observado (1 o 0) en valor numerico, no categorizado
#library (ROCR)
predMA <-predict(covid.glm.2, type="response")
predMA<-as.numeric(predMA)
labels<-(DL.covid.impact$COVID.impact)

#guardar los resultados en un csv
resultado<-data.frame(predMA, labels)

#curva de ROC
library(pROC)

#aplicar la curva de ROC, analysis estima el poder de clasificaci?n (el ?rea debajo de la curva de ROC)
analysis <- roc(labels,predMA)
analysis

#Bucar el valor de corte ?timo (Find t that minimizes error)
e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]
opt_t

#calculo sensibilidad
tp <- sum( (predMA > opt_t) & labels)
tp
sensitivity <- tp / sum (labels)
sensitivity

#calculo especificidad
tn <- sum( (predMA <= opt_t) & (!labels))
tn
specificity <- tn / sum(!labels)
specificity

#tabla de clasificaci?n
umbral<-opt_t
pred.modelo<- cut(predMA,breaks=c(-Inf , umbral , Inf), labels=c("no inf","inf"))
cTab<-table(labels, pred.modelo, dnn=c("actual", "predicted"))
addmargins(cTab)

#Plot ROC Curve (ojo que a veces se queda trabado y hay que reiniciar R)
plot(1-analysis$specificities,analysis$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="black",lwd=2,
     main = "ROC")
abline(a=0,b=1)
abline(v = opt_t) #add optimal t to ROC curve


#Feel##############
summary(DB_analysis2020$Feel.median)

DL.covid$Feel[DL.covid$Feel == 11] <- 10
DL.covid$Feel <- as.numeric(DL.covid$Feel)

mean(DL.covid$Feel,na.rm = TRUE)
var(DL.covid$Feel,na.rm = TRUE)
table(DL.covid$Feel)
prob <- mean(DL.covid$Feel,na.rm = TRUE)/10

par(mfrow=c(1,1))
bar1 <- barplot(as.vector(table(DL.covid$Feel)),names.arg=seq(1:10), ylim=c(0,2000))
points(bar1,dnorm(seq(1,10),7.76,4.32)*sum(table(DL.covid$Feel)),cex=2,type="b",col="red",lwd=2,pch=19)
points(bar1,dbinom(seq(1,10), size = 10, prob = prob)*sum(table(DL.covid$Feel)),cex=2,type="b",col="sienna",lwd=2,pch=19)
points(bar1,dpois(seq(1,10),7.76)*sum(table(DL.covid$Feel)),cex=2,type="b",col="darkgreen",lwd=2,pch=19)

summary(DL.covid$Feel)

##feel activities
plotje <- ggplot(data = DB_analysis2020, aes(x = day))+
    geom_line(aes(y=Feel.median)) +
    scale_x_date(date_breaks = "2 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b", expand=c(0,0)) +
    theme_bw()+
    theme(axis.text.x=element_text(angle=45,hjust=1))
plotje
table(DL.covid$Feel_COVIDimpact)

#modelo individual covid/feel/outdoor --> hous is it associated to outdoor activities, yard, etc

summary(DL.covid$Feel)

DL.covid$Feel_COVIDimpact <- as.factor(DL.covid$Feel_COVIDimpact)
DL.covid$Feel_COVIDimpact <- relevel(DL.covid$Feel_COVIDimpact, ref = "Neutral")

summary(DL.covid$ruralurban_cat)
DL.covid$ruralurban_cat <- as.factor(DL.covid$ruralurban_cat)
DL.covid$ruralurban_cat <- relevel(DL.covid$ruralurban_cat, ref = "rural")
DL.covid$gender.2<- ifelse(DL.covid$gender == "Female", "Female", ifelse(DL.covid$gender == "Male", "Male", "Other"))

DL.covid.feel <- subset(DL.covid, !is.na(Feel) & day < "2020-07-31" )
DL.covid.feel <- subset(DL.covid.feel, !is.na(Feel_COVIDimpact) & !is.na(gender.2) & !is.na(age_r) & !is.na(Outdoor_YN) & !is.na(ruralurban_cat) & !is.na(SH.index10) & !is.na(cases100))

#DL.covid.feel$Out_Peridom[is.na(DL.covid.feel$Out_Peridom)]<-0
#DL.covid.feel$Out_Natarea[is.na(DL.covid.feel$Out_Natarea)]<-0

library(lme4)

fitglm.feel <- glmer(cbind(Feel,10-Feel) ~ Feel_COVIDimpact + SH.index10 + covidtot_7per100 + region + Outdoor_YN + ruralurban_cat + gender.2 + age_r + (1|user.id), family = binomial, data = DL.covid.feel,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.feel)

library("glmmTMB")
fitglm.feel.2 <- glmmTMB(Feel ~ Feel_COVIDimpact + SH.index10 + cases100 + Outdoor_YN + ruralurban_cat + gender + age_r + (1|user.id), family = truncated_compois(link = "log"), data = DL.covid.feel)
summary(fitglm.feel.2)

#the first one has the lowest AIC
res.2<-predict(fitglm.feel.2, type="response")
max(res.2)
res.2 <- cut(res.2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,Inf))
table(res.2)

res<-predict(fitglm.feel, type="response")*10
res <- cut(res, breaks=c(0,1,2,3,4,5,6,7,8,9,10))
point.res <-table(res)
point.res <- as.vector(point.res)
point.res <- append(point.res,0)
point.res <- append(point.res,0)
point.res


bar.feel <- table(DL.covid$Feel)
bar.feel <- as.vector(bar.feel)
bar.feel <- append(bar.feel,0)
bar.feel <- append(bar.feel,0)
bar.feel

bar1 <- barplot(bar.feel,names.arg=seq(1:12), ylim= c(0,1500))
points(bar1,point.res,cex=2,type="b",col="blue",lwd=2,pch=18)
points(bar1,table(res.2),cex=2,type="b",col="red",lwd=2,pch=20)


fitglm.feel.3 <- glm(cbind(Feel,10-Feel) ~ Feel_COVIDimpact + Outdoor_YN + SH.index10 + covidtot_7per100 + region + Outdoor_YN + ruralurban_cat + gender.2 + age_r, family = binomial, data = DL.covid.feel)
rms::vif(fitglm.feel.3)
anova(fitglm.feel,fitglm.feel.3)
summary(fitglm.feel)

ci<-confint(fitglm.feel, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.feel))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.feel),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

fitglm.feel.null <- glmer(cbind(Feel,10-Feel) ~ 1 + (1|user.id), family = binomial, data = DL.covid.feel,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.feel.null)
anova(fitglm.feel,fitglm.feel.null)

anova(fitglm.feel,fitglm.feel.null)

#interactions
library(lme4)

fitglm.feel <- glmer(cbind(Feel,10-Feel) ~ Feel_COVIDimpact + SH.index10 + covidtot_7per100 + Outdoor_YN*age_r + gender.2 + ruralurban_cat + (1|user.id), family = binomial, data = DL.covid.feel,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.feel)

DL.covid.feel$Tick_self_2 <- ifelse(DL.covid.feel$Tick_self == "No",0, ifelse(is.na(DL.covid.feel$Tick_self), NA, 1))


fitglm.feel <- glmer(cbind(Feel,10-Feel) ~ SH.index10 + covidtot_7per100 + Outdoor_YN*(age_r+Feel_COVIDimpact) + gender.2 + ruralurban_cat + Tick_self_2 + (1|user.id), family = binomial, data = DL.covid.feel,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.feel)

chisq.test(table(DL.covid.feel$Feel_COVIDimpact, DL.covid.feel$Outdoor_YN))

write.table(prop.table(table(DL.covid.feel$Feel_COVIDimpact, DL.covid.feel$Outdoor_YN), margin=2), "clipboard", sep="\t")

#auc, vif tables QQplot--------------
library(DHARMa)
par(mfrow=c(1,3))
set.seed(123)
simulationOutput <- simulateResiduals(fittedModel = fitglm.feel)
plot(simulationOutput, quantreg = F)

plotQQunif(simulationOutput)
plotResiduals(simulationOutput, quantreg = T)
testDispersion(simulationOutput)

names.vif <- list("Self-reported impact", "Did any outdoor activity", "SH index", "Covid cases", "Urbanicity", "Gender", "Age")
GVIF <- vif(fitglm.feel.3)
GVIF
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p4 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3,4), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 2),
                        breaks = c(1, 2),
                        labels = c("1", "2"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.15))

p4

#recreational activities
library(lme4)
fitglm.feel.rec <- glmer(cbind(Feel,10-Feel) ~ Feel_COVIDimpact + SH.index10 + cases100 + Out_Natarea + ruralurban_cat + gender.2 + age_r + (1|user.id), family = binomial, data = DL.covid.feel,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.feel.rec)

summary(fitglm.feel.rec)
ci<-confint(fitglm.feel.rec, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.feel.rec))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.feel.rec),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

#peridomestic activities
library(lme4)
fitglm.feel.peri <- glmer(cbind(Feel,10-Feel) ~ Feel_COVIDimpact + SH.index10 + cases100 + Out_Peridom + ruralurban_cat + gender.2 + age_r + (1|user.id), family = binomial, data = DL.covid.feel,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.feel.peri)

summary(fitglm.feel.peri)
ci<-confint(fitglm.feel.peri, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.feel.peri))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.feel.peri),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")




#Ticks on self ##############

summary(DL.covid$Tick_self)
DL.covid.tick <- subset(DL.covid, DL.covid$day > "2019-04-01" & DL.covid$day < "2019-07-31" | DL.covid$day > "2020-04-01" & DL.covid$day < "2020-07-31")

DL.covid.tick$Tick_self_2 <- ifelse(DL.covid.tick$Tick_self == "No",0, ifelse(is.na(DL.covid.tick$Tick_self), NA, 1))

DL.covid.tick$gender.2<- ifelse(DL.covid.tick$gender == "Female", "Female", ifelse(DL.covid.tick$gender == "Male", "Male", "Other"))

DL.covid.tick$year <- as.factor(DL.covid.tick$year)
DL.covid.tick$month <- as.factor(DL.covid.tick$month)

DL.covid.tick$ruralurban_cat <- relevel(DL.covid.tick$ruralurban_cat, ref = "rural")

DL.covid.tick <- subset(DL.covid.tick, !is.na(gender.2) & !is.na(age) & !is.na(Outdoor_YN) & !is.na(ruralurban_cat) & !is.na(year) & !is.na(month) & !is.na(user.id) & !is.na(Tick_self_2))


fitglm.tick <- glmer(Tick_self_2 ~ year + month + Outdoor_YN + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick)

fitglm.tick.2 <- glm(Tick_self_2 ~ year + month + ruralurban_cat + Outdoor_YN +  gender.2 + age, family = binomial, data = DL.covid.tick)
summary(fitglm.tick.2)

anova(fitglm.tick,fitglm.tick.2)

DL.covid.tick$year <- relevel(DL.covid.tick$year, ref = "2019")
DL.covid.tick$Outdoor_YN <- relevel(DL.covid.tick$Outdoor_YN, ref = "No, I didn't do any outdoor activities")
fitglm.tick.int.1a <- glmer(Tick_self_2 ~ (year + Outdoor_YN)*month + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.int.1a)

ci<-confint(fitglm.tick.int.1a, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick.int.1a))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick.int.1a),ci)), pval)
table.coef <- table.coef[3:6,1:4]
nombres <- as.vector(rownames(table.coef))
nombres[1] <- paste0("19yes_",nombres[1])
nombres[2:4] <- paste0("19no_",nombres[2:4])
rownames(table.coef) <- nombres
print(table.coef, digits = 2)
table.coef_19no <- table.coef

DL.covid.tick$year <- relevel(DL.covid.tick$year, ref = "2019")
DL.covid.tick$Outdoor_YN <- relevel(DL.covid.tick$Outdoor_YN, ref = "Yes")
fitglm.tick.int.1b <- glmer(Tick_self_2 ~ (year + Outdoor_YN)*month + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

ci<-confint(fitglm.tick.int.1b, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick.int.1b))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick.int.1b),ci)), pval)
table.coef <- table.coef[3:6,1:4]
nombres <- as.vector(rownames(table.coef))
nombres[1] <- paste0("19no_",nombres[1])
nombres[2:4] <- paste0("19yes_",nombres[2:4])
rownames(table.coef) <- nombres
print(table.coef, digits = 2)
table.coef_19yes <- table.coef

DL.covid.tick$year <- relevel(DL.covid.tick$year, ref = "2020")
DL.covid.tick$Outdoor_YN <- relevel(DL.covid.tick$Outdoor_YN, ref = "No, I didn't do any outdoor activities")
fitglm.tick.int.1c <- glmer(Tick_self_2 ~ (year + Outdoor_YN)*month + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ci<-confint(fitglm.tick.int.1c, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick.int.1c))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick.int.1c),ci)), pval)
table.coef <- table.coef[3:6,1:4]
nombres <- as.vector(rownames(table.coef))
nombres[1] <- paste0("20yes_",nombres[1])
nombres[2:4] <- paste0("20no_",nombres[2:4])
rownames(table.coef) <- nombres
print(table.coef, digits = 2)
table.coef_20no <- table.coef

DL.covid.tick$year <- relevel(DL.covid.tick$year, ref = "2020")
DL.covid.tick$Outdoor_YN <- relevel(DL.covid.tick$Outdoor_YN, ref = "Yes")
fitglm.tick.int.1d <- glmer(Tick_self_2 ~ (year + Outdoor_YN)*month + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.int.1d)
ci<-confint(fitglm.tick.int.1d, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick.int.1d))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick.int.1d),ci)), pval)
table.coef <- table.coef[3:6,1:4]
nombres <- as.vector(rownames(table.coef))
nombres[1] <- paste0("20no_",nombres[1])
nombres[2:4] <- paste0("20yes_",nombres[2:4])
rownames(table.coef) <- nombres
print(table.coef, digits = 2)
table.coef_20yes <- table.coef

fitglm.tick.null <- glmer(Tick_self_2 ~ 1 + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(fitglm.tick,fitglm.tick.int.1)
anova(fitglm.tick, fitglm.tick.2)
anova(fitglm.tick.int.1a, fitglm.tick.null)

ci<-confint(fitglm.tick, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

#forestplot-------

list.table.coef <- list(table.coef_19no,table.coef_19yes,table.coef_20no,table.coef_20yes)
forestdf <- do.call("rbind", list.table.coef)
forestdf <- data.frame(forestdf,stringsAsFactors = FALSE)
write.csv(forestdf,"Dailylog analysis/JAMA paper/forestdf.csv", row.names = FALSE)


forestdf <- read.csv("forestdf.csv")
nombres.forestdf <- list("2019 - Yes - April",
                         "2019 - No - May",
                         "2019 - No - June",
                         "2019 - No - July",
                         "2019 - No - April",
                         "2019 - Yes - May",
                         "2019 - Yes - June",
                         "2019 - Yes - July",
                         "2020 - Yes - April",
                         "2020 - No - May",
                         "2020 - No - June",
                         "2020 - No - July",
                         "2020 - No - April",
                         "2020 - Yes - May",
                         "2020 - Yes - June",
                         "2020 - Yes - July")
nombres.forestdf <- as.data.frame(unlist(nombres.forestdf))
colnames(nombres.forestdf) <- "Interaction"
forestdf <- cbind(nombres.forestdf,forestdf)
forestdf <- forestdf[c(5,2:4,1,6:8,13,10:12,9,14:16), ]
forestdf[1,2] <- 1
forestdf[1,3] <- 1
forestdf[1,4] <- 1

p <- ggplot(forestdf, aes(x = Interaction, y = OR, ymin = X2.5.., ymax = X97.5..)) +
    geom_vline(xintercept = c(1,3,5,7,9,11,13,15), colour = "grey95", size = 6)+
    geom_vline(xintercept = c(8.5), colour = "red", size = 0.75)+
    geom_vline(xintercept = c(4.5), colour = "blue", size = 0.25)+
    geom_vline(xintercept = c(12.5), colour = "blue", size = 0.25)+
    geom_pointrange(shape = 22, fill = "black") +
    geom_hline(yintercept = 1, linetype = 3)+
    #xlab("Variable") +
    ylab("Adjusted OR with 95% CI") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(forestdf$Interaction)) +
    scale_y_log10 (limits = c(0.03, 4),
                   breaks = c(0.1, 0.25, 0.5,1,2,4),
                   labels = c("0.1", "0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
    coord_flip()+
theme(axis.ticks.y = element_blank(),axis.line.y=element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),plot.margin = margin(0, 5, 0, 0))

p

forestdf$OR <- format(forestdf$OR, digits=2)
forestdf$X2.5.. <- format(forestdf$X2.5.., digits=1)
forestdf$X97.5.. <- format(forestdf$X97.5.., digits=2)

forestdf$`OR (CI95%)`<- paste0(forestdf$OR," (",forestdf$X2.5.., " - ",forestdf$X97.5..,")")
forestdf[1,6] <- 1

data_table <- ggplot(data = forestdf, aes(y = Interaction)) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2020 - Yes - July"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2020 - Yes - May"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2020 - No - July"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2020 - No - May"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2019 - Yes - July"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2019 - Yes - May"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2019 - No - July"), colour = "grey95", size = 6) +
    geom_hline(yintercept = which(rev(forestdf$Interaction)== "2019 - No - May"), colour = "grey95", size = 6) +
    geom_text(aes(x = 0, label = Interaction), hjust = 0) +
    geom_text(aes(x = 1, label = `OR (CI95%)`), hjust = 1) +
    #geom_text(aes(x = 7, label = arr), hjust = 1) +
    scale_colour_identity() +
    scale_y_discrete(limits = rev(forestdf$Interaction))+
    theme_void() +
    theme(plot.margin = margin(0, 0, 28, 0))

data_table

grid.arrange(data_table,p, ncol = 2,padding = NULL)

#auc, vif tables QQplot--------------
library(DHARMa)
par(mfrow=c(1,3))
set.seed(123)
simulationOutput <- simulateResiduals(fittedModel = fitglm.tick.int.1a)
res2 <- recalculateResiduals(simulationOutput, group = DL.covid.tick$user.id)
plot(res2, quantreg = F)

plotQQunif(res2)
plotResiduals(res2, quantreg = T)
testDispersion(res2)

names.vif <- list("Year", "Month", "Urbanicity", "Did any outdoor activity", "Gender", "Age")
GVIF <- vif(fitglm.tick.2)
GVIF
graph.vif <- data.frame(cbind(names.vif,GVIF))
graph.vif$names.vif <- as.character(graph.vif$names.vif)
graph.vif$GVIF <- as.numeric(graph.vif$GVIF)

p4 <- ggplot(graph.vif, aes(x = names.vif, y = GVIF, group = names.vif)) +
    geom_vline(xintercept = c(1,3,5,7,9), colour = "grey95", size = 5)+
    geom_point(size=2) +
    geom_hline(yintercept = c(1,2,3,4), linetype = 3)+
    xlab("Variable") +
    ylab("VIF") +
    theme_classic() +
    scale_colour_identity() +
    scale_x_discrete(limits = rev(graph.vif$names.vif)) +
    scale_y_continuous (limits = c(0.75, 2),
                        breaks = c(1, 2),
                        labels = c("1", "2"), expand = c(0,0)) +
    coord_flip()+
    geom_text(size=3,label=round(graph.vif$GVIF, 2),position = position_nudge(y = 0.25))

p4

#labels es el valor observado (1 o 0) en valor numerico, no categorizado
#library (ROCR)
predMA <-predict(fitglm.tick.int.1a, type="response")
predMA<-as.numeric(predMA)
labels<-(DL.covid.tick$Tick_self_2)

#guardar los resultados en un csv
resultado<-data.frame(predMA, labels)
resultado
#aplicar la curva de ROC, analysis estima el poder de clasificaci?n (el ?rea debajo de la curva de ROC)
analysis <- roc(labels,predMA)
analysis

#Bucar el valor de corte ?timo (Find t that minimizes error)
e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]
opt_t

#calculo sensibilidad
tp <- sum( (predMA > opt_t) & labels)
tp
sensitivity <- tp / sum (labels)
sensitivity

#calculo especificidad
tn <- sum( (predMA <= opt_t) & (!labels))
tn
specificity <- tn / sum(!labels)
specificity

#tabla de clasificaci?n
umbral<-opt_t
pred.modelo<- cut(predMA,breaks=c(-Inf , umbral , Inf), labels=c("no inf","inf"))
cTab<-table(labels, pred.modelo, dnn=c("actual", "predicted"))
addmargins(cTab)

#Plot ROC Curve (ojo que a veces se queda trabado y hay que reiniciar R)
plot(1-analysis$specificities,analysis$sensitivities,type="l",
     ylab="Sensitiviy",xlab="1-Specificity",col="black",lwd=2,
     main = "ROC")
abline(a=0,b=1)
abline(v = opt_t) #add optimal t to ROC curve


##peridomestic-----------

fitglm.tick.peri <- glmer(Tick_self_2 ~ year + month + Out_Peridom + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.peri)

fitglm.tick.peri.2 <- glm(Tick_self_2 ~ year + month + ruralurban_cat + Out_Peridom +  gender.2 + age, family = binomial, data = DL.covid.tick)
summary(fitglm.tick.peri.2)
vif(fitglm.tick.peri.2)

anova(fitglm.tick.peri,fitglm.tick.peri.2)

fitglm.tick.peri.int.1 <- glmer(Tick_self_2 ~ year*month + Out_Peridom + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.peri.int.1)

fitglm.tick.peri.int.2 <- glmer(Tick_self_2 ~ month*(year + Out_Peridom) + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.peri.int.2)
#not diff from above

anova(fitglm.tick.peri,fitglm.tick.peri.int.1)
anova(fitglm.tick.peri.int.1,fitglm.tick.peri.int.2)

ci<-confint(fitglm.tick.peri, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick.peri))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick.peri),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")

##rec------------

fitglm.tick.rec <- glmer(Tick_self_2 ~ year + month + Out_Natarea + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.rec)

fitglm.tick.rec.2 <- glm(Tick_self_2 ~ year + month + ruralurban_cat + Out_Natarea +  gender.2 + age, family = binomial, data = DL.covid.tick)
summary(fitglm.tick.2)
vif(fitglm.tick.2)

anova(fitglm.tick.rec,fitglm.tick.rec.2)

fitglm.tick.rec.int.1 <- glmer(Tick_self_2 ~ year*month + Out_Natarea + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.rec.int.1)

fitglm.tick.rec.int.2 <- glmer(Tick_self_2 ~ month*(year + Out_Natarea) + ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.rec.int.2)

fitglm.tick.rec.int.3 <- glmer(Tick_self_2 ~ month*year + Out_Natarea*ruralurban_cat + gender.2 + age + (1|user.id), family = binomial, data = DL.covid.tick,control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(fitglm.tick.rec.int.3)


anova(fitglm.tick.rec,fitglm.tick.rec.int.1)
anova(fitglm.tick.rec,fitglm.tick.rec.int.2)
anova(fitglm.tick.rec.int.1,fitglm.tick.rec.int.2)
anova(fitglm.tick.rec.int.1,fitglm.tick.rec.int.3)

ci<-confint(fitglm.tick.rec, parm="beta_", method = "Wald")
pval<-as.numeric(format(round((coef(summary(fitglm.tick.rec))[,'Pr(>|z|)']),2), scientific = FALSE))
table.coef <- cbind(exp(cbind(OR=fixef(fitglm.tick.rec),ci)), pval)
print(table.coef, digits = 2)
write.table(table.coef, "clipboard", sep="\t")


###-----
####------------limpiar ----> to export data
##generate complete dataset for dates
ts <- seq.POSIXt(as.POSIXct("2018-04-20",'%Y/%m/%d'), as.POSIXct("2020-07-01",'%Y/%m/%d'), by="day")
ts <- seq.POSIXt(as.POSIXlt("2018-04-20"), as.POSIXlt("2020-07-31"), by="day")
ts <- format.POSIXct(ts,'%Y/%m/%d')
df <- data.frame(day=ts)
df$day <- as.character.POSIXt(df$day)
library(lubridate)
df$day <- as.Date(df$day, "%Y/%m/%d")
df$region_MidWest <- NA
df$region_Northeast <- NA
df$year <- year(df$day)
df$month <- month(df$day)
df$week <- week(df$day)
library(splitstackshape)
df <- merged.stack(df, id.vars="day",
                   var.stubs=c("region"), sep = "_")
df <- select(df, -region)
colnames(df)[which(names(df) == ".time_1")] <- "region"
df <- data.frame(df)
df$id <- paste(df$year,df$week,df$region)
df <- moveMe(df, "region", where = "last", ba= NULL)
df <- moveMe(df, "id", where = "first", ba= NULL)
#remove duplicates
df <- subset(df[!duplicated(df$id),])
write.csv(df, "Dailylog analysis/JAMA paper/SumCovidempty.csv",row.names=F)


##merge with the summary database
SumDL.covid <- merge(SumDL,df, by.x = c("id"), by.y = c("id"), all.y = TRUE)
SumDL.covid$region.x <- as.character(SumDL.covid$region.x)
SumDL.covid$region.x <- ifelse(is.na(SumDL.covid$region.x), SumDL.covid$region.y, SumDL.covid$region.x)
SumDL.covid$year.x <- ifelse(is.na(SumDL.covid$year.x), SumDL.covid$year.y, SumDL.covid$year.x)
SumDL.covid$week.x <- ifelse(is.na(SumDL.covid$week.x), SumDL.covid$week.y, SumDL.covid$week.x)
SumDL.covid <- SumDL.covid[order(SumDL.covid$region.x, SumDL.covid$year.x, SumDL.covid$week.x),]
colnames(SumDL.covid)[which(names(SumDL.covid) == "region.x")] <- "region"
colnames(SumDL.covid)[which(names(SumDL.covid) == "week.x")] <- "week"
colnames(SumDL.covid)[which(names(SumDL.covid) == "year.x")] <- "year"
SumDL.covid <- select(SumDL.covid, -region.y, -week.y, -year.y, -id)

SumDL.covid$prop.Outdoor <- ifelse(SumDL.covid$N<10 | is.na(SumDL.covid$N), NA, SumDL.covid$AnyOutdoor/SumDL.covid$N)
SumDL.covid$prop.Rec <- ifelse(SumDL.covid$N<10 | is.na(SumDL.covid$N), NA, SumDL.covid$Rec/SumDL.covid$N)
SumDL.covid$prop.Peri <- ifelse(SumDL.covid$N<10 | is.na(SumDL.covid$N), NA, SumDL.covid$Peri/SumDL.covid$N)

SumDL.covid <- moveMe(SumDL.covid, "day", where = "after", ba="week")
SumDL.covid <- moveMe(SumDL.covid, "month", where = "after", ba= "year")

write.csv(SumDL.covid, "Dailylog analysis/JAMA paper/SumCovid_by_reg.csv",row.names=F)

#subset to 2019-20 data only
SumDL.covid.1920 <- subset(SumDL.covid, SumDL.covid$day > "2019-02-01")

#subset to 2020 data only
SumDL.covid.20 <- subset(SumDL.covid, SumDL.covid$day > "2020-01-01")
write.csv(SumDL.covid, "Outdoor_TickApp/OutdoorTickApp_byregion_2020.csv",row.names=F)

#Graph
plotje <- ggplot(data = SumDL.covid.1920, aes(x = day))+
    geom_line(aes(y=Users, linetype = "Total active users")) +
    geom_line(aes(y=N, color = "Total surveys")) +
    geom_line(aes(y=AnyOutdoor, color= "Any Ooutdoor act.")) +
    #geom_line(aes(y=Rec, color = "Recreational act.")) +
    #geom_line(aes(y=Peri, color = "Peridomestic act.")) +
    ylab("Number of surveys") +
    xlab("date") +
    scale_linetype_manual("",
                          breaks = c("Total active users"),
                          values = c("dashed")) +
    scale_colour_manual("",
                        breaks = c("Total surveys", "Any Ooutdoor act.", "Recreational act.", "Peridomestic act."),
                        values = c("red", "blue", "green", "gray12", "gray0")) +
    theme_bw()+
    facet_grid(region~., scales="free_y")
plotje
ggsave("DL_count_time_regions.png")


covid_table <- read.table(text =
                              "  Start        End
                          1 2020-03-21 2020-04-02
                          ", header = TRUE,
                          stringsAsFactors = FALSE)
covid_table$Start <- as.Date(covid_table$Start)
covid_table$End <- as.Date(covid_table$End)

plotje <- ggplot(data = SumDL.covid.1920, aes(x = day))+
    geom_line(aes(y=prop.Outdoor, color= "Any Ooutdoor act.")) +
    geom_line(aes(y=prop.Rec, color = "Recreational act. in green spaces")) +
    geom_line(aes(y=prop.Peri, color = "Peridomestic act.")) +
    #geom_line(aes(y=prop.Other, color = "Other outdoor act.")) +
    ylab("Proportion of surveys") +
    xlab("date") +
    scale_colour_manual("Reported outdoor activities",
                        breaks = c("Any Ooutdoor act.", "Recreational act. in green spaces", "Peridomestic act.", "Other outdoor act.*"),
                        values = c("red","blue", "darkgreen","gray0")) +
    scale_x_date(date_breaks = "3 month",
                 date_minor_breaks = "1 month",
                 date_labels = "%Y-%b") +
    theme_bw()+
    geom_rect(data= covid_table, inherit.aes = FALSE,
              aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf,
                  fill="Effective dates"),colour=NA, alpha=0.5)+
    scale_fill_manual('Statewide stay-at-home orders
                      in the Northeast and Midwest',
                      values = 'pink') +
    facet_grid(region~., scales="free_y")
plotje

#plotje + geom_vline(xintercept = as.numeric(as.Date("2020-03-03"))) + geom_vline(xintercept = as.numeric(as.Date("2020-04-03")))
ggsave("DL_prop_time_regions.png")


#---------------------
##By CDC region

DL.covid$CDCregion[DL.covid$user.state == "Wisconsin" |
                       DL.covid$user.state == "Michigan" |
                       DL.covid$user.state == "Minnesota" |
                       DL.covid$user.state == "Illinois" |
                       DL.covid$user.state == "Ohio" |
                       DL.covid$user.state == "Indiana"] <- "Reg 5"

DL.covid$CDCregion[DL.covid$user.state == "Iowa" |
                       DL.covid$user.state == "Kansas" |
                       DL.covid$user.state == "Missouri" |
                       DL.covid$user.state == "Nebraska"] <- "Reg 7"

DL.covid$CDCregion[DL.covid$user.state == "Pennsylvania" |
                       DL.covid$user.state == "West Virginia" |
                       DL.covid$user.state == "Virginia" |
                       DL.covid$user.state == "Maryland" |
                       DL.covid$user.state == "Delaware" ] <- "Reg 3"


DL.covid$CDCregion[DL.covid$user.state == "New York" |
                       DL.covid$user.state == "New Jersey"] <- "Reg 2"

DL.covid$CDCregion[DL.covid$user.state == "Connecticut" |
                       DL.covid$user.state == "Maine" |
                       DL.covid$user.state == "Vermont" |
                       DL.covid$user.state == "New Hampshire" |
                       DL.covid$user.state == "Massachusetts" |
                       DL.covid$user.state == "Rhode Island"] <- "Reg 1"

DL.covid$CDCregion[DL.covid$user.state == "Nevada" |
                       DL.covid$user.state == "California" |
                       DL.covid$user.state == "Nevada"|
                       DL.covid$user.state == "Hawaii"] <- "Reg 9"

DL.covid$CDCregion[DL.covid$user.state == "Washington" |
                       DL.covid$user.state == "Oregon" |
                       DL.covid$user.state == "Idaho"|
                       DL.covid$user.state == "Alaska"] <- "Reg 10"

DL.covid$CDCregion[DL.covid$user.state == "Montana" |
                       DL.covid$user.state == "North Dakota" |
                       DL.covid$user.state == "South Dakota"|
                       DL.covid$user.state == "Colorado"|
                       DL.covid$user.state == "Utah"|
                       DL.covid$user.state == "Wyoming"] <- "Reg 8"

DL.covid$CDCregion[DL.covid$user.state == "New Mexico" |
                       DL.covid$user.state == "Texas" |
                       DL.covid$user.state == "Oklahoma"|
                       DL.covid$user.state == "Arkansas"|
                       DL.covid$user.state == "Louisiana"] <- "Reg 6"

DL.covid$CDCregion[DL.covid$user.state == "Kentuky" |
                       DL.covid$user.state == "Tennessee" |
                       DL.covid$user.state == "Alabama"|
                       DL.covid$user.state == "Georgia"|
                       DL.covid$user.state == "Florida"|
                       DL.covid$user.state == "South Carolina"|
                       DL.covid$user.state == "North Carolina"|
                       DL.covid$user.state == "Mississippi"] <- "Reg 4"

table(DL.covid$CDCregion)

# Summary
SumDL <- dplyr::summarize(group_by(DL.covid, year, week, CDCregion),
                          N = n(),
                          Users = n_distinct(user.id),
                          AnyOutdoor = length(which(Outdoor_YN == "Yes")),
                          num.male = length(which(gender == "Male")),
                          num.female = length(which(gender == "Female")),
                          age.median = median(age, na.rm = TRUE),
                          age.IQR = IQR (age, na.rm = TRUE),
                          Rec = sum(Out_Natarea, na.rm = TRUE),
                          Peri = sum(Out_Peridom, na.rm = TRUE),
                          Other_outdoor = sum(Out_Notick, na.rm = TRUE),
                          Unknown_risk = sum(Out_Unknownrisk, na.rm = TRUE)
)
SumDL <- subset(SumDL, CDCregion == "Reg 1" | CDCregion == "Reg 2" | CDCregion == "Reg 3" | CDCregion == "Reg 5")

SumDL$id <- paste(SumDL$year,SumDL$week, SumDL$CDCregion)

##generate complete dataset for dates
ts <- seq.POSIXt(as.POSIXct("2018-04-20",'%Y/%m/%d'), as.POSIXct("2020-07-01",'%Y/%m/%d'), by="day")
ts <- seq.POSIXt(as.POSIXlt("2018-04-20"), as.POSIXlt("2020-07-01"), by="day")
ts <- format.POSIXct(ts,'%Y/%m/%d')
df <- data.frame(day=ts)
df$day <- as.character.POSIXt(df$day)
library(lubridate)
df$day <- as.Date(df$day, "%Y/%m/%d")
df$`CDCregion_Reg 1` <- NA
df$`CDCregion_Reg 2` <- NA
df$`CDCregion_Reg 3` <- NA
df$`CDCregion_Reg 5` <- NA
df$year <- year(df$day)
df$month <- month(df$day)
df$week <- week(df$day)
library(splitstackshape)
df <- merged.stack(df, id.vars="day",
                   var.stubs=c("CDCregion"), sep = "_")
df <- select(df, -CDCregion)
colnames(df)[which(names(df) == ".time_1")] <- "CDCregion"
df <- data.frame(df)
df$id <- paste(df$year,df$week,df$CDCregion)
df <- moveMe(df, "CDCregion", where = "last", ba= NULL)
df <- moveMe(df, "id", where = "first", ba= NULL)
#remove duplicates
df <- subset(df[!duplicated(df$id),])
write.csv(df, "SumCovidempty.csv",row.names=F)


##merge with the summary database
SumDL.covid <- merge(SumDL,df, by.x = c("id"), by.y = c("id"), all.y = TRUE)
SumDL.covid$CDCregion.x <- ifelse(is.na(SumDL.covid$CDCregion.x), SumDL.covid$CDCregion.y, SumDL.covid$CDCregion.x)
SumDL.covid$year.x <- ifelse(is.na(SumDL.covid$year.x), SumDL.covid$year.y, SumDL.covid$year.x)
SumDL.covid$week.x <- ifelse(is.na(SumDL.covid$week.x), SumDL.covid$week.y, SumDL.covid$week.x)
SumDL.covid <- SumDL.covid[order(SumDL.covid$CDCregion.x, SumDL.covid$year.x, SumDL.covid$week.x),]
colnames(SumDL.covid)[which(names(SumDL.covid) == "CDCregion.x")] <- "CDCregion"
colnames(SumDL.covid)[which(names(SumDL.covid) == "week.x")] <- "week"
colnames(SumDL.covid)[which(names(SumDL.covid) == "year.x")] <- "year"
SumDL.covid <- select(SumDL.covid, -CDCregion.y, -week.y, -year.y, -id)

SumDL.covid$prop.Outdoor <- ifelse(SumDL.covid$N<10 | is.na(SumDL.covid$N), NA, SumDL.covid$AnyOutdoor/SumDL.covid$N)
SumDL.covid$prop.Rec <- ifelse(SumDL.covid$N<10 | is.na(SumDL.covid$N), NA, SumDL.covid$Rec/SumDL.covid$N)
SumDL.covid$prop.Peri <- ifelse(SumDL.covid$N<10 | is.na(SumDL.covid$N), NA, SumDL.covid$Peri/SumDL.covid$N)

SumDL.covid <- moveMe(SumDL.covid, "day", where = "after", ba="week")
SumDL.covid <- moveMe(SumDL.covid, "month", where = "after", ba= "year")

write.csv(SumDL.covid, "SumCovid_by_CDCreg.csv",row.names=F)

#subset to 2019-20 data only
SumDL.covid.1920 <- subset(SumDL.covid, SumDL.covid$day > "2019-02-01")

#subset to 2020 data only
SumDL.covid.20 <- subset(SumDL.covid, SumDL.covid$day > "2020-01-01")
write.csv(SumDL.covid, "Outdoor_TickApp/OutdoorTickApp_byCDCregion_2020.csv",row.names=F)