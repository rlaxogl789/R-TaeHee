#Let's  pick only real estate category from data 
questions <- read.csv("questions.csv", header=TRUE)
View(questions)

attorneytimeentries<-read.csv("attorneytimeentries.csv", header=TRUE)

#let's omit the null data 
questions[questions == "NULL"] = NA
questionswithoutnull <- na.omit(questions)

attorneytimeentries[attorneytimeentries == "NULL"] = NA
attorneytimeentrieswithoutnull <- na.omit(attorneytimeentries)


#extractbeforecovid attorney time 
extractyear3<-substring(attorneytimeentrieswithoutnull$EnteredOnUtc,1,4)
extractbeforecovid1<-subset(attorneytimeentrieswithoutnull, extractyear3=="2017")
extractbeforecovid2<-subset(attorneytimeentrieswithoutnull, extractyear3=="2018")
extractbeforecovid3<-subset(attorneytimeentrieswithoutnull, extractyear3=="2019")
extractbeforecovid<-rbind(extractbeforecovid1,extractbeforecovid2,extractbeforecovid3)
View(extractbeforecovid)

#extractaftercovid attorney time 
extractyear3<-substring(attorneytimeentrieswithoutnull$EnteredOnUtc,1,4)
extractaftercovid1<-subset(attorneytimeentrieswithoutnull, extractyear3=="2020")
extractaftercovid2<-subset(attorneytimeentrieswithoutnull, extractyear3=="2021")
extractaftercovid3<-subset(attorneytimeentrieswithoutnull, extractyear3=="2022")
extractaftercovid<-rbind(extractaftercovid1,extractaftercovid2,extractaftercovid3)
View(extractaftercovid)

#create new csv file for attorney 
write.csv(extractbeforecovid, "extractbeforecovid(attorney).csv", row.names=FALSE, quote=FALSE) 
write.csv(extractaftercovid, "extractaftercovid(attorney).csv", row.names=FALSE, quote=FALSE)





#extractbeforecovid
extractyear<-substring(questionswithoutnull$AskedOnUtc,1,4)
extractbeforecovid1<-subset(questionswithoutnull, extractyear=="2017")
extractbeforecovid2<-subset(questionswithoutnull, extractyear=="2018")
extractbeforecovid3<-subset(questionswithoutnull, extractyear=="2019")
extractbeforecovid<-rbind(extractbeforecovid1,extractbeforecovid2,extractbeforecovid3)
View(extractbeforecovid)

#extractaftercovid
extractyear<-substring(questionswithoutnull$AskedOnUtc,1,4)
extractaftercovid1<-subset(questionswithoutnull, extractyear=="2020")
extractaftercovid2<-subset(questionswithoutnull, extractyear=="2021")
extractaftercovid3<-subset(questionswithoutnull, extractyear=="2022")
extractaftercovid<-rbind(extractaftercovid1,extractaftercovid2,extractaftercovid3)
View(extractaftercovid)

#create new csv file
write.csv(extractbeforecovid, "extractbeforecovid.csv", row.names=FALSE, quote=FALSE) 
write.csv(extractaftercovid, "extractaftercovid.csv", row.names=FALSE, quote=FALSE) 

#calculate the time difference before covid 
questionsDifferencebeforecovid <- difftime(extractbeforecovid$ClosedOnUtc, xtractbeforecovid$AskedOnUtc, units = "hours")
questionsDifferencebeforecovid





extractmonth<-substring(questions$AskedOnUtc,6,7)
extractmonth
View(extractmonth)
questionsMay<- subset(questions, extractmonth=="05")
View(questionsMay)
questionsMay
nrow(questionsMay)
#subtraction of dates
questionsMay$Difference <- difftime(questionsMay$ClosedOnUtc, questionsMay$AskedOnUtc, units = "hours")

difftime(questionsMay$AskedOnUtc,questionsMay$ClosedOnUtc, units = "hours")


questionshousing<-questions[questions$Category == 'Housing and Homelessness',]
questionshousing
View(questionshousing)
nrow(questionshousing)
#34755 number of demands concerning Housing and Homelessness
#most frequent values in subset 
names(which.max(table(questionshousing$StateAbbr)))
#Florida has the most demand for legal service concerning Housing and homelessness

#Florida has 3660 demands for the legal service concerning Housing and homelessness 
sum(questionshousing$StateAbbr == 'FL', na.rm=TRUE)
sum(questionshousing$StateAbbr == 'CA', na.rm=TRUE)
sum(questionshousing$StateAbbr == 'TX', na.rm=TRUE)
sum(questionshousing$StateAbbr == 'UT', na.rm=TRUE)

#Why CA lowest? that might be related to the client income 

#what specific legal issues are people demanding in Housing category? 
sum(questionshousing$Subcategory == 'Housing or Property Owned
', na.rm=TRUE)

#table function 쓰면 더 쉽게 찾을 수 있음. 
table(questionshousing$Subcategory)
#'Housing or property owned' accounts for the most 

#which month is the most frequent in questionshousing 
extractmonth2<-substring(questionshousing$AskedOnUtc,6,7)
extractmonth2
questionsMay<- subset(questionshousing, extractmonth2=="05")
nrow(questionsMay)
questionsJanuary<- subset(questionshousing, extractmonth2=="01")
nrow(questionsJanuary)
questionsFebruary<- subset(questionshousing, extractmonth2=="02")
nrow(questionsFebruary)
questionsMarch<- subset(questionshousing, extractmonth2=="03")
nrow(questionsMarch)
questionsApril<- subset(questionshousing, extractmonth2=="04")
nrow(questionsApril)
questionsJune<- subset(questionshousing, extractmonth2=="06")
nrow(questionsJune)
questionsJuly<- subset(questionshousing, extractmonth2=="07")
nrow(questionsJuly)
questionsAugust<- subset(questionshousing, extractmonth2=="08")
nrow(questionsAugust)
questionsSeptember<- subset(questionshousing, extractmonth2=="09")
nrow(questionsSeptember)
questionsOctober<- subset(questionshousing, extractmonth2=="10")
nrow(questionsOctober)
questionsNovember<- subset(questionshousing, extractmonth2=="11")
nrow(questionsNovember)
questionsDecember<- subset(questionshousing, extractmonth2=="12")
nrow(questionsDecember)
#August, September, October has the most number of demands regarding Housing 



#remove values with NULL
questions[questions == "NULL"] = NA
questionswithoutnull <- na.omit(questions)

View(questionswithoutnull)
nrow(questionswithoutnull)
nrow(questions)
#california, Michigan, 

#Average hours spent per state 
df <- data.frame(StateAbbr +attorney_time$StateAbbr, 
                 Hours = attorney_time$Hours)
df_avg <- aggregate(Hours ~ StateAbbr, data = df, FUN = mean )

#calculate correlation between income and performance in 7 states. 

#create new csv file for attorney 

write.csv(extractbeforecovid, "extractbeforecovid(attorney).csv", row.names=FALSE, quote=FALSE) 
write.csv(extractaftercovid, "extractaftercovid(attorney).csv", row.names=FALSE, quote=FALSE)

afterminusbefore_avg <- read.csv("afterminusbefore_avg.csv", header=TRUE)
sevenstates<- subset(afterminusbefore_avg, afterminusbefore_avg$DIfference>0)
sevenstates
View(sevenstates)

write.csv(sevenstates, "sevenstates.csv", row.names=FALSE, quote=FALSE)

#correlation analysis 
correlation<-read.csv("correlation.csv", header=TRUE)
cor(correlation$DIfference,correlation$inc_avg_change)
plot(correlation$DIfference,correlation$inc_avg_change)


