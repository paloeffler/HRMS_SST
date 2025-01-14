### Mass accuracy evaluation for long-term system suitability in non-target and suspect screening
  #Paul Löffler, Svante Rehnstam, Lutz Ahrens, Foon Yin Lai, Alberto Celma
  #Department of Aquatic Sciences and Assessment, Swedish University of Agricultural Sciences (SLU), Uppsala, SE-75007, Sweden

required_packages <- c("ggplot2", "data.table", "dplyr", "reshape2", "readr", "lubridate")
missing_packages <- setdiff(required_packages, installed.packages()[, "Package"])
if (length(missing_packages)) install.packages(missing_packages)
lapply(required_packages, library, character.only = TRUE)

orig.data <-  read_excel("ADJUST PATH..../Löffler_Rehnstam_SST_MS_data.xlsx", 
                         skip = 2)

data <- orig.data %>% setDT()
data <- data[,c(1,2,16:36)]
colnames(data) <- c("Time","Replicate","Column","Org modifier","MP additive","Name","Date","Accept/reject","Batch Injections","Comment","152",
                    "195","237","287","441","455","502","716","262","312","412","426","497") 
data$Date <- as.factor(data$Date)
data2 <- data[complete.cases(data[,Name])]
data2[Time =="Between", Time :="After"]
data2 <- data2[Date != "2023-03-14"]

Batch <- rep(0,nrow(data2))
Batch[1] <- 1
for (i in 2:nrow(data2)){
  if(data2$Time[i] =="Before" & data$Time[i-1] == "After"){
    Batch[i] <- Batch[i-1]+1
  }
  else {
    Batch[i] <- Batch[i-1]
  }
}
data2$Batch <- Batch

data.pos <- data2[,c(1:18,24)]


data.pos2 <- data.table::melt(data.pos, id.vars = c("Time","Replicate","Column","Org modifier","MP additive","Name","Date","Accept/reject","Batch Injections","Comment","Batch"),
                              measure.vars = c("152", "195","237","287","441","455","502","716"),
                              variable.name = "Mass", value.name = "massdev")

data.pos2 <- data.pos2[massdev != 1000000]
data.pos2$polarity <- "positive"

data.neg <- data2[,c(1:10,19:24)]
data.neg2 <- melt(data.neg, id.vars = c("Time","Replicate","Column","Org modifier","MP additive","Name","Date","Accept/reject","Batch Injections","Comment","Batch"),
                  measure.vars = c("262","312","412","426","497"), variable.name = "Mass",value.name = "massdev") %>% setDT()
data.neg2 <- data.neg2[massdev != 1000000]
data.neg2$polarity <- "negative"
data.neg2[massdev > 3.5, massdev := 0.6]

data <- rbind(data.pos2,data.neg2)
sum <- data %>% group_by(Mass) %>% summarise(.,polarity=unique(polarity),Mean = mean(massdev), SD = sd(massdev))
print(sum)
sum$Injections <- 5
overall.sum <- sum %>% group_by(polarity) %>% summarize(., mean=mean(Mean), SD = sd(SD))
print(overall.sum)

data2 <- data[,.SD[1:2], by = .(Time,Batch,Mass)]
data1 <- data[,.SD[1], by = .(Time,Batch,Mass)]
sum2 <- data2 %>% group_by(Mass) %>% summarise(.,polarity=unique(polarity),Mean = mean(massdev), SD = sd(massdev))
print(sum2)
sum2$Injections <- 2
sums <- rbind(sum,sum2)
overall.sum2 <- sum2 %>% group_by(polarity) %>% summarize(., mean=mean(Mean), SD = sd(SD))
print(overall.sum2)

sdata <- data
sdata$Date <- as.Date(sdata$Date)
sdata$Name <- as.factor(sdata$Name)
sdata$polarity <- as.factor(sdata$polarity)
sdata$Time <- as.factor(sdata$Time)
sdata$Column <- as.factor(sdata$Column)
sdata$`Org modifier` <- as.factor(sdata$`Org modifier`)
sdata$`MP additive` <- as.factor(sdata$`MP additive`)
sdata$decimaldate <- decimal_date(sdata$Date) %>% as.numeric()
sdata <- sdata %>% setDT()


lm_model.152 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 152, ] )
summary(lm_model.152)
lm_model.152.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 152  & `Batch Injections` < 100, ] )
summary(lm_model.152.2)
lm_model.195 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 195, ] )
summary(lm_model.195)
lm_model.195.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 195 & `Batch Injections` < 100, ] )
summary(lm_model.195.2)
lm_model.237 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 237, ] )
summary(lm_model.237)
lm_model.237.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 237 & `Batch Injections` < 100, ] )
summary(lm_model.237.2)
lm_model.287 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 287, ] )
summary(lm_model.287)
lm_model.287.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 287& `Batch Injections` < 100, ] )
summary(lm_model.287.2)
lm_model.441 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 441 , ] )
summary(lm_model.441)
lm_model.441.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 441& `Batch Injections` < 100, ] )
summary(lm_model.441.2)
lm_model.455 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 455, ] )
summary(lm_model.455)
lm_model.455.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 455 & `Batch Injections` < 100, ] )
summary(lm_model.455.2)
lm_model.502 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 502, ] )
summary(lm_model.502)
lm_model.502.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 502 & `Batch Injections` < 100, ] )
summary(lm_model.502.2)
lm_model.716 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 716, ] )
summary(lm_model.716)
lm_model.716.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 716 & `Batch Injections` < 100, ] )
summary(lm_model.716.2)

lm_model.262 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 262, ] )
summary(lm_model.262)
lm_model.262.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 262 & `Batch Injections` < 100, ] )
summary(lm_model.262.2)
lm_model.312 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 312, ] )
summary(lm_model.312)
lm_model.312.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 312 & `Batch Injections` < 100, ] )
summary(lm_model.312.2)
lm_model.412 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 412, ] )
summary(lm_model.412)
lm_model.412.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 412 & `Batch Injections` < 100, ] )
summary(lm_model.412.2)
lm_model.426 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 426, ] )
summary(lm_model.426)
lm_model.426.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 426 & `Batch Injections` < 100, ] )
summary(lm_model.426.2)
lm_model.497 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 497, ] )
summary(lm_model.497)
lm_model.497.2 <- lm(massdev ~Time*`Batch Injections`, data = sdata[sdata$Mass == 497 & `Batch Injections` < 100, ] )
summary(lm_model.497.2)

lm.model.pos <- lm(massdev~ Time*`Batch Injections` , data = sdata[sdata$polarity == "positive", ])
summary(lm.model.pos)
lm.model.neg <- lm(massdev~ Time*`Batch Injections` , data = sdata[sdata$polarity == "negative", ])
summary(lm.model.neg)


orig.caldata <- read_excel("ADJUST PATH..../Löffler_Rehnstam_SST_Calibration_data.xlsx", col_names = T) %>% setDT()
caldata <- melt(data = orig.caldata,id.vars = c("Date") , measure.vars = c("cal nom (pos)","cal den (pos)","cal nom (neg)","cal den (neg)","flex nom (pos)","flex den (pos)",
                                                                           "flex nom (neg)","flex den (neg)"), value.name = "value", variable.name = "calname") %>% setDT()
caldata <- caldata %>% mutate(polarity = ifelse(grepl("pos", calname), "positive", "negative"))
caldata <- caldata %>% mutate(fraction = ifelse(grepl("nom", calname), "nominator", "denominator"))
caldata <- caldata %>% mutate(type = ifelse(grepl("flex", calname),"flex","normal"))
caldata$decimaldate <- decimal_date(caldata$Date)

caldata.n <- caldata
caldata.n$Date <- as.factor(caldata.n$Date)

caldata2 <- dcast(caldata, decimaldate + polarity ~ calname, value.var = "value")
sdata2 <- full_join(sdata, caldata2, by = c("decimaldate","polarity"))
sdata3 <- sdata2[complete.cases(sdata2$Time),] 
cal <- caldata[,c(3,7)]
cal$value <- as.integer(cal$value > 0.2)
cal <- unique(na.omit(cal))
cal.mass <- merge(cal, sdata , by = "decimaldate", all = T)
sdata3$caldist <- ifelse(rowSums(!is.na(sdata3[, 16:23])) != 0, 0, NA)
for (i in 1:nrow(sdata3)) {
  if (is.na(sdata3$caldist[i])) {
    smaller_entry <- sdata3$decimaldate[which(sdata3$decimaldate < sdata3$decimaldate[i] & rowSums(!is.na(sdata3[, 16:23])) != 0)]
    if (length(smaller_entry) > 0) {
      sdata3$caldist[i] <- sdata3$decimaldate[i] - max(smaller_entry)
    }
  }
}

sdata3$caldist <- sdata3$caldist *365.25

any_above_0.3 <- apply(sdata3[, 16:23] > 0.3, 1, function(x) any(x, na.rm = TRUE))
any_above_0.3 <- ifelse(any_above_0.3, "above", "below")
caldata3 <- caldata2
caldata3$calibtype <- ifelse(rowSums(!is.na(caldata3[, 3:10])) > 2, "normal+flex", "normal")
table(caldata3$calibtype)

sdata3b <- sdata3 %>% setDT()
sdata3b$calibtype <- ifelse(rowSums(!is.na(sdata3b[, 16:23])) == 0, NA,
                            ifelse(rowSums(!is.na(sdata3b[, 16:23])) == 2, "normal",
                                   "normal+flex"))

for (i in 2:nrow(sdata3b)) {
  if (is.na(sdata3b$calibtype[i])) {
    sdata3b$calibtype[i] <- sdata3b$calibtype[i-1]
  }
}

sdata3c <- sdata3b %>% setDT()
if (!"calqual" %in% colnames(sdata3c)) {
  sdata3c[, calqual := character()]
}

for (i in 1:nrow(sdata3c)) {
  if (any(!is.na(sdata3c[i, 16:23]) & sdata3c[i, 16:23] > 0.3)) {
    sdata3c[i, calqual := "bad"]
  } else if (any(!is.na(sdata3c[i, 16:23]) & sdata3c[i, 16:23] <= 0.3)) {
    sdata3c[i, calqual := "good"]
  } else {
    sdata3c[i, calqual := NA]
  }
}

for (i in 2:nrow(sdata3c)) {
  if (is.na(sdata3c$calqual[i])) {
    sdata3c$calqual[i] <- sdata3c$calqual[i-1]
  }
}


sdata3c$calqual <- as.factor(sdata3c$calqual)
sdata3c$calibtype <- as.factor(sdata3c$calibtype)
sdata3c <- within(sdata3c, calqual <- relevel(calqual, ref = "good"))
sdata3c <- within(sdata3c, calibtype <- relevel(calibtype, ref = "normal+flex"))

cal.152 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 152, ] )
summary(cal.152)
cal.195 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 195, ] )
summary(cal.195)
cal.237 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 237, ] )
summary(cal.237)
cal.287 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 287, ] )
summary(cal.287)
cal.441 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 441, ] )
summary(cal.441)
cal.455 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 455, ] )
summary(cal.455)
cal.502 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 502, ] )
summary(cal.502)
cal.716 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 716, ] )
summary(cal.716)

cal.262 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 262, ] )
summary(cal.262)
cal.312 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 312, ] )
summary(cal.312)
cal.412 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 412, ] )
summary(cal.412)
cal.426 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 426, ] )
summary(cal.426)
cal.497 <- lm(abs(massdev) ~ caldist*calqual+calibtype, data = sdata3c[Mass == 497, ] )
summary(cal.497)

sdata4 <- sdata3[,c("Time","Replicate","Batch Injections","Batch","Mass","massdev","polarity")]
sdata4 <- sdata4 %>% group_by(Batch,`Batch Injections`,Time, Mass,polarity) %>% summarize(mean_massdev = mean(massdev), .groups = 'drop')
sdata4 <- reshape2::dcast(sdata4, Batch + Mass +  `Batch Injections`~ Time, value.var = "mean_massdev") %>% setDT()
sdata4[,diff := After - Before] %>% setDT()
sdata4a <- sdata4[ `Batch Injections` < 100]
