#Dataset from data.gov
#Data from: The influence of active video game play upon physical activity and screen-based activities in sedentary children
#By : Agricultural Research Service
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(micromap)
#micromap gave me the right() to get last n characters, substring() will get me first n characters
#start 11 pm 3/22
exp_data <- read_xlsx('24 hour recalldata.xlsx')
exp_data <- as.data.frame(exp_data)
date_check <- as.data.frame(exp_data)
head(date_check,2)

#formats are mostly wrong, going to convert the characters into nums and make the date a date
#getting rid of subheader that split columns into hours and minutes
exp_data[,c(6:31)] <- sapply(exp_data[,6:31],as.numeric)
exp_data <- exp_data[2:nrow(exp_data),]
names(exp_data)
exp_data[,3] <- as.Date(exp_data[,3],"%m/%d/%y")
date_check[,3]

#going to multiply hour column by 60 to just have minutes
#getting rid of NAs, will check hours to find 0s in columns where a 0 doesn't make sense later
names(exp_data)
exp_data[is.na(exp_data)] <- 0
exp_data1 <- exp_data %>% mutate(active_video_game = `(2A.1)  How long did you play active video games?`*60 + ...7,
                                 non_active_video_game = `(2B.1) How long did you play non-active video games?`*60+...9,
                                 watching_TV = `(3.1)  How long did you watch TV/DVD?`*60+...11,
                                 computer = `(4.1) How long did you use the computer?`*60+...13,
                                 homework = `(5.1) How much time did you spend doing your homework?`*60+...15,
                                 social_situations = `(6.1) How much time did you spend in social activities?`*60+...17,
                                 other_hobbies = `(7.1) How much time did you spend in other hobbies?`*60+...19,
                                 transportation = `(8.1) How much time did you spend in other transportation (car, bus, etc)?`*60+...21,
                                 school = `(9.1)  How much time did you spend in school?`*60+...23,
                                 sports_or_active_games = `(10.1)  How much time did you spend in Sports/Active Games?`*60+...25,
                                 walking_or_biking = `(11.1)  How much time did you spend Walking or Biking?`*60+...27,
                                 household_chores = `(12.1)  How much time did you spend in Household Chores?`*60+...29,
                                 Physical_Education_Class = `(13.1)  How much time did you spend in Physical Education Class?`*60+...31)
names(exp_data1)
exp_data <- exp_data1 %>% select(`Volunteer Number:`,`Week:`,`Date:`,`(1A)   What time did you go to bed?`,`(1B)    What time did you get up?`,active_video_game,non_active_video_game,watching_TV,computer,homework,social_situations,other_hobbies,
                                 transportation,school,sports_or_active_games,walking_or_biking,household_chores,Physical_Education_Class)

#checking to see if I removed NAs in columns wehre it doesnt make sense
# if count is low, I will eliminate those rows

nrow(exp_data)
exp_data2 <- exp_data %>% filter(`(1A)   What time did you go to bed?` != 0 & `(1B)    What time did you get up?` != 0)
nrow(exp_data2)
exp_data <- exp_data2


#have combined hours and minutes, and gotten rid of entries where there used to be NAs
#will continute later, stopping 1230am, an hour and a half in
#restarting 3pm , going to reformat sleeping column 

exp_data$`(1A)   What time did you go to bed?`
str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}')
sum(str_detect(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'))

exp3 <- exp_data
exp_data$`(1A)   What time did you go to bed?`[str_detect(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'9','09')
exp_data$`(1A)   What time did you go to bed?`[str_detect(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'8','08')
exp_data$`(1A)   What time did you go to bed?`[str_detect(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'0 P','00 P')
exp_data$`(1A)   What time did you go to bed?`[str_detect(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'2 P','00 P')
(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'))


str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'9','09')  
str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'8','08')
str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'0 P','00 P')
str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{1}:\\d{1}'),'2 P','00 P')


exp_data$`(1A)   What time did you go to bed?`[str_detect(exp_data$`(1A)   What time did you go to bed?`,'^\\d{2}:\\d{3} PM')] <- str_replace(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{2}:\\d{3}'),'00 P','0 P')
(str_subset(exp_data$`(1A)   What time did you go to bed?`,'^\\d{2}:\\d{3}'))


exp_data$`(1B)    What time did you get up?`[str_detect(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}'),'7:','07:')
exp_data$`(1B)    What time did you get up?`[str_detect(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}'),'9:','09:')
exp_data$`(1B)    What time did you get up?`[str_detect(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}')] <- str_replace(str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}'),'6:','06:')
str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{1}:\\d{1}')


exp_data$`(1B)    What time did you get up?`[str_detect(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} A')] <- str_replace(str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} A'),'0 A','00 A')
exp_data$`(1B)    What time did you get up?`[str_detect(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} A')] <- str_replace(str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} A'),'2 A','00 A')
str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} A')

exp_data$`(1B)    What time did you get up?`[str_detect(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} P')] <- str_replace(str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} P'),'0 P','00 P')
str_subset(exp_data$`(1B)    What time did you get up?`,'^\\d{2}:\\d{1} P')
exp_data$`(1B)    What time did you get up?`

#now the go to sleep and get up times are both in hh:mm AM/PM format
#stopping at 430, founds hours slept! took a while tho
#some people slept more than 24 hrs...
bed <- format(as.POSIXct(exp_data$`(1A)   What time did you go to bed?`,format='%I:%M %p'),format="%H:%M:%S")
bed
wake <-format(as.POSIXct(exp_data$`(1B)    What time did you get up?`,format='%I:%M %p'),format="%H:%M:%S")
wake
HoursSlept <- -as.difftime(bed,units = 'hours')+(as.difftime(wake,units = 'hours')+24)

#add it on to the dataset
exp_data <- mutate(exp_data,HoursSlept = as.numeric(-as.difftime(format(as.POSIXct(exp_data$`(1A)   What time did you go to bed?`,format='%I:%M %p'),format="%H:%M:%S"),units = 'hours') +
                                  (as.difftime(format(as.POSIXct(exp_data$`(1B)    What time did you get up?`,format='%I:%M %p'),format="%H:%M:%S"),units ='hours')+24)))

#dont know which entries are which
#check to find index
#just found out importing dataset made dates go up 3years? weird
#going off on a limb and assuming no one slept more than 12 hrs, taking 12 hours away from those who
#slept so much, if still over 16, removing 12 more
exp_hrs <- exp_data
exp_hrs$HoursSlept
exp_hrs$HoursSlept[exp_hrs$HoursSlept>16] <- exp_hrs$HoursSlept[exp_hrs$HoursSlept>16]-12
exp_hrs$HoursSlept
exp_hrs$HoursSlept[exp_hrs$HoursSlept>16]
which(exp_hrs$HoursSlept>16)
exp_hrs$HoursSlept[exp_hrs$HoursSlept>16]

#overiding dataset since I am now comfortable with these sleeping ranges
#leaving commnets since RMarkdownReport wont have this in it
exp_data$HoursSlept[exp_data$HoursSlept>16] <- exp_data$HoursSlept[exp_data$HoursSlept>16]-12
anyNA()
#str_replace('1234'1,'^','0')
#str_replace('1234','$','5')

exp_data <- exp_data %>% select(-c(`(1A)   What time did you go to bed?`,`(1B)    What time did you get up?`))
 #dataset is finaly clean, took around 4 hours off, learned so much though
head(exp_data)
exp_data %>% group_by(`Week:`) %>% summarize(time_on_comp = mean(computer))                          
#something in computer is driving it wayyy too high
exp_data$computer[exp_data$computer==1800] <- exp_data$computer[exp_data$computer==1800]/60
exp_data %>% group_by(`Week:`) %>% summarize(time_on_comp = mean(computer)) %>% arrange(time_on_comp)
mean(exp_data$computer[exp_data$`Week:`==10])
#exp_data <- exp_data1
 exp_data
exp_data$`Week:` <- as.numeric(exp_data$`Week:`)
exp_data %>% group_by(`Week:`) %>% summarize(time_on_comp = mean(computer)) %>% arrange(`Week:`)
#there we go king
#this dataset is now sparkling clean
#I can work with this data now
#values are in minutes
#I will work on adding demographics at some other time
exp_data %>% group_by(`Week:`) %>% summarize(mean(active_video_game),mean(watching_TV)) %>% arrange(`Week:`)

#Going to assume entries of 1800 minutes were supposed to be 30 minutes entries, not 30 hour entries
exp_data[exp_data == 1800] <- 1800/60
names(exp_data)
exp_data %>% ggplot(aes(x = `Week:`,y = active_video_game)) + geom_smooth(mapping = aes(x = `Week:`,y = active_video_game))
exp_data %>% ggplot() + geom_point(mapping = aes(x = active_video_game,y = watching_TV,color = `Week:`)) + geom_smooth(method =lm,aes(x = active_video_game,y = watching_TV,color = `Week:`))+facet_grid(~`Week:`)+xlim(0,85)+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = active_video_game,y = computer,color = `Week:`)) + geom_smooth(method =lm,aes(x = active_video_game,y = computer,color = `Week:`))+facet_grid(~`Week:`)+xlim(0,85)+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = `Week:`,y = computer)) + geom_smooth(aes(x = `Week:`,y = computer),color = 'red')+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = `Week:`,y = watching_TV)) + geom_smooth(aes(x = `Week:`,y = watching_TV),color = 'red')+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = `Week:`,y = non_active_video_game)) + geom_smooth(aes(x = `Week:`,y = non_active_video_game),color = 'red')+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = `Week:`,y = walking_or_biking)) + geom_smooth(aes(x = `Week:`,y = walking_or_biking),color = 'red')+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = `Week:`,y = social_situations)) + geom_smooth(aes(x = `Week:`,y = social_situations),color = 'red')+ylim(0,500)
exp_data %>% ggplot() + geom_point(mapping = aes(x = `Week:`,y = sports_or_active_games)) + geom_smooth(aes(x = `Week:`,y = sports_or_active_games),color = 'red')+ylim(0,500)


exp_data %>% group_by(`Week:`) %>% summarize(mean(sports_or_active_games),mean(active_video_game),mean(watching_TV),mean(walking_or_biking),mean(non_active_video_game),mean(social_situations)) %>% arrange(`Week:`)
