
# approve/disapprove with donald trump
thesis$Trump <- thesis$POL1DT_W37

table(thesis$Trump)

thesis <- thesis%>%
  mutate(Trump=recode(Trump, "Approve"=1,"Disapprove"=2))
table(thesis$Trump)

# importance of congress -> combining 1+2 = matters & 3+4 = does not matter
thesis$Matterscong <- thesis$MATTERSCONG_W37

thesis <- thesis %>%
  mutate(Matterscong=recode(MATTERSCONG_W37,
                            "1 Really matters which party wins control of Congress"=1,
                            "2" = 1,
                            "3" = 2,
                            "4 Doesn't really matter which party wins control of Congress"=2))
table(thesis$Matterscong)

# recoding If the elections for the U.S. House of Representatives were being held TODAY, would you vote for…
unique(thesis$CONG_W37)
thesis$TodayCong <- thesis$CONG_W37
table(thesis$TodayCong)
thesis<- thesis %>%
  mutate(TodayCong=recode(CONG_W37,
                          "The Democratic Party\x92s candidate in your district"=1,
                          "The Republican Party\x92s candidate in your district"=2,
                          "Not sure"=3,
                          "Another candidate" =4   ))
table(thesis$TodayCong)

# recoding CONGA: As of TODAY, do you lean more to…

thesis$AsTodayCong <- thesis$CONGA_W37
table(thesis$AsTodayCong)

thesis <- thesis %>%
  mutate(AsTodayCong=recode(CONGA_W37, 
                            "The Democratic Party\x92s candidate in your district" =1,
                            "The Republican Party\x92s candidate in your district" =2,
                            "Not sure" =3,
                            "Lean to another candidate" =4
                            ))
table(thesis$AsTodayCong) # there is a lot of NA in this variable 

# news platforms; a=print , b = radio

thesis$PrintNews <- thesis$NEWS_PLATFORMA_W37
thesis <- thesis %>%
  mutate(PrintNews=recode(NEWS_PLATFORMA_W37, 
                          "Often" = 1,
                          "Sometimes" = 2,
                          "Hardly ever" = 3,
                          "Never" = 4))

# radio
thesis$RadioNews<- thesis$NEWS_PLATFORMB_W37

thesis <- thesis %>%
  mutate( RadioNews = recode( NEWS_PLATFORMB_W37,
                              "Often" = 1,
                              "Sometimes" = 2,
                              "Hardly ever" = 3,
                              "Never" = 4 ))


# TV 

thesis$TvNewsHabit <- thesis$NEWS_PLATFORMD_W37
table(thesis$NEWS_PLATFORMD_W37)

thesis <- thesis %>%
  mutate( TvNewsHabit = recode( NEWS_PLATFORMD_W37,
                              "Often" = 1,
                              "Sometimes" = 2,
                              "Hardly ever" = 3,
                              "Never" = 4 ))
table(thesis$TvNewsHabit)

# national evening tv news

thesis$NationalEveningTv <-  thesis$NEWS_PLATFORME_W37
table( thesis$NEWS_PLATFORME_W37)

thesis <- thesis %>%
  mutate( NationalEveningTv = recode( NEWS_PLATFORME_W37,
                                "Often" = 1,
                                "Sometimes" = 2,
                                "Hardly ever" = 3,
                                "Never" = 4 ))
table(thesis$NationalEveningTv)

# f - cable news

thesis$CableNews <- thesis$NEWS_PLATFORMF_W37
table(thesis$NEWS_PLATFORMF_W37)

thesis <- thesis %>%
  mutate( CableNews= recode( NEWS_PLATFORMF_W37,
                                      "Often" = 1,
                                      "Sometimes" = 2,
                                      "Hardly ever" = 3,
                                      "Never" = 4 ))
table(thesis$CableNews)


# social media news source

thesis$SocialMediaNews <-  thesis$NEWS_PLATFORMG_W37
table(thesis$NEWS_PLATFORMG_W37)

thesis <- thesis %>%
  mutate( SocialMediaNews= recode( NEWS_PLATFORMG_W37,
                             "Often" = 1,
                             "Sometimes" = 2,
                             "Hardly ever" = 3,
                             "Never" = 4 ))
table(thesis$SocialMediaNews)

# website news or app

thesis$WebNews <- thesis$NEWS_PLATFORMH_W37
table(thesis$NEWS_PLATFORMH_W37)

thesis <- thesis %>%
  mutate( WebNews= recode( NEWS_PLATFORMH_W37,
                                   "Often" = 1,
                                   "Sometimes" = 2,
                                   "Hardly ever" = 3,
                                   "Never" = 4 ))
table(thesis$WebNews)


# news preferences
table(thesis$NEWS_PREFER_W37)
unique(thesis$NEWS_PREFER_W37)

thesis$NewsPrefer <- thesis$NEWS_PREFER_W37
table(thesis$NewsPrefer)


thesis <- thesis %>%
  mutate( NewsPrefer= recode( NEWS_PREFER_W37,
                              "Watching news on television" = 1 ,
                              "Getting news from a news website or app"  = 2,
                              "Reading news in a print newspaper"  = 3,
                              "Listening to news on the radio"  = 4 ,
                              "Getting news from a social media site (such as Facebook, Twitter or Snapchat)"=5))]

#How often do you watch news on a TV using a streaming device

table(thesis$STREAMNEWS_W37)
thesis$StreamNews <- thesis$STREAMNEWS_W37

table(thesis$StreamNews)
thesis <- thesis %>%
  mutate( StreamNews= recode( STREAMNEWS_W37,
                           "Often" = 1,
                           "Sometimes" = 2,
                           "Hardly ever" = 3,
                           "Never" = 4 ))


# I have ignored News_plafrom2 variables because they are full of empty !!

# And how often do you get news…

thesis$OfLocal<- thesis$TVNEWSA_W37
table(thesis$TVNEWSA_W37)
unique(thesis$TVNEWSA_W37)
table(thesis$TVNEWSB_W37)

#/- because there is a lot of NA, i have ignored them too ... -/

# Whether online or offline, do you prefer to get your news by…

table(thesis$NEWS_FORM_W37)
unique(thesis$NEWS_FORM_W37)

thesis$NewsType <- thesis$NEWS_FORM_W37

thesis <- thesis %>%
  mutate(NewsType= recode(thesis$NEWS_FORM_W37,
                          "Reading it" =1,
                          "Listening to it" = 2,
                          "Watching it" = 3 ,
                          "Refused" = NA_real_ ))
table(thesis$NewsType)

table(thesis$SNSA_W37)
unique(thesis$SNSA_W37)
names(thesis)

#Do you use any of the following social media sites?

thesis$UseFacebook<-  thesis$SNSA_W37

thesis$UseTwitter<-  thesis$SNSB_W37

thesis$UseLinkedIn<-  thesis$SNSD_W37

thesis$UseInstagram<-  thesis$SNSE_W37

thesis$UseInstagram<-  thesis$SNSE_W37

thesis$UseTumblr<-  thesis$SNSG_W37

thesis$UseYouTube<-  thesis$SNSH_W37

thesis$UseReddit<-  thesis$SNSI_W37

thesis$UseSnapchat<-  thesis$SNSJ_W37

thesis$UseWhatsApp<-  thesis$SNSK_W37

#SNSNEWS [S] Do you ever get news or news headlines on any of the following sites?
# By news we mean information about events and issues that involve more than just your friends or family.
# Using Recoder package!

#facebook
table(thesis$SNSNEWSA_W37)
thesis$FacebookPrefer <- thesis$SNSNEWSA_W37

thesis$FacebookPrefer <- recoder(thesis$FacebookPrefer, '"No":2;"Yes":1;"Refused":0',other = NULL,na=NA)
table(thesis$FacebookPrefer)

# Twitter

thesis$TwitterPrefer <- thesis$SNSNEWSB_W37
thesis$TwitterPrefer <- recoder(thesis$TwitterPrefer, '"No":0;"Yes":1',other = NA)

table(thesis$TwitterPrefer)

# LinkedIN

thesis$LinkedInPrefer <- thesis$SNSNEWSD_W37
thesis$LinkedInPrefer <- recoder(thesis$LinkedInPrefer, '"No":0;"Yes":1',other = NA)

table(thesis$LinkedInPrefer)

# Instagram

thesis$InstaPrefer <- thesis$SNSNEWSE_W37
thesis$InstaPrefer <- recoder(thesis$InstaPrefer, '"No":0;"Yes":1',other = NA)

table(thesis$InstaPrefer)

# Tumblr


thesis$TumblrPrefer <- thesis$SNSNEWSG_W37
thesis$TumblrPrefer <- recoder(thesis$TumblrPrefer,'"No":0;"Yes":1',other = NA)

table(thesis$TumblrPrefer)

# YouTube

thesis$YouTubePrefer <- thesis$SNSNEWSH_W37
thesis$YouTubePrefer <- recoder(thesis$YouTubePrefer, '"No":0;"Yes":1',other = NA)

table(thesis$YouTubePrefer)

# Reddit

thesis$RedditPrefer <- thesis$SNSNEWSI_W37
thesis$RedditPrefer <- recoder(thesis$RedditPrefer,'"No":0;"Yes":1',other = NA)

table(thesis$RedditPrefer)

# Snapchat

thesis$SnapchatPrefer <- thesis$SNSNEWSJ_W37
thesis$SnapchatPrefer <- recoder(thesis$SnapchatPrefer,'"No":0;"Yes":1',other = NA)

table(thesis$SnapchatPrefer)

# WhatsApp

thesis$WhatsAppPrefer <- thesis$SNSNEWSK_W37
thesis$WhatsAppPrefer <- recoder(thesis$WhatsAppPrefer,'"No":0;"Yes":1',other = NA)

table(thesis$WhatsAppPrefer)

# Social media news overal: Overall, would you say news on social media has…

thesis$SocialMediaOveral<- thesis$SNSINFORM_W37
table(thesis$SocialMediaOveral)
unique(thesis$SocialMediaOveral)

library(dplyr)

thesis <- thesis %>%
  mutate(SocialMediaOveral= recode(SNSINFORM_W37,
                                   "Not made much of a difference" =0,
                                   "Helped you better understand current events"=1,
                                   "Made you more confused about current events"=-1,
                                   "Refused"= NA_real_))
table(thesis$SocialMediaOveral)
table(thesis$SNSINFORM_W37)

# SNSSKEP [S] Which of the following best describes how you approach news stories from social media sites?

unique(thesis$SNSSKEP_W37)
thesis$SocialMediaAccuracy <- thesis$SNSSKEP_W37
unique(thesis$SocialMediaAccuracy
       )

thesis <- thesis%>%
  mutate( SocialMediaAccuracy= recode( SNSSKEP_W37,
                                       "Largely be inaccurate" = -1,
                                       "Largely be accurate" = 1,
                                       "Refused" = NA_real_))
table(thesis$SNSSKEP_W37)
table(thesis$SocialMediaAccuracy)

# wmnprz: Do you think it would be a [show in same order as response list: (good thing) or a (bad thing)] for the country 
# if a DEMOCRATIC WOMAN was elected president?

table(thesis$WMNPRZ2_W37)
unique(thesis$WMNPRZ2_W37)

thesis$DemoWomanPresident <- thesis$WMNPRZ2_W37

thesis<- thesis %>%
  mutate( DemoWomanPresident= recode( WMNPRZ2_W37,
                                  "Neither good nor bad"=0 ,
                                  
                                  "Very bad" = -2,
                                  "Somewhat bad" = -1,
                                  "Very good" = 2,
                                  "Somewhat good" = 1,
                                  "Refused"= NA_real_  ))
table(thesis$DemoWomanPresident)
library(ggplot2)
ggplot(thesis,aes(DemoWomanPresident)) + geom_histogram(binwidth = 0.5)

# Republican woman president:

thesis$RepWomanPresident <- thesis$WMNPRZ3_W37


thesis<- thesis %>%
  mutate( RepWomanPresident= recode( WMNPRZ3_W37,
                                      "Neither good nor bad"=0 ,
                                      
                                      "Very bad" = -2,
                                      "Somewhat bad" = -1,
                                      "Very good" = 2,
                                      "Somewhat good" = 1,
                                      "Refused"= NA_real_  ))
table(thesis$WMNPRZ3_W37)
table(thesis$RepWomanPresident)
ggplot(thesis,aes(RepWomanPresident)) + geom_histogram() +
  labs(title = "Frequency of Republican Woman President")


write.csv(thesis,
          "thesis 020202",row.names = FALSE)

library(dplyr)
 
# Ideoself: Where would you place YOURSELF on this same scale from 0 to 10?

thesis<-read.csv("thesis 020202.csv", header = T)
table(thesis$IDEOSELF_W37)
unique(thesis$IDEOSELF_W37)

table(thesis$THERMIRAN_W37)

#creating a new variable for idealogy
thesis$Ideology <- thesis$IDEOSELF_W37
thesis <- thesis %>%
  mutate( Ideology=recode( IDEOSELF_W37,
                           "0 Very conservative"= 0,
                           "10 Very liberal"=10,
                           "Refused"=NA_real_,
                           "1"=1,
                           "2"=2,
                           "3"=3,
                           "4"=4,
                           "5"=5,
                           "6"=6,
                           "7"=7,
                           "8"=8,
                           "9"=9))
table(thesis$Ideology)

unique(thesis$THERMIRAN_W37)
thesis$AttitudeIran<- thesis$THERMIRAN_W37
thesis$AttitudeIran<- as.numeric(thesis$AttitudeIran)
class(thesis$AttitudeIran)
unique(thesis$AttitudeIran)
summary(thesis$AttitudeIran)

table(thesis$Ideology)


# doing my first oredered logistics regression by MASS package:
library(MASS)
library(recoder
        )

logit <- subset(thesis,select = c(AttitudeIran,Ideology))
str(logit)
logit$Ideology<- as.factor(logit$Ideology)
logit$AttitudeIran<- as.factor(logit$AttitudeIran)
model <- polr(AttitudeIran ~ Ideology, data = logit)

logit$AttitudeIran <- ifelse(logit$AttitudeIran <= 25, -2,
                             ifelse(logit$AttitudeIran <= 50, -1,
                                    ifelse(logit$AttitudeIran <= 75, 1, 2)))

table(logit$AttitudeIran)

summary(model)
qplot(model)
library(ggplot2)



# plotting:

df <- data.frame(
  ideology = factor(0:9, levels = 0:9),
  estimate = c(-0.1478, 0.2837, 0.4713, 1.2533, 1.3231, 1.7657, 1.7626, 1.9687, 2.2505, 2.2291)
)

library(ggplot2)
ggplot(df, aes(x = ideology, y = estimate)) +
  geom_point() +
  geom_line() +
  xlab("Ideology") +
  ylab("Estimated Effect on Attitude Toward Iran") +
  ggtitle("Relationship Between Ideology and Attitude Toward Iran")


# ---- recoding other variables:
table(thesis$F_SEX_FINAL)

thesis$Sex <- thesis$F_SEX_FINAL

thesis <- thesis %>%
  mutate( Sex= recode( F_SEX_FINAL,
                       "Female"=0,
                       "Male"=1))
table(thesis$Sex)

#  F_EDUCCAT_FINAL Three-way category coded from self-reported educational attainment.
thesis$Edu1<- thesis$F_EDUCCAT_FINAL

table(thesis$Edu1)

thesis <- thesis %>%
  mutate( Edu1= recode( F_EDUCCAT_FINAL,
                        "H.S. graduate or less"=1,
                        "College graduate+"=3,
                        "Some college"=2,
                        "Don't know/Refused"=NA_real_))

#F_EDUCCAT2_FINAL Six-way category coded from self-reported educational attainment.

thesis$Edu2 <- thesis$F_EDUCCAT2_FINAL
 
thesis <- thesis %>%
  mutate( Edu2= recode( F_EDUCCAT2_FINAL,
                        "Less than high school" =1,
                        "High school graduate" =2,
                        "Some college, no degree" =3,
                        "Associate\x92s degree"=4,
                        "College graduate/some postgrad"=5,
                        "Postgraduate" =6,
                        "Don\x92t know/Refused" = NA_real_))
table(thesis$Edu2)

getwd()

# wendsday

write.csv(thesis,"thesis 020204",row.names = FALSE)

thesis <- read.csv(file.choose(), header = T)

head(thesis$AttitudeIran)
tail(thesis$AttitudeIran)

glimpse(thesis$Edu2)
library(dplyr)


# scatterplot
library(ggplot2)
ggplot(thesis, aes(x=Edu2, y=AttitudeIran)) + 
  geom_point() 
  labs(x="Education Level", y="Attitudes Toward Iran")
  
  
library(MASS)


  df_analysis <- data.frame(AttitudeIran = thesis$AttitudeIran,
                            Edu2 = thesis$Edu2)

  model <- polr(AttitudeIran ~ Edu2, data = df_analysis)
  
  df_analysis$AttitudeIran <- as.factor(df_analysis$AttitudeIran)
  summary(model)
  library(effects)
  library(effects)
  plot(effect("Edu2", model), type = "probability")
  
  

