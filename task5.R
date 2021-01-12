## importing required library

library("dplyr")
library("tibble")
library("ggplot2")


#read both the dataset.

deli= read.csv("E:/dataset/deliveries.csv")
matches= read.csv("E:/dataset/matches.csv")

summary(matches)

#converting  dataset into dataframe

df<-data.frame(matches)
view(df)
summary(df)

DF<-data.frame(deli)
view(DF)

#see the dimension

dim(matches)

#find the total seasons played 

a=unique(matches$season)
View(a)

#So total 12 season are there in our dataset.


#count the total no. of matches won by each team in each season.

c<- df %>%
  group_by(season,winner) %>%
  summarise("Total win"=n())

View(c)

#Lets see in which season most no. of matches were played

matches %>%
  group_by(season) %>%
  summarise(wins=n()) %>%
ggplot(aes(x=season,y=wins,label=wins))+
geom_bar(stat = "identity")+
  geom_text(position=position_stack(vjust=0.5))+
  scale_y_continuous("Total Matches Played In each season")

#we can cross check the values 

matches %>%
  group_by(season) %>%
  summarise(Total_Matches_Played=n()) %>%
  filter(Total_Matches_Played==max(Total_Matches_Played))


##count the total no. of matches Win by each team.


d<- df %>% 
  group_by(winner)%>% 
  summarise(wins=n())

View(d)

d<- d[-c(1),]

##plot the above graph

matches %>%
  group_by(winner) %>%
  summarise(Total_win=n()) %>%
  ggplot(aes(x=winner,y=Total_win,fill=winner,label=Total_win))+
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_text(position=position_stack(vjust=0.5))+
  scale_y_continuous("Total Matches Won By Each Team")


#From this we came to know that Mumbai Indians is most successfull team.

##Toss dependency


## Lets check what % of caption choose when they won the toss.

e<- df%>%
  group_by(toss_winner,winner) %>%
  summarise(toss_decision)

View(e)

E<- as.data.frame(table(matches$toss_decision))
E<- mutate(E,percentage = (Freq/sum(Freq))*100)
pie(E$Freq, labels = round(E$percentage), main = "Toss Decision in percent", col = c("black","white"))
legend("topright", c("BAT","FIELD"), cex=0.9,fill=c("black","white"))

view(E)

#matches%>%
#  group_by(toss_winner)%>%
#  summarise(toss_decision)%>%
#  ggplot(aes(x=toss_decision,y=winner))+
#  geom_bar(stat = "identity")+
#  coord_flip()

##top 10 batsmans who scored most run


f<-deli %>%
  group_by(batsman) %>%
  summarise(runs= sum(batsman_runs)) %>%
  arrange(-runs)%>%
  head(10)

View(f)


#plot

f %>%
  ggplot(aes(x=runs,y=batsman,label=runs))+
  geom_bar(stat = "identity",fill="orange")+
  geom_text(position=position_stack(vjust=0.5))+
  theme_classic()
  

             
##Top 10 batsman who scored highest runs in a single match

g <- deli%>%
  group_by(batsman,match_id)%>%
  summarise(Runs= sum(batsman_runs))%>%
  arrange(-Runs)%>%
  head(10)%>%
  select(batsman, Runs)
  
  

view(g)
  



  
## Checking in which city most of the matches were played 

ggplot(matches[which(!is.na(matches$city)),],
       aes(city,fill=city,rm.na=T))+
        geom_bar()+
        guides(TRUE)+
        coord_flip()+
        theme_minimal()+
       ylab("Total No. Of Matches Played In City")

## Checking in which stadium most of the matches were played 


ggplot(matches[which(!is.na(matches$city)),],
       aes(venue,rm.na=T))+geom_bar()+
  guides(TRUE)+
  coord_flip()+
  theme_minimal()+
  ylab("Total No. Of Matches Played In Statidum")


## NOw lets see teams which has scored highest runs in a single match from season 08-19

h <- deli %>%
  group_by(match_id,batting_team)%>%
  summarise(Runs= sum(total_runs))%>%
  arrange(-Runs)%>%
  head(15)


view(h)

## plot above data

h%>%  
  ggplot(aes(reorder(Runs,batting_team),batting_team))+
  geom_bar(stat = "identity",width = 0.03,alpha=0.2)+
  geom_point(size=5, alpha=0.2,shape=20,stroke=5)+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90))


## Now lets see which player like to hit more runs against which team

i <- deli%>%
  group_by(bowling_team,batsman)%>%
  summarise(Total_Runs = sum(batsman_runs))%>%
  arrange(-Total_Runs)%>%
  group_by(bowling_team)%>%
  slice(which.max(Total_Runs))
  
view(i)

##Lets see which bowler does batsman like to play to hit most runs

o<- deli %>%
  group_by(batsman,bowler)%>%
  summarise(Total_Runs= sum(table(batsman_runs)))%>%
  arrange(-Total_Runs)%>%
 head(20)

view(o)



  
## Lets see the most successful bowler who put most maiden overs.


j <- as.data.frame(deli%>%
                     group_by(bowler,match_id,over)%>%
                     summarise(Runs  = sum(total_runs))%>% 
                     filter(Runs==0)%>%
                     ungroup()%>%
                     select(bowler)%>%
                     table())%>%
  
  arrange(-Freq)

view(j)

colnames(j) = c("Bowler","No")

j %>%
  filter(No > 5)%>%
  ggplot()+
  aes(reorder(Bowler,No), No,fill=Bowler)+
  geom_bar(stat= "identity", width = 0.50,position = "dodge")+
  geom_text(aes(Bowler, No, label= No), hjust=-0.2, vjust=-0.2)+
  theme_minimal()+
  guides(TRUE)+
  theme(axis.title.x = element_blank())+
  labs(x="",y="", title = "  Most Maiden Overs By A Bowler ")
  

## Most wicket Taking bowler in IPL

k <-deli[deli$player_dismissed!="",]
l <-k[k$dismissal_kind!="run out",]
view(k)
view(l)
l%>%
  group_by(bowler) %>%
  summarise(Wickets_Taken= length(player_dismissed))%>%
  arrange(-Wickets_Taken)%>%
 head(15)%>%
  
##plotting above things
  
ggplot(aes(reorder(bowler,Wickets_Taken),Wickets_Taken,label=Wickets_Taken))+
  geom_bar(stat = "identity",fill="skyblue")+
  geom_text(vjust=-0.2)+
  xlab("Name Of Bowlers")+
  ylab("Total Wickets Taken")

##Above data In table

m<-l%>%
  group_by(bowler) %>%
  summarise(Wickets_Taken= length(player_dismissed))%>%
  arrange(-Wickets_Taken)%>%
  filter(Wickets_Taken>= 5)%>%
  select(bowler,Wickets_Taken)%>%
  head(15)

view(m)

##which bowler has take most wickets of which batsman

n<-l%>%
  group_by(bowler,batsman) %>%
  summarise(Total_Dismissed = sum(table(dismissal_kind)))%>%
  arrange(-Total_Dismissed)%>%
  head(15)

view(n)

## Team which is taking most wickets

p<- l%>%
  filter(dismissal_kind!="run out")%>%
  group_by(bowler,bowling_team)%>%
  summarise(Total_wickets=sum(table(dismissal_kind)))%>%
  arrange(-Total_wickets)%>%
  group_by(bowling_team)%>%
  slice(which.max(Total_wickets))
# head(15)

view(p)

p%>%
  ggplot(aes(reorder(bowling_team,Total_wickets),Total_wickets,label=Total_wickets))+
  geom_bar(stat = "identity",fill="skyblue")+
  geom_text(position=position_stack(vjust=0.5))+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90))+

  xlab("Name Of Team")+
  ylab("Total Wickets Taken")



## Now lets see IPL winners according to seasons

q<- matches%>%
    select(season, winner, id,team1,team2,toss_decision,toss_winner)%>%
    group_by(season)%>%
    slice(which.max(id))%>%
    select(season,team1,team2,toss_winner,toss_decision,winner)

view(q)


## which team has won by max runs

r<- matches[which.max(matches$win_by_runs),]
s<-r%>%
  select(winner, win_by_runs,season)

view(s)


##Batsman with high strike rate

t<- deli%>%
  group_by(batsman)%>%
  filter(length(total_runs)>500)%>%
  summarise(Strick_Rate=mean(batsman_runs)*100)%>%
  arrange(-Strick_Rate)%>%
 head(10)

t%>%
  ggplot(aes(reorder(batsman,Strick_Rate),Strick_Rate,fill=batsman,label=round(Strick_Rate,digits =2)))+
  geom_bar(stat = "identity")+
  geom_text(vjust=-0.2)+
  guides(TRUE)+
  xlab("Name Of Batsman")
  
view(t)


