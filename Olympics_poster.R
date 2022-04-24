
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readr)
library(wesanderson)
library(tibble)
library(directlabels)
library(rvg)
library(magrittr)
library(officer)



olympics=read.csv("athlete_events.csv")

color<-c("#F5819C","#ada0a5")

gender =olympics%>%group_by(Sex)%>%summarize(total=n())%>% 
  mutate(percent = total/sum(total))

#plot1<-
ggplot(gender, aes(x='', y=percent,fill=Sex)) +
  geom_col()+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values=color)+
  geom_text(aes(label = paste0(total,
                               " (",
                               round(percent,2)*100,
                               "%)")),
            position = position_stack(vjust = 0.5))+
  ggtitle("Male Vs. Female Athletes")+
  theme(plot.title = element_text(hjust = 0.5))  

#dml <- dml(ggobj = plot1)


#read_pptx() %>%
#add_slide(layout = "Title and Content", master = "Office Theme") %>%
#ph_with(dml, location = ph_location(left = 0, top = 0.8, height = 5, width = 10))%>%
#print("graph.pptx")



olympics=olympics%>%
  mutate(medals=ifelse(is.na(Medal),'NotWon','Won'))

won=olympics%>% filter(medals=='Won')%>%
  distinct(NOC)%>%count()
a=olympics%>% distinct(NOC)%>%count()
medal=rbind(won=won,not_won=a-won)%>%
  mutate(percent = round(n/sum(n),2)*100)
medal=cbind(status=c("won",'not won'),medal)

plot2=ggplot(medal, aes(x='', y=percent,fill=status)) +
  geom_col()+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values=color)+
  geom_text(aes(label = paste0(n,
                               " (",
                               percent,
                               "%)")),
            position = position_stack(vjust = 0.5))+
  ggtitle("Countries/Regions Won & Not won Medals")+
  theme(plot.title = element_text(hjust = 0.5))  

dml <- dml(ggobj = plot2)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(dml, location = ph_location(left = 0, top = 0.8, height = 5, width = 10))%>%
  print("graph1.pptx")


athleteWon=olympics%>% filter(medals=='Won')%>%
  distinct(Name)%>%count()
athlete=olympics%>% distinct(Name)%>%count()
Athlete_medal=rbind(won=athleteWon,not_won=athlete-athleteWon)%>%
  mutate(percent = round(n/sum(n),2)*100)
Athlete_medal=cbind(status=c("won",'not won'),Athlete_medal)

plot3<-ggplot(Athlete_medal, aes(x='', y=percent,fill=status)) +
  geom_col()+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values=color)+
  geom_text(aes(label = paste0(n,
                               " (",
                               percent,
                               "%)")),
            position = position_stack(vjust = 0.5))+
  ggtitle("Athletes Won & Not won Medals")+
  theme(plot.title = element_text(hjust = 0.5))  

dml <- dml(ggobj = plot3)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(dml, location = ph_location(left = 0, top = 0.8, height = 5, width = 10))%>%
  print("graph2.pptx")

palette <- c("#F5819C", "#EDCABE", "#E9B666",
             "#BFD0CA", "#A5B2B5", "#0F4C81",
             "#5C9090", "#648198", "#ada0a5",
             "#a09bc2")

team <- olympics%>%filter(!is.na(Medal),Year>=2000)%>%group_by(Team)%>%summarize(medal=n())%>%arrange(desc(medal))%>%head(n=10)

plot4<-ggplot(team,aes(x=reorder(Team,medal),y=medal,fill=Team))+
  geom_bar(stat = 'identity',position="dodge")+
  coord_flip() +
  ggtitle("Top 10 Teams with Medals")+
  labs(x="Team",y="Number of Medals")+
  theme_bw()+
  scale_fill_manual(values = palette)

dml <- dml(ggobj = plot4)

read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(dml, location = ph_location(left = 0, top = 0.8, height = 5, width = 10))%>%
  print("graph3.pptx")
```