
# Clear memory 

rm(list=ls())

# Packages

library(tidyverse)

# Data from kaggle

master <- read.csv("CompleteDataset.csv") # dataset taken from Kaggle
head(master)

#Teams that are to play to qualify for quater finals,There are about 16 teams such as stored in the below vetcor

teams <- c('Uruguay','Portugal','France','Argentina','Brazil','Mexico','Belgium','Japan','Spain','Russia','Croatia','Denmark','Sweden','Switzerland','Colombia','England')
length(teams) # 16


#Subsetting the master data with the 16 teams that are to play to qualify for the quaterfinals
teams_16 <- filter(master,Nationality %in% teams) 
View(teams_16)
length(unique(teams_16 $Nationality)) #16

#Overall look at the entries,some countries very high entries like england
ggplot(teams_16,aes(x=Nationality,fill=Nationality))+geom_bar()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
  labs(title='Distribution of data across Nationalites',y='No. of Players Data',subtitle="Some Nationalities have more Players data,\nSo using Average data / Nationality will be approapriate for analysis")

# Age of players
ggplot(teams_16,aes(x=Nationality,y=Age))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
  labs(title='Age Vs Nationality',subtitle="Using Age for analysis may not be appropriate")

#Overall performance,Quite a few outliers and they are due to star players,but it is not a good idea to remove them
ggplot(teams_16,aes(x=Nationality,y=Overall))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+
  labs(title='Overall performance Vs Nationality',subtitle="Some nationalities show outliers in overall performace,\nThe outliers needs to be accounted as they are star players \nHence using average data will be appropriate ")

# Correlation between overall and potential
cor(teams_16$Overall,teams_16$Potential) #0.68


######################################## compare teams using the variable 'Overall' from the dataset####################################### 
#uruguay vs portugal - There is a slight chance for Portugal to win
teams_16 %>% filter(Nationality %in% c('Uruguay','Portugal')) %>% group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 1,direction = 1)+
  labs(title='Uruguay vs Portugal',subtitle="There is a slight chance for Portugal to win",caption='Aggregated overall performance of players from each Nationality')
  

#France vs Argentina - Argentina has slightly better chance to win
teams_16 %>% filter(Nationality %in% c('France','Argentina')) %>%group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>% ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+
  geom_text(aes(label=Percentage),vjust=-0.5)+theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 2,direction = 1)+
  labs(title='France vs Argentina',subtitle="Argentina has slightly better chance to win",caption='Aggregated overall performance of players from each Nationality')

#Brazil vs Mexico  - There is better chance for Brazil to win
teams_16 %>% filter(Nationality %in% c('Brazil','Mexico')) %>% group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+
  geom_text(aes(label=Percentage),vjust=-0.5)+theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 3,direction = 1)+
  labs(title='Brazil vs Mexico',subtitle="There is better chance for Brazil to win",caption='Aggregated overall performance of players from each Nationality')

#Belgium vs Japan - There is better chance for Belgium to win
teams_16 %>% filter(Nationality %in% c('Belgium','Japan')) %>% group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 4,direction = 1)+
  labs(title='Belgium vs Japan',subtitle="There is better chance for Belgium to win",caption='Aggregated overall performance of players from each Nationality')

# Spain Vs Russia - There is better chnace for Spain to win
teams_16 %>% filter(Nationality %in% c('Spain','Russia')) %>% group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 5,direction = 1)+
  labs(title='Spain Vs Russia',subtitle="There is better chnace for Spain to win",caption='Aggregated overall performance of players from each Nationality')

# Croatia vs Denmark - There is better chance for Croatia to win
teams_16 %>% filter(Nationality %in% c('Croatia','Denmark')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 1,direction = 1)+
  labs(title='Croatia vs Denmark ',subtitle="There is better chance for Croatia to win",caption='Aggregated overall performance of players from each Nationality')

# Sweden vs Switzerland - There is better chance for Sweden to win
teams_16 %>% filter(Nationality %in% c('Sweden','Switzerland')) %>% group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 7,direction = 1)+
  labs(title='Sweden vs Switzerland',subtitle="There is better chance for Sweden to win",caption='Aggregated overall performance of players from each Nationality')

# Columbia vs England - There is better chance for colombia to win
teams_16 %>% filter(Nationality %in% c('Colombia','England')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 8,direction = 1)+
  labs(title='Columbia vs England',subtitle="There is better chance for Colombia to win",caption='Aggregated overall performance of players from each Nationality')

####################################### Quater finals - taking the winners from above###################################

#Portugal vs Argentina - Portugal has a better chance to win
teams_16 %>% filter(Nationality %in% c('Portugal','Argentina')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 1,direction = 1)+
  labs(title='Portugal vs Argentina',subtitle="Portugal has a better chance to win",caption='Aggregated overall performance of players from each Nationality')

#Brazil vs Belgium - Brazil has a better chance to win
teams_16 %>% filter(Nationality %in% c('Brazil','Belgium')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 3,direction = 1)+
  labs(title='Brazil vs Belgium ',subtitle="Brazil has a better chance to win",caption='Aggregated overall performance of players from each Nationality')

#Spain vs Croatia - Spain slightly has a better chance to win
teams_16 %>% filter(Nationality %in% c('Spain','Croatia')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 7,direction = 1)+
  labs(title='Spain vs Croatia',subtitle="pain slightly has a better chance to win",caption='Aggregated overall performance of players from each Nationality')

#Sweden vs Colombia - Columbia slightly has a better chance to win
teams_16 %>% filter(Nationality %in% c('Sweden','Colombia')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 8,direction = 1)+
  labs(title='Sweden vs Colombia',subtitle="Columbia slightly has a better chance to win",caption='Aggregated overall performance of players from each Nationality')

####################################### #Semi Finals - Takings the winners from quater finals####################################### 

#Portugal vs Brazil - Brazil slightly has a better chance to win
teams_16 %>% filter(Nationality %in% c('Portugal','Brazil')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 2,direction = 1)+
  labs(title='Portugal vs Brazil ',subtitle="Brazil slightly has a better chance to win",caption='Aggregated overall performance of players from each Nationality')

#Spain vs Colombia - Spain  has a better chance to win
teams_16 %>% filter(Nationality %in% c('Spain','Colombia')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 4,direction = 1)+
  labs(title='Spain vs Colombia',subtitle="Spain  has a better chance to win",caption='Aggregated overall performance of players from each Nationality')

####################################### First Semifinalist###########################
#Portugal vs Colombia - Spain  has a better chance to win
teams_16 %>% filter(Nationality %in% c('Portugal','Colombia')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 4,direction = 1)+
  labs(title='Portugal vs Colombia',subtitle="Portugal  has a better chance to win",caption='Aggregated overall performance of players from each Nationality')


######################################## Finals - Taking winners from Semi finals####################################### 

#Brazil vs Spain - Brazil  has a better chance to win the world Cup
teams_16 %>% filter(Nationality %in% c('Brazil','Spain')) %>% 
  group_by(Nationality)%>%summarise(Average = mean(Overall))%>%
  mutate(Percentage = paste(round((Average/sum(Average))*100,2),'%'))%>%
  ggplot(aes(Nationality,Average,fill=Nationality))+geom_col()+geom_text(aes(label=Percentage),vjust=-0.5)+
  theme(panel.background = element_rect(fill = 'white',colour = 'grey50'))+scale_fill_brewer(type='qual',palette = 1,direction = 1)+
  labs(title='Brazil vs Spain',subtitle="Brazil  has a better chance to win the world Cup",caption='Aggregated overall performance of players from each Nationality')

######################################## END ######################################## 