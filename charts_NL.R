library(tidyverse)
library(glue)
library(magrittr)
library(reshape2)
library(readr)
library(lubridate)
library(scales)


data<-read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_usa_gather.csv') %>%
  rename(name=ent_regis) %>%
  mutate(country='US') %>% 
  rbind(read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_mx_gather.csv') %>% 
          mutate(country='MX')) %>% 
  mutate(metric=str_to_title(metric),
         metric= str_replace_all(metric,'_',' '),
         name=str_to_title(name)
         )



## NL chart

name_<-'Nuevo León'
data %>% 
  filter(date_ref<ymd('2022-01-01')) %>%
  filter(name==name_) %>%
  rename(date=date_ref) %>% 
  ggplot()+
  aes(x=date,y=deaths, color=metric)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Deaths',
    title='Expected deaths: Nuevo León',
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_NL_expected_deaths_all.png",width = 21, height = 14, units = "cm")


data %>% 
  filter(date_ref<ymd('2022-01-01')) %>%
  filter(name==name_) %>%
  filter(metric!='Farrington estimation all') %>% 
  rename(date=date_ref) %>% 
  ggplot()+
  aes(x=date,y=deaths, color=metric)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Deaths',
    title='Expected deaths: Nuevo León',
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_NL_expected_deaths.png",width = 21, height = 14, units = "cm")


### MX
name_<-'National'
data %>% 
  filter(date_ref<ymd('2022-01-01')) %>%
  filter(name==name_) %>%
  rename(date=date_ref) %>% 
  ggplot()+
  aes(x=date,y=deaths, color=metric)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Deaths',
    title='Expected deaths MX',
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_MX_expected_deaths_all.png",width = 21, height = 14, units = "cm")


data %>% 
  filter(date_ref<ymd('2022-01-01')) %>%
  filter(name==name_) %>%
  filter(metric!='Farrington estimation all') %>% 
  rename(date=date_ref) %>% 
  ggplot()+
  aes(x=date,y=deaths, color=metric)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Deaths',
    title='Expected deaths MX',
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_MX_expected_deaths.png",width = 21, height = 14, units = "cm")




### USA

name_<- 'United States'

data %>%
  filter(date_ref<ymd('2022-01-01')) %>%
  filter(name==name_) %>%
  rename(date=date_ref) %>% 
  ggplot()+
  aes(x=date,y=deaths, color=metric)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Deaths',
    title='Expected deaths USA',
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_USA_expected_deaths_all.png",width = 21, height = 14, units = "cm")


data %>% 
  filter(name==name_) %>%
  filter(date_ref<ymd('2022-01-01')) %>%
  filter(metric!='Farrington estimation all') %>% 
  rename(date=date_ref) %>% 
  ggplot()+
  aes(x=date,y=deaths, color=metric)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Deaths',
    title='Expected deaths USA',
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_USA_expected_deaths.png",width = 21, height = 14, units = "cm")



#####  Binational


data<-read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_usa_gather.csv') %>%
  rename(name=ent_regis) %>%
  mutate(country='US') %>% 
  rbind(read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_mx_gather.csv') %>% 
          mutate(country='MX')) %>% 
  mutate(metric=str_to_title(metric),
         metric= str_replace_all(metric,'_',' '),
         name=str_to_title(name)
  )


data_binacional<- read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_usa.csv') %>% 
  filter(ent_regis=="United States") %>% 
  filter(week_regis<53) %>% 
  select(date_ref,deaths_count,channel_endemic_estimation,farrington_partial) %>%
  mutate(country='USA') %>% 
  rbind(.,
    read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_mx.csv') %>% 
      filter(name=="NATIONAL") %>% 
      select(date_ref,deaths_count,channel_endemic_estimation,farrington_partial) %>% 
    mutate(country='MX')
  ) %>% 
  mutate(
    excess_death_percent_ce=(deaths_count-channel_endemic_estimation)/channel_endemic_estimation,
    excess_death_percent_ce=ifelse(excess_death_percent_ce<0,0,excess_death_percent_ce),
    excess_death_percent_ff=(deaths_count-farrington_partial)/farrington_partial,
    excess_death_percent_ff=ifelse(excess_death_percent_ff<0,0,excess_death_percent_ff)
  )

# Global excesss
data_binacional %>%
  group_by(country) %>% 
  summarise(deaths=sum(deaths_count),est=sum(channel_endemic_estimation)) %>% 
  mutate(percent=100*((deaths/est)-1))


data_binacional %>%
  group_by(country) %>% 
  summarise(deaths=sum(deaths_count),est=sum(farrington_partial)) %>% 
  mutate(percent=100*((deaths/est)-1))



# charts  Farrington
data_binacional %>% 
  ggplot()+
  aes(x=date_ref,y=100*excess_death_percent_ff,color=country)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Excess of death (%)',
    title='Comparative Excess of deaths MX-USA',
    caption='Expected deaths were estimated by Farrington method',
    subtitle='Global Excess MX:33.0%  USA:12.1%'
  )+
  theme(legend.title=element_blank(), 
        legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
        ) +
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_binational_expected_deaths_Farrington.png",width = 21, height = 14, units = "cm")


# Channel endemic


data_binacional %>% 
  ggplot()+
  aes(x=date_ref,y=100*excess_death_percent_ce,color=country)+
  geom_line(lwd=1.5)+
  labs(
    x='Date',
    y='Excess of death (%)',
    title='Comparative Excess of deaths MX-USA',
    caption='Expected deaths were estimated by endemic channel method',
    subtitle='Global Excess MX:47.1%  USA:17.2%'
  )+
  theme(legend.title=element_blank(), 
        legend.direction = "horizontal",
        legend.text = element_text(size=8),
        legend.position = "bottom", 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)
  ) +
  scale_x_date(breaks = pretty_breaks(10))
ggsave("charts/22-02-21_binational_expected_deaths_channel_endemic.png",width = 21, height = 14, units = "cm")










