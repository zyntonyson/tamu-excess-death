library(tidyverse)
library(readr)
library(magrittr)
library(lubridate)
library(glue)
library(scales)

dataMxExcess <- read_csv("G:/Mi unidad/TAMUMX/TAMUMXGEO/Mortalidad/dataMxExcess_report.csv")
catalogo_entidades_mx <- read_csv("G:/Mi unidad/TAMUMX/TAMUMXGEO/Mortalidad/catalogo_entidades_mx.csv") %>% 
  mutate(ent_regis=as.numeric(ent_regis))  


rename_metrics<-function(x){
  names_orig<-c('observed_deaths','q90_estimation_ce','farrington_est','excess_ce','excess_fa')
  names_replace<-c('Observed deaths','Expected deaths by Channel endemic',
                   'Expected deaths by Farrington Method',
                   'Percent Excess of Deaths (CE estimation)',
                   'Percent Excess of Deaths (Farrington estimation)')
  return(names_replace[which(names_orig==x)])
}



dataMxExcess %>%
  left_join(catalogo_entidades_mx,by="ent_regis") %>% 
  mutate(ent_regis=name) %>% 
  select(-c(name,abr)) %>% 
  mutate(
    date_ref=ymd( glue("{anio_regis}-01-01") ) + weeks( week_regis - 1 ),
    month=month(ymd( glue("{anio_regis}-01-01") ) + weeks( week_regis - 1 ),label=TRUE, locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  filter(ent_regis=="NUEVO LEÓN",date_ref>ymd("2020-03-07")) %>% 
  select(anio_regis,week_regis,month, date_ref,ent_regis,q90,farrington_est,observed_deaths) %>% 
  mutate(ce_cumsum=cumsum(q90),
         far_cumsum=cumsum(farrington_est),
         obsdeat_cumsum=cumsum(observed_deaths),
         cum_excess_q90=100*(obsdeat_cumsum-ce_cumsum)/ce_cumsum,
         cum_excess_far=100*(obsdeat_cumsum-far_cumsum)/ce_cumsum
         )%>% 
  write.csv(r"(C:\Users\Roms\Downloads\cum_excess_nuevoleon.csv)",row.names = FALSE)





dataMxExcess %>%
  left_join(catalogo_entidades_mx,by="ent_regis") %>% 
  mutate(ent_regis=name) %>% 
  select(-c(name,abr)) %>% 
  mutate(
    date_ref=ymd( glue("{anio_regis}-01-01") ) + weeks( week_regis - 1 ),
    month=month(ymd( glue("{anio_regis}-01-01") ) + weeks( week_regis - 1 ),label=TRUE, locale = Sys.setlocale("LC_TIME", "English"))) %>% 
  filter(ent_regis =="NATIONAL") %>% 
  select(anio_regis,week_regis,month, date_ref,ent_regis,q90,farrington_est,observed_deaths) %>% 
  mutate(ce_cumsum=cumsum(q90),
         far_cumsum=cumsum(farrington_est),
         obsdeat_cumsum=cumsum(observed_deaths),
         cum_excess_q90=100*(obsdeat_cumsum-ce_cumsum)/ce_cumsum,
         cum_excess_far=100*(obsdeat_cumsum-far_cumsum)/ce_cumsum
  )%>%  write.csv(r"(C:\Users\Roms\Downloads\cum_excess_nacional.csv)",row.names = FALSE)







##########################################EXCESOS NACIONALES#########################################################

library(tidyverse)
library(readr)
library(magrittr)
library(lubridate)
library(glue)
library(scales)


rename_metrics<-function(x){
  names_orig<-c('observed_deaths','q90_estimation_ce','farrington_est','excess_ce','excess_fa')
  names_replace<-c('Observed deaths','Expected deaths by Channel endemic',
                   'Expected deaths by Farrington Method',
                   'Percent Excess of Deaths (CE estimation)',
                   'Percent Excess of Deaths (Farrington estimation)')
  return(names_replace[which(names_orig==x)])
}




dataMxExcess <- read_csv(r"(G:\Mi unidad\TAMUMX\TAMUMXGEO\Mortalidad\exceso_final\mex_data\dataMXExcess_report.csv)") %>% 
  filter(ent_regis == 0) %>%
  select(anio_regis,week_regis,observed_deaths,q90,farrington_est) %>% 
  mutate(Country='MX')  
  

dataUsaExcess <- read_csv(r"(G:\Mi unidad\TAMUMX\TAMUMXGEO\Mortalidad\exceso_final\usa_data\dataUSAExcess_report.csv)") %>% 
  filter(ent_regis == 'United States') %>%
  select(anio_regis,week_regis,observed_deaths,q90,farrington_est) %>% 
  mutate(Country='USA')


data<-dataUsaExcess %>% 
  rbind(dataMxExcess) %>% 
  mutate(percent_excess_q90= 100*(observed_deaths-q90)/q90,
         percent_excess_q90= ifelse(percent_excess_q90<0,0,percent_excess_q90),
         percent_excess_farrington= 100*(observed_deaths-farrington_est)/farrington_est,
         percent_excess_farrington= ifelse(percent_excess_farrington<0,0,percent_excess_farrington),
         date= ymd( glue("{anio_regis}-01-01") ) + weeks( week_regis - 1 )) 
  

### Endemic channel
data %>% 
  ggplot()+
  aes(x=date,y=percent_excess_q90,color=Country)+
  geom_line(lwd=2) +
  labs(
    x='Date',
    y='Excess of death (%)',
    title='Comparative Excess of deaths MX-USA',
    caption='Expected deaths were estimated by endemic channel method',
    subtitle='Global Excess MX:45.6%  USA:15%'
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal", 
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave(r"(C:\Users\Roms\Desktop\img_\excess_channel_endemic.png)",width = 21, height = 14, units = "cm")




### Endemic channel con eje secundario
data %>% 
  ggplot()+
  aes(x=date,y=percent_excess_q90,color=Country)+
  geom_line(lwd=2) +
  geom_line( aes(y=observed_deaths,color=Country), col='grey') +
  labs(
    x='Date',
    #y='Excess of death (%)',
    title='Comparative Excess of deaths MX-USA',
    caption='Expected deaths were estimated by endemic channel method',
    subtitle='Global Excess MX:45.6%  USA:15%'
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal", 
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_y_continuous(name = 'Observed deatyjs', 
                     sec.axis = sec_axis(~./50000, name = 'Excess of death (%)', 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")}))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave(r"(C:\Users\Roms\Desktop\img_\excess_channel_endemic.png)",width = 21, height = 14, units = "cm")




data %>%
  filter(percent_excess_q90>=0) %>% 
  group_by(Country) %>% 
  summarise(observed_deaths=sum(observed_deaths),
            q90=sum(q90)
            ) %>% 
  transmute(Country,excess_death=100*(observed_deaths-q90)/q90)






### Farrington
data %>% 
  ggplot()+
  aes(x=date,y=percent_excess_farrington,color=Country)+
  geom_line(lwd=2) +
  labs(
    x='Date',
    y='Excess of death (%)',
    title='Comparative Excess of deaths MX-USA',
    subtitle='Global Excess MX:33%  USA:10.1%',
    caption='Expected deaths were estimated by Farrington method'
  )+
  theme(legend.title=element_blank(),legend.direction = "horizontal", 
        legend.position = "bottom", 
        legend.key = element_blank(),axis.text.x = element_text(angle = 45, hjust=1))+
  scale_x_date(breaks = pretty_breaks(10))
ggsave(r"(C:\Users\Roms\Desktop\img_\excess_farrington.png)",width = 21, height = 14, units = "cm")




data %>% 
  filter(percent_excess_farrington>=0) %>% 
  group_by(Country) %>% 
  summarise(observed_deaths=sum(observed_deaths),
            farrington_est=sum(farrington_est)
  ) %>% 
  transmute(Country,excess_death=100*(observed_deaths-farrington_est)/farrington_est)



#####  Excess by estimation


data %>% 
  group_by(Country) %>% 
  summarise(observed_deaths=sum(observed_deaths),
            farrington_est=sum(farrington_est),
            observed_deaths=sum(observed_deaths),
            q90=sum(q90)
  ) %>% 
  transmute(Country,
            excess_death_farrington=100*(observed_deaths-farrington_est)/farrington_est,
            excess_death_q90=100*(observed_deaths-q90)/q90)




