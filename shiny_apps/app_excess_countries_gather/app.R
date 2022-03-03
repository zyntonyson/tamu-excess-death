library(stringi)
library(shiny)
library(tidyverse)
library(glue)
library(magrittr)
library(reshape2)
library(readr)
library(lubridate)
library(scales)
library(plotly)
library(googlesheets4)
# Funciones

format_states<-function(state){
  if(!is.na(str_match(state,"coahuila"))){
    return('coahuila')
  }else if(state=="50_states_and_district_of_columbia"){
    return("united_states")
  }else{
    return(state)
  }
}

updates_states_by_country<-function(data,country_)
{
  choices<-data %>% 
    filter(country == country_) %>%
    .$state %>% 
    unique()
  
  default_choice<-ifelse(country_=='MX','National','United States')
  
  list(choices=choices,default=default_choice)
}


# Cargado de datos

# Datos poblacionales obtenidos de wikipedia
RATE_POP<-10000
POP_SHEET_USA<-"1JaRml8MRpBX_20T69blVjwRd_5JSCTW0zN1p2as4wfI"
pop_us<-read_sheet(POP_SHEET_USA,skip = 3) %>% 
  select(c(1,3)) %>% 
  rename(state=1,
         pop=2) %>% 
  mutate(pop=as.character(pop),
         pop=str_replace_all(pop,',',''),
         pop=str_replace_all(pop,"\\.",''),
         pop=as.numeric(pop)/RATE_POP,
         state=str_replace_all(str_to_lower(state),' ','_'),
         state=stri_trans_general(state,id = "Latin-ASCII")
         )

POP_SHEET_MX<-"1-a9lMlnfkbBQkedx18KLZrSlLhMYmt9g48B5qL0jcCM"
pop_mx<-read_sheet(POP_SHEET_MX,skip = 5) %>% 
  select(4,7) %>% 
  rename(state=1,
         pop=2) %>% 
  mutate(
         pop=str_replace_all(pop,' ',''),
         pop=as.numeric(pop)/RATE_POP,
         state=str_replace_all(str_to_lower(state),' ','_'),
         state=stri_trans_general(state,id = "Latin-ASCII")
  )

pop_global<-pop_mx %>% 
  rbind(pop_us) %>%
  mutate(
    state_format=sapply(state,format_states)
  ) %>% 
  select(-state) %>% 
  rbind(data.frame(state_format='national',pop=pop_mx %>%.$pop %>% sum()))
  

# Data integrada
data<- read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_mx.csv') %>%
    filter(week_regis<=52) %>% 
    select(date_ref,name,deaths_count,farrington_partial,channel_endemic_estimation) %>% 
    rename(state=name) %>%
    mutate(country='MX') %>%  
    rbind(
        read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_usa.csv') %>% 
            filter(week_regis<=52) %>% 
            select(date_ref,ent_regis,deaths_count,farrington_partial,
                   channel_endemic_estimation) %>% 
            mutate(country='US') %>% 
            rename(state=ent_regis)
    ) %>%
    rename(farrington=farrington_partial) %>% 
    mutate(
        excess_channel_endemic=100*(deaths_count-channel_endemic_estimation)/channel_endemic_estimation,
        excess_channel_endemic=ifelse(excess_channel_endemic <0,0,excess_channel_endemic),
        excess_farrington=100*(deaths_count-farrington)/farrington,
        excess_farrington=ifelse(excess_farrington < 0,0,excess_farrington),
        state_format=str_replace_all(str_to_lower(state),' ','_'),
        state_format=stri_trans_general(state_format,id = "Latin-ASCII"),
        state=str_to_title(state)
    ) %>% 
  left_join(pop_global,by='state_format') %>% 
  mutate(deaths_by_10k=deaths_count/pop) %>% 
  select(-c(pop,state_format)) 


states_mx<-data %>% 
    arrange(country) %>% 
    filter(country=='MX') %>%
    .$state %>% 
    unique()


states_us<-data %>% 
    arrange(country) %>% 
    filter(country=='US') %>%
    .$state %>% 
    unique()


country_ops<-c('US','MX')
#ent_op<-unique(data$name)
#metrics_ops<-unique(data$metric)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Excess of Deaths"),
    fluidRow(
        
        
        
        column(12,
               h4('Metric'),       
               selectInput('metric',
                           label = "Metric for excess",
                           choices = c('Channel Endemic','Farrington','Deaths by 10K'),
                           selected = 'Farrington')),
        
        
        br(), 
        br(),br(),
        
        column(10,
               h4('Country'),       
               selectInput('country_1',
                           label = 'Country',
                           choices = country_ops ,
                           selected = 'MX')),
        
        column(10,
               h4('States'),       
               checkboxInput('select_all',
                           label = 'Select All states',
                           value=F)),
        
                
        
        column(8,
               checkboxGroupInput('state_1',
                           label = 'States',
                           choices = states_mx ,
                           selected = 'National',
                           inline = T)),
      
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        br(),br(),
        plotlyOutput('plotlyplotcomparative')
)

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

#Update choices using country
    observe(
        {
            selection_1<-input$country_1
            updates_1<-updates_states_by_country(data=data,country = selection_1)
            if(input$select_all){
                selected_<-updates_1$choices
            }else{
                selected_<-updates_1$default
            }
            
            updateCheckboxGroupInput(session=session,
                              inputId = 'state_1',
                              label= "State",
                              choices=updates_1$choices,
                              selected=selected_,
                              inline=TRUE
            )
        })
    
    data_metric<-reactive({
        if(input$metric=='Farrington'){
            data %>%
                rename(metric=excess_farrington, raw_=farrington)
        }else if(input$metric=='Channel Endemic'){
          data %>%
            rename(metric=excess_channel_endemic, raw_=channel_endemic_estimation)
          }else{
            data %>%
              mutate(raw_=deaths_by_10k) %>% 
                rename(metric=deaths_by_10k)}
        
    })
    
    
    output$plotlyplotcomparative<-renderPlotly({
        df=data_metric()

        df %<>% 
            filter(country == input$country_1 ) %>%
            filter(state %in% input$state_1 ) 
        plot_ly(df,x=~date_ref,y=~metric,color=~state, type = 'scatter', mode = 'lines')
        
        
    })
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
