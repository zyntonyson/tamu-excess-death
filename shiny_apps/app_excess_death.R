library(shiny)
library(tidyverse)
library(glue)
library(magrittr)
library(reshape2)
library(readr)
library(lubridate)
library(scales)

# Cargado de datos


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
  state=str_to_title(state)
  )


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

updates_states_by_country<-function(data,country_)
{
  choices<-data %>% 
    filter(country == country_) %>%
    .$state %>% 
    unique()
  
  default_choice<-ifelse(country_=='MX','National','United States')
  
  list(choices=choices,default=default_choice)
}

country_ops<-c('US','MX')
ent_op<-unique(data$name)
metrics_ops<-unique(data$metric)



ui <- fluidPage(
  
  # Application title
  titlePanel("Excess of deaths "),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    
    
    
    column(12,
           h4('Metric'),       
           selectInput('metric',
                       label = "Metric for excess",
                       choices = c('Channel Endemic','Farrington'),
                       selected = 'Farrington')),
    
    
    br(), 
    br(),br(),

    column(2,
           h4('Country'),       
           selectInput('country_1',
                       label = 'Country',
                       choices = country_ops ,
                       selected = 'MX')),
    
        
        
    column(2,
           h4('State'),       
           selectInput('state_1',
                       label = 'States',
                       choices = states_mx ,
                       selected = 'National')),
    
    br(),
    
    plotOutput("myPlot1"),
    
    br(),
    br(),
    br(),
    br(), br(), br(), br(), br(), br(), br(),
    
    column(2,
           h4('Country'),       
           selectInput('country_2',
                       label = 'Country',
                       choices = country_ops ,
                       selected = 'US')),
    
    
    column(2,
           h4('State'),       
           selectInput('state_2',
                       label = 'States',
                       choices = states_us ,
                       selected = 'United States')),
    
        
    
    
    
    
    ),
  
  br(),br(),br(),
  plotOutput("myPlot2"),
  br(),br(),
  h4('Comparative'),  
  #dataTableOutput('mydf')
  br(),br(),
  plotOutput("myPlotcomparative"),
  
  br(),br()
  )


server <- function(input, output,session)
{
  
  ## Observe selections for countries
  observe(
    {
      selection_1<-input$country_1
      updates_1=updates_states_by_country(data=data,country = selection_1)
      updateSelectInput(session=session,
                        inputId = 'state_1',
                        label= "State",
                        choices=updates_1$choices,
                        selected=updates_1$default
                        )
    })
  
  
  observe(
    {
      selection_2<-input$country_2
      updates_2=updates_states_by_country(data=data,country = selection_2)
      updateSelectInput(session=session,
                        inputId = 'state_2',
                        label= "State",
                        choices=updates_2$choices,
                        selected=updates_2$default
      )
    })
  
## Filter data by selected metric
  data_metric<-reactive({
    if(input$metric=='Farrington'){
      data %>%
        rename(metric=excess_farrington, raw_=farrington)
    }else{
      data %>%
        rename(metric=excess_channel_endemic, raw_=channel_endemic_estimation)}
    
    })
  
  
## Plot 1 data

   global_excess_plot1<-reactive({
     df<-data_metric()
     country_selection<-input$country_1
     state_selection<- input$state_1
     df %>% 
       filter(country == country_selection ) %>%
       filter(state == state_selection ) %>% 
       group_by(state) %>% 
       summarise(deaths=sum(deaths_count),metric=sum(raw_)) %>% 
       mutate(excess=100*(deaths-metric)/metric) %>% 
       .$excess %>% as.numeric() %>% round(2)
     
   }) 

  
  
output$myPlot1<-renderPlot({
  df=data_metric()
  country_selection<-input$country_1
  state_selection<- input$state_1
  
  excess= as.numeric(global_excess_plot1())
  
    df%>%
      filter(country == country_selection ) %>%
      filter(state == state_selection ) %>% 
      ggplot() +
      aes(x=date_ref,y=metric) +
      geom_line(lwd=1.5) +
      labs(
        x='Date',
        y='Excess of death (%)',
        title=glue('Excess of deaths {input$country_1}:{input$state_1}'),
        caption=glue("Expected deaths were estimated by {input$metric} method"),
        subtitle=glue('Global Excess : {excess} %')
      )+
      theme(legend.title=element_blank(), 
            legend.direction = "horizontal",
            legend.text = element_text(size=8),
            legend.position = "bottom", 
            legend.key = element_blank(),
            axis.text.x = element_text(angle = 45, hjust=1)
      ) +
      scale_x_date(breaks = pretty_breaks(10))
    
  })


## Plot 2 data


global_excess_plot2<-reactive({
  df<-data_metric()
  country_selection<-input$country_2
  state_selection<- input$state_2
  df %>% 
    filter(country == country_selection ) %>%
    filter(state == state_selection ) %>% 
    group_by(state) %>% 
    summarise(deaths=sum(deaths_count),metric=sum(raw_)) %>% 
    mutate(excess=100*(deaths-metric)/metric) %>% 
    .$excess %>% as.numeric() %>% round(2)
  
}) 



output$myPlot2<-renderPlot({
  df=data_metric()
  country_selection<-input$country_2
  state_selection<- input$state_2
  
  excess= as.numeric(global_excess_plot2())
  
  df%>%
    filter(country == country_selection ) %>%
    filter(state == state_selection ) %>% 
    ggplot() +
    aes(x=date_ref,y=metric) +
    geom_line(lwd=1.5) +
    labs(
      x='Date',
      y='Excess of death (%)',
      title=glue('Excess of deaths {input$country_2}:{input$state_2}'),
      caption=glue("Expected deaths were estimated by {input$metric} method"),
      subtitle=glue('Global Excess : {excess} %')
    )+
    theme(legend.title=element_blank(), 
          legend.direction = "horizontal",
          legend.text = element_text(size=8),
          legend.position = "bottom", 
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_x_date(breaks = pretty_breaks(10))
  
})



### Comparation plot

output$myPlotcomparative<-renderPlot({
  df=data_metric()
  excess_1= as.numeric(global_excess_plot1())
  excess_2= as.numeric(global_excess_plot2())
  
  df %>% 
    filter(country == input$country_1 ) %>%
    filter(state == input$state_1 ) %>%
    rbind(
      df %>% 
        filter(country == input$country_2 ) %>%
        filter(state == input$state_2 )
    ) %>% 
    ggplot() +
    aes(x=date_ref,y=metric,color=state) +
    geom_line(lwd=1.5)+
    labs(
      x='Date',
      y='Excess of death (%)',
      title=glue('Excess of deaths Comparative {input$state_1}({input$country_1}):{input$state_2}({input$country_2})'),
      caption=glue("Expected deaths were estimated by {input$metric} method"),
      subtitle=glue('Global Excess: {input$state_1}({excess_1}%), {input$state_2}({excess_2}%)')
    )+
    theme(legend.title=element_blank(), 
          legend.direction = "horizontal",
          legend.text = element_text(size=8),
          legend.position = "bottom", 
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1)
    ) +
    scale_x_date(breaks = pretty_breaks(10))
  
})


}


# Run the application 
shinyApp(ui = ui, server = server)