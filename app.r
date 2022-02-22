library(shiny)
library(tidyverse)
library(glue)
library(magrittr)
library(reshape2)

# Cargado de datos


data<-read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_usa_gather.csv') %>%
  rename(name=ent_regis) %>%
  mutate(country='US') %>% 
  rbind(read_csv('https://raw.githubusercontent.com/zyntonyson/tamu-excess-death/main/reports/excess_death_mx_gather.csv') %>% 
          mutate(country='MX')) %>% 
  mutate(metric=str_to_title(metric),
         metric= str_replace_all(metric,'_',' '))
 
country_ops<-c('US','MX')
ent_op<-unique(data$name)
metrics_ops<-unique(data$metric)



ui <- fluidPage(
  
  # Application title
  titlePanel("Excess of deaths "),
  
  # Sidebar with a slider input for number of bins
  
  
  fluidRow(
    
    column(6,
           h4('Country'),       
           checkboxGroupInput('metric',
                              label="country",
                              choices = country_ops,
                              selected = c('MX'),
                              inline=TRUE)),
    column(6,
           h4('States'),       
           selectInput('ent',
                       label = 'States',
                       choices = output$ent_values ,
                       selected = 'NATIONAL')),
    column(6,
           h4('Metrics'),       
           checkboxGroupInput('metric',
                              label="Metrics",
                              choices = metrics_ops,
                              selected = c('Deaths count'),
                              inline=TRUE)
           
    )),
  
  
  plotOutput("myPlot"),
  br(),
  #h4('En la tabla se muestra el cálculo de exceso de muerte usando las estimaciones con
       #el método de Farrintong'),  
  #dataTableOutput('mydf')
  
  
)




server <- function(input, output)
{
 output$ent_values<-data %>% 
   filter(country == input$country) %>% 
   .$name %>% unique()
  
  output$myPlot<-renderPlot({
   data %>% 
     filter(country == input$country) %>% 
     filter(metric %in% input$metric,name==input$ent) %>% 
     ggplot()+
     aes(x=date_ref,deaths,color=metric) %>% 
     geom_line(size=2) +
     labs(x='Date', y='Deaths',
          title=glue('Excess of death for {input$ent} ')
          )
     
   
 }) 
  
}


# Run the application 
shinyApp(ui = ui, server = server)