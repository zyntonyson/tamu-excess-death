iso_week<-function(anio,mes,dia)
{
  tryCatch(
    expr=week(glue('{anio}-{mes}-{dia}')),
    error= function(e){return(0)} 
  )
  
}

replace_na_with_zeros<-function(x)
{
  ifelse(is.na(x),0,x)
}


rename_metrics<-function(x){
  metrics_list<-unlist(strsplit('q90;observed_deaths;farrington_est;mean;mean_farrington;estimated_mean',';'))
  metrics_list_names<-unlist(strsplit('Estimation by Endemic channel;Observed Deaths;Estimation by Farrington;Historical mean;Mean by Farrington(GLM);Mean by Farrington',';'))
  if(x %in% metrics_list){
    idx<-match(x,metrics_list)
    metrics_list_names[idx]
  }else{
    x
  }
}



convertWeek<-function(year,week)
{
  lubridate::ymd( glue("{year}-01-01") ) + lubridate::weeks( week - 1 )
}