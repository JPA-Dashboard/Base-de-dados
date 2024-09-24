rm(list = ls())
setwd("C:/Users/Sergio/OneDrive/ANALISES/FAPEMIG/dashboard/DASHBOARDMILHO/d1/exemplosergio/models11JPA")
require(openxlsx)##Buscar o pacote instalado
dados <- read.xlsx("JuncaoPrecoMilhoB3eJPA (2).xlsx", sheet = 1)
dados##ler todos os dados
str(dados)##Estrutura dos dados
attach(dados)##Cria uma lista dos dados
library(xgboost)
library(tidymodels)
library(modeltime)
#remotes::install_github("business-science/modeltime.ensemble")
library(modeltime.ensemble)
library(tidyverse)
library(lubridate)
library(timetk)
library(zoo)
interactive <- FALSE
dados <- subset(dados,dados$DATA >0& dados$Preco>0)
dados
attach(dados)

dados1<-dados
str(dados1)
library(date)
dados1$DATA<-as.numeric(dados1$DATA)
## So for dates (post-1901) from Windows Excel
dados1$DATA<-as.Date(dados1$DATA, origin = "1899-12-30") # 1998-07-05
dados1$DATA

preco_por_praca <- dados1 %>%
  mutate(DATA = as_date(DATA)) %>%
  group_by(Estado,Região, DATA) %>%
  summarise(Preco = Preco,
            B3= Fechamento)
preco_por_praca
summary(preco_por_praca)
preco_por_praca1 <- subset(preco_por_praca,preco_por_praca$Preco<125)
preco_por_praca1

preco_por_praca2 <- preco_por_praca1
preco_por_praca2

str(preco_por_praca2)

#preco_por_praca2<-preco_por_praca2%>%
#filter(!Região %in% c("Ourinhos","Abelardo Luz","Ribeirão do Sul"))
#preco_por_praca2

#preco_por_praca2<-preco_por_praca2 %>% 
#filter(!Estado %in% c("GO","MA","RJ","SP","TO"))
#preco_por_praca2
#View(preco_por_praca2)
# -- Iterate through all groupings
library(tidyverse)
library(lubridate)
library(readr)
library(TSstudio)
library(scales)
library(plotly)
library(h2o)
library(vip)
library(gridExtra)
library(knitr)

library(ggplot2)
data_tbl <- preco_por_praca2 %>% 
  select(Estado,Região, DATA, Preco, B3)
data_tbl
library(feather)
library(data.table)
library(mgcv)
library(car)
library(ggplot2)
library(grid)
library(animation)

# Use feather (fast to share data) to read data.table
data_tbl <- as.data.table(data_tbl)
data_tbl

data_tbl$mes <- format(as.Date(data_tbl$DATA), "%Y-%m")
data_tbl$mes

data_tbl$ano <- format(as.Date(data_tbl$DATA), "%Y")
data_tbl$ano

data_tbl
#Libraries
library(prophet)
library(dplyr)
library(forecast)
library(tidyverse)
library(lubridate)
library(readr)
library(TSstudio)
library(scales)
library(plotly)
library(h2o)
library(vip)
library(gridExtra)
library(knitr)

data_tbl %>% summary()
model_data_tbl <- 
  data_tbl %>% 
  mutate(trend       = 1:nrow(data_tbl),
         trend_sqr   = trend^2,
         rev_lag_1  = lag(Preco, n = 1),
         MA=rollmean(Preco, k = 1, fill = NA)
         #season      = case_when(Preco == 0 ~ 0,
         #TRUE ~ 1)
  ) %>% 
  filter(!is.na(MA))
model_data_tbl
# Read in data
data_tbl <- model_data_tbl 
#View(data_tbl)
data_tbl
require(webshot)
require(export)
# Load necessary library
library(tidyr)

# Assuming your data frame is named "model_data_tbl"
# Unite Estado and Região columns into a single column named Estado_Região
data_tbl <- unite(data_tbl, Estado_Região, Estado,Região, sep = "/")
data_tbl

library(xlsx)
library(plotly)
library(orca)
library(htmlwidgets)
library(tidymodels)
library(modeltime.h2o)
library(tidyverse)
library(timetk)
require(dplyr)
require(modeltime)
library(lubridate)
require(agua)
h2o.init(max_mem_size = "12G")
h2o.no_progress()
# Function to train AutoML models by state and region with different behaviors for each state
train_automl <- function(data, save_dir = "./models/") {
  # Join Estado and Região columns into a single column named Estado_Região
  data$Estado_Região <- paste(data$Estado, data$Região, sep = "")
  # Initialize an empty list to store trained models
  all_models <- list()
  
  # Loop through each unique state-region combination
  for (state_region in unique(data$Estado_Região)) {
    # Subset the data for the specified state-region combination
    subset_data <- data[data$Estado_Região == state_region, ]
    # Split data into train, test, and forecast sets
    data_tbl1 <- separate(subset_data, col=Estado_Região, into=c('Estado', 'Região'), sep='/')
    data_tbl1$Região_Estado<- paste(data_tbl1$Região,data_tbl1$Estado, sep = "/")
    region_state <- unique(data_tbl1$Região_Estado)
    # Save the objects train_tbl, test_tbl, and forecast_tbl as Excel files
    file_prefix <- paste0(save_dir, state_region)
    save_path <- paste0(file_prefix, "")
    DADOStudo<-data.frame(subset_data)
    DADOStudo$Preco<-round(DADOStudo$Preco,2)
    DADOStudo$B3<-round(DADOStudo$B3,2)
    require(lubridate)
    
    ano5<-max(DADOStudo$DATA) - years(5)
    dados3151<-DADOStudo[DADOStudo$DATA >= ano5, ]
    dados3151
    lm.model <- augment(lm(Preco-B3 ~ DATA, data = dados3151))
    require(dplyr)
    require(highcharter)
    graphtudo2<-highchart(type = "stock") %>% 
      hc_add_series(data = dados3151, 
                    type = "line", 
                    hcaes(x = DATA, y = round(Preco,2)), 
                    name = "Real")%>% 
      hc_add_series(data = dados3151, 
                    type = "spline", 
                    hcaes(x = as.Date(DATA), y = round(B3,2)), 
                    name = "B3",
                    id = "fit", # this is for link the arearange series to this one and have one legend
                    lineWidth = 2)%>%  
      hc_add_series(data = dados3151,
                    type = "spline",
                    hcaes(x = as.Date(DATA), y = round(Preco-B3,2)),
                    name = "Basis",
                    linkedTo = "fit", # here we link the legends in one.
                    color ="orange"  # put a semi transparent 
      ) %>%
      hc_add_series(lm.model, "line", hcaes(x = DATA, y = round(.fitted,2)),
                    name = "Basis médio",
                    linkedTo = "fit", # here we link the legends in one.
                    color ="red")  # put a semi transparent color
    
    
    graphtudo2 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em", region_state))
    
    graphtudo3<-graphtudo2 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em", region_state))
    library(highcharter)
    library(htmlwidgets)
    save_pathtudo3 <- paste0(htmlwidgets::saveWidget(widget =graphtudo3,
                                                     file = file.path(save_path, "highcharttudo3MESORREGIAO.html"),
                                                     selfcontained = TRUE))
    
    # Convert Preco to numeric and handle possible locale issues
    dados3151$Preco <- as.numeric(gsub(",", ".", as.character(dados3151$Preco)))
    
    # Ensure DATA is of Date type
    dados3151$DATA <- as.Date(dados3151$DATA)
    
    # Get the latest date
    latest_date <- max(dados3151$DATA, na.rm = TRUE)
    
    library(dplyr)
    library(lubridate)
    
    # Define the latest date
    latest_date <- max(dados3151$DATA)
    
    # Calculate the date 30 days ago
    trinta_days_ago <- latest_date - days(30)
    
    # Add a new column to calculate the difference in days from trinta_days_ago
    data_with_diff30 <- dados3151 %>%
      mutate(date_diff30 = abs(difftime(DATA, trinta_days_ago, units = "days")))
    
    # Find the closest date to trinta_days_ago
    closest_date30 <- data_with_diff30 %>%
      filter(date_diff30 == min(date_diff30)) %>%
      pull(DATA) %>%
      unique()  # Assuming there could be multiple dates with the same minimum difference
    
    closest_date30
    # Filter the data for the closest date
    filtered_data30 <- dados3151 %>%
      filter(DATA %in% closest_date30)
    
    filtered_data30
    
    # Find the closest date and filter data
    filtered_data30 <- dados3151 %>%
      mutate(date_diff30 = abs(difftime(DATA, trinta_days_ago, units = "days"))) %>%
      filter(date_diff30 == min(date_diff30)) %>%
      select(-date_diff30)%>%slice(1)  # Remove the date_diff column if no longer needed
    filtered_data30
    
    value30 <- filtered_data30 %>%
      pull(Preco)
    value30<- unique(value30)
    value301 <- filtered_data30 %>%
      pull(DATA)
    value301 <- format(value301, "%d-%m-%Y")
    value301<- unique(value301)
    
    # Calculate the date 180 days ago
    seis_meses_ago <- latest_date - days(181)
    
    # Add a new column to calculate the difference in days from seis_meses_ago
    data_with_diff180 <- dados3151 %>%
      mutate(date_diff180 = abs(difftime(DATA, seis_meses_ago, units = "days")))
    
    # Find the closest date to seis_meses_ago
    closest_date180 <- data_with_diff180 %>%
      filter(date_diff180 == min(date_diff180)) %>%
      pull(DATA) %>%
      unique()  # Assuming there could be multiple dates with the same minimum difference
    
    closest_date180
    # Filter the data for the closest date
    filtered_dados315180 <- dados3151 %>%
      filter(DATA %in% closest_date180)
    
    filtered_dados315180
    
    # Find the closest date and filter data
    filtered_dados315180 <- dados3151 %>%
      mutate(date_diff180 = abs(difftime(DATA, seis_meses_ago, units = "days"))) %>%
      filter(date_diff180 == min(date_diff180)) %>%
      select(-date_diff180)%>%slice(1)  # Remove the date_diff column if no longer needed
    filtered_dados315180
    
    value180 <- filtered_dados315180 %>%
      pull(Preco)
    value180<- unique(value180)
    value1801 <- filtered_dados315180 %>%
      pull(DATA)
    value1801 <- format(value1801, "%d-%m-%Y")
    value1801<- unique(value1801)
    
    # Calculate the date 365 days ago
    seis_meses_ago <- latest_date - days(366)
    
    # Add a new column to calculate the difference in days from seis_meses_ago
    data_with_diff365 <- dados3151 %>%
      mutate(date_diff365 = abs(difftime(DATA, seis_meses_ago, units = "days")))
    
    # Find the closest date to seis_meses_ago
    closest_date365 <- data_with_diff365 %>%
      filter(date_diff365 == min(date_diff365)) %>%
      pull(DATA) %>%
      unique()  # Assuming there could be multiple dates with the same minimum difference
    
    closest_date365
    # Filter the data for the closest date
    filtered_data365 <- dados3151 %>%
      filter(DATA %in% closest_date365)
    
    filtered_data365
    
    # Find the closest date and filter data
    filtered_data365 <- dados3151 %>%
      mutate(date_diff365 = abs(difftime(DATA, seis_meses_ago, units = "days"))) %>%
      filter(date_diff365 == min(date_diff365)) %>%
      select(-date_diff365)%>%slice(1)  # Remove the date_diff column if no longer needed
    filtered_data365
    
    value365 <- filtered_data365 %>%
      pull(Preco)
    value365<- unique(value365)
    value3651 <- filtered_data365 %>%
      pull(DATA)
    value3651 <- format(value3651, "%d-%m-%Y")
    value3651<- unique(value3651)
    
    # Get the latest price by finding the most recent date
    latest_data <- dados3151 %>%
      arrange(desc(DATA)) %>%
      slice(1) %>%
      pull(Preco)
    latest_data<- unique(latest_data)
    # Calculate the start of the most recent month
    start_of_current_month <- floor_date(latest_date, "month")
    start_of_current_month
    # Calculate the start of the previous month
    start_of_last_month <- start_of_current_month 
    start_of_last_month
    # Calculate the end of the previous month (one day before the start of the current month)
    end_of_last_month <- latest_date
    end_of_last_month
    # Filter data for the last month
    filtered_data <- dados3151 %>%
      filter(DATA >= start_of_last_month & DATA <= end_of_last_month)%>%slice(1)
    
    # Calculate the mean price for the filtered period
    mean_price_last_month <- filtered_data %>%
      pull(Preco) %>%
      mean(na.rm = TRUE)
    
    mean_price_last_month<- unique(mean_price_last_month)
    
    valueend_of_last_month <- format(end_of_last_month, "%d-%m-%Y")
    valueend_of_last_month<- unique(valueend_of_last_month)
    
    valuestart_of_last_month <- format(start_of_last_month, "%d-%m-%Y")
    valuestart_of_last_month<- unique(valuestart_of_last_month)
    
    # Combine into a data frame
    combined_data_certo <- data.frame(
      latest_data=latest_data,
      valueend_of_last_month=valueend_of_last_month,
      mean_price_last_month=mean_price_last_month,
      valuestart_of_last_month=valuestart_of_last_month,
      value30 = value30,
      value301 = value301,
      value180 = value180,
      value1801 = value1801,
      value365 = value365,
      value3651 = value3651
    )
    
    write.xlsx(combined_data_certo, file = file.path(save_path, "DADOSBOX.xlsx"), sheetName = "Valores", row.names = FALSE)
    all_models[[state_region]] <- list(model = save_path
    )
  }
  
  # Return the list of paths to saved models
  return(all_models)
}
train_automl(data_tbl, save_dir = "C:/Users/Sergio/OneDrive/ANALISES/FAPEMIG/dashboard/DASHBOARDMILHO/d1/exemplosergio/models11JPA/MESORREGIÃO/")
