rm(list = ls())
setwd("C:/Users/Sergio/OneDrive/ANALISES/FAPEMIG/WEB SCRAPING MILHO")
require(openxlsx)##Buscar o pacote instalado
dados <- read.xlsx("BASEMILHO 16082024.xlsx", sheet = 1)
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
  summarise(Preco = mean(Preco))
preco_por_praca
summary(preco_por_praca)
preco_por_praca1 <- subset(preco_por_praca,preco_por_praca$Preco<125)
preco_por_praca1

preco_por_praca2 <- preco_por_praca1
preco_por_praca2

str(preco_por_praca2)

preco_por_praca2<-preco_por_praca2%>%
  filter(!Região %in% c("Ourinhos","Abelardo Luz","Ribeirão do Sul"))
preco_por_praca2

#preco_por_praca2<-preco_por_praca2 %>% 
#filter(!Estado %in% c("BA","GO","MA",'MG',"MS","MT","PE","PR","RO","RS","SC","TO"))


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
  select(Estado,Região, DATA, Preco)
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

preco_B3<- data_tbl%>%
  filter(Estado_Região %in% c("SP/Campinas - Esalq/B3"))
preco_B3
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
    train_tbl <- subset_data[subset_data$DATA < "2021-01-11", ]
    test_tbl <- subset_data[subset_data$DATA >= "2021-01-11" & subset_data$DATA <= "2023-01-01", ]
    forecast_tbl <- subset_data[subset_data$DATA > "2023-01-01", ]
    data_tbl1 <- separate(subset_data, col=Estado_Região, into=c('Estado', 'Região'), sep='/')
    data_tbl1$Região_Estado<- paste(data_tbl1$Região,data_tbl1$Estado, sep = "/")
    region_state <- unique(data_tbl1$Região_Estado)
    # Save the objects train_tbl, test_tbl, and forecast_tbl as Excel files
    file_prefix <- paste0(save_dir, state_region)
    save_path <- paste0(file_prefix, "")
    
    # Train AutoML model for the specific state-region combination
    automl_model <- h2o.glm(
      x = setdiff(names(subset_data), c("Preco", "Estado", "Região", "mes", "ano", "DATA", "season", "Estado_Região",'trend',"trend_sqr","rev_lag_1")),
      y = "Preco",
      training_frame = as.h2o(train_tbl),
      validation_frame = as.h2o(test_tbl),
      nfolds = 5,
      family = "gaussian",
      seed = 1975
    )
    
    # Print information about the trained model
    #print(automl_model)
    # Save the trained model
    save_path <- paste0(file_prefix, "")
    h2o.saveModel(object = automl_model, path = save_path)
    #write.xlsx(train_tbl, file = file.path(save_path, "treino.xlsx"), sheetName = "treino", row.names = FALSE)
    #write.xlsx(test_tbl, file = file.path(save_path, "teste.xlsx"), sheetName = "teste", row.names = FALSE)
    #write.xlsx(forecast_tbl, file = file.path(save_path, "previsao.xlsx"), sheetName = "previsao", row.names = FALSE)
    perf_h2o  <- h2o.performance(automl_model, newdata = as.h2o(test_tbl)) 
    
    R2   <- perf_h2o %>% h2o.r2()  
    RMSE <- perf_h2o %>% h2o.rmse()
    MSE <- perf_h2o %>% h2o.mse()
    MAE <- perf_h2o %>% h2o.mae()
    Metricas<-tibble(R2, RMSE, MSE, MAE)
    #write.xlsx(Metricas, file = file.path(save_path, "Metricaspredicao.xlsx"), sheetName = "Metricas", row.names = FALSE)
    ###Function model automl
    require(agua)
    model_spec1 <- linear_reg(penalty = 0.03133692, mixture = 0.5) %>%
      parsnip::set_engine(
        engine                      = 'h2o',
        nfolds = 5,
        family = "gaussian",
        seed= 1975, 
        solver="IRLSM",
        max_iterations=2,
        link="identity",
        fold_assignment="Random",
        objective_epsilon=1e-04,
        gradient_epsilon=1e-04,
        lambda_min_ratio=1e-04,
        max_active_predictors=5000,
        obj_reg=0.000237023
      ) 
    
    #print(model_spec1)
    #print(automl_model@parameters)
    model_fitted <- model_spec1 %>%
      fit(Preco ~ MA, data = train_tbl)
    
    #print(model_fitted)
    
    modeltime_tbl <- modeltime_table(
      model_fitted
    ) 
    
    #print(modeltime_tbl)
    # PREDICT ----
    #print(predict(model_fitted, test_tbl))
    
    modeltime_tbl %>%
      modeltime_calibrate(test_tbl) %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = FALSE
      )
    #print(Metricas)
    
    test_h2o   <- as.h2o(test_tbl)
    test_h2o<-test_h2o[-1,] 
    pred_h2o  <- h2o.predict(automl_model, newdata = as.h2o(test_h2o)) 
    predicao<-pred_h2o %>% 
      as_tibble() %>% 
      cbind(test_tbl %>% select(DATA, Preco))
    #print(predicao)
    #write.xlsx(predicao, file = file.path(save_path, "predicao.xlsx"), sheetName = "predicao", row.names = FALSE)
    perf_h2o1  <- h2o.performance(automl_model, newdata = as.h2o(forecast_tbl)) 
    
    R2   <- perf_h2o1 %>% h2o.r2()  
    RMSE <- perf_h2o1 %>% h2o.rmse()
    MSE <- perf_h2o1 %>% h2o.mse()
    MAE <- perf_h2o1 %>% h2o.mae()
    Metricas1<-tibble(R2, RMSE, MSE, MAE)
    #print(Metricas1)
    write.xlsx(Metricas1, file = file.path(save_path, "Metricasprevisao.xlsx"), sheetName = "Metricas", row.names = FALSE)
    test_h2o1   <- as.h2o(forecast_tbl)
    test_h2o1<-test_h2o1[-1,] 
    pred_h2o1  <- h2o.predict(automl_model, newdata = as.h2o(test_h2o1)) 
    previsao<-pred_h2o1 %>% 
      as_tibble() %>% 
      cbind(forecast_tbl %>% select(DATA, Preco))
    #print(previsao)
    #write.xlsx(previsao, file = file.path(save_path, "previsaomodelo.xlsx"), sheetName = "previsao", row.names = FALSE)
    error_tbl1 <- previsao %>% 
      mutate(
        error     = Preco - predict,
        error_pct = error / Preco
      ) %>% 
      select(DATA, Preco, predict, error, error_pct)
    #print(error_tbl1)
    write.xlsx(error_tbl1, file = file.path(save_path, "ERROPREVISAO.xlsx"), sheetName = "Erroprevisao", row.names = FALSE)
    f_error1 <- 
      error_tbl1 %>% summarise(
        n=length(error),
        mean = mean(error),
        var = sum((error-mean)^2)/(n-1),
        std = sqrt(var),
        mae = mean(abs(error)),
        rmse = mean(error^2)^0.5,
        mape = mean(abs(error_pct)),
        mpe = mean(error_pct),
        skew = sum(((error - mean)/std)^3)/n,
        kurtosis = sum(((error - mean)/std)^4)/n-3
      ) 
    
    #print(f_error1)
    write.xlsx(f_error1, file = file.path(save_path, "METRICASERROPREVISAO.xlsx"), sheetName = "METRICASErroprevisao", row.names = FALSE)
    data_prepared_tbl <- bind_rows(train_tbl, test_tbl,forecast_tbl)
    calibration_tbl <- modeltime_tbl %>%
      modeltime_calibrate(data_prepared_tbl)
    #print(calibration_tbl)
    refit_tbl <- modeltime_tbl %>%
      modeltime_refit(data_prepared_tbl)
    #print(refit_tbl)
    resultados<-modeltime_tbl %>%
      modeltime_calibrate(data_prepared_tbl) %>%
      modeltime_forecast(
        new_data    = data_prepared_tbl,
        actual_data = data_prepared_tbl,
        keep_data   = FALSE
      )
    resultados<-data.frame(resultados)
    # Filter actual values
    data1 <- subset(resultados, .key == "actual")
    
    # Convert .value to numeric and handle possible locale issues
    data1$.value <- as.numeric(gsub(",", ".", as.character(data1$.value)))
    
    # Ensure .index is of Date type
    data1$.index <- as.Date(data1$.index)
    
    # Get the latest date
    latest_date <- max(data1$.index, na.rm = TRUE)
    
    library(dplyr)
    library(lubridate)
    
    # Define the latest date
    latest_date <- max(data1$.index)
    
    # Calculate the date 30 days ago
    trinta_days_ago <- latest_date - days(30)
    
    # Add a new column to calculate the difference in days from trinta_days_ago
    data_with_diff30 <- data1 %>%
      mutate(date_diff30 = abs(difftime(.index, trinta_days_ago, units = "days")))
    
    # Find the closest date to trinta_days_ago
    closest_date30 <- data_with_diff30 %>%
      filter(date_diff30 == min(date_diff30)) %>%
      pull(.index) %>%
      unique()  # Assuming there could be multiple dates with the same minimum difference
    
    closest_date30
    # Filter the data for the closest date
    filtered_data30 <- data1 %>%
      filter(.index %in% closest_date30)
    
    filtered_data30
    
    # Find the closest date and filter data
    filtered_data30 <- data1 %>%
      mutate(date_diff30 = abs(difftime(.index, trinta_days_ago, units = "days"))) %>%
      filter(date_diff30 == min(date_diff30)) %>%
      select(-date_diff30)%>%slice(1)  # Remove the date_diff column if no longer needed
    filtered_data30
    
    value30 <- filtered_data30 %>%
      pull(.value)
    value30
    value301 <- filtered_data30 %>%
      pull(.index)
    value301 <- format(value301, "%d-%m-%Y")
    value301
    
    # Calculate the date 180 days ago
    seis_meses_ago <- latest_date - days(181)
    
    # Add a new column to calculate the difference in days from seis_meses_ago
    data_with_diff180 <- data1 %>%
      mutate(date_diff180 = abs(difftime(.index, seis_meses_ago, units = "days")))
    
    # Find the closest date to seis_meses_ago
    closest_date180 <- data_with_diff180 %>%
      filter(date_diff180 == min(date_diff180)) %>%
      pull(.index) %>%
      unique()  # Assuming there could be multiple dates with the same minimum difference
    
    closest_date180
    # Filter the data for the closest date
    filtered_data180 <- data1 %>%
      filter(.index %in% closest_date180)
    
    filtered_data180
    
    # Find the closest date and filter data
    filtered_data180 <- data1 %>%
      mutate(date_diff180 = abs(difftime(.index, seis_meses_ago, units = "days"))) %>%
      filter(date_diff180 == min(date_diff180)) %>%
      select(-date_diff180)%>%slice(1)  # Remove the date_diff column if no longer needed
    filtered_data180
    
    value180 <- filtered_data180 %>%
      pull(.value) 
    value180
    value1801 <- filtered_data180 %>%
      pull(.index)
    value1801 <- format(value1801, "%d-%m-%Y")
    value1801
    
    # Calculate the date 365 days ago
    seis_meses_ago <- latest_date - days(366)
    
    # Add a new column to calculate the difference in days from seis_meses_ago
    data_with_diff365 <- data1 %>%
      mutate(date_diff365 = abs(difftime(.index, seis_meses_ago, units = "days")))
    
    # Find the closest date to seis_meses_ago
    closest_date365 <- data_with_diff365 %>%
      filter(date_diff365 == min(date_diff365)) %>%
      pull(.index) %>%
      unique()  # Assuming there could be multiple dates with the same minimum difference
    
    closest_date365
    # Filter the data for the closest date
    filtered_data365 <- data1 %>%
      filter(.index %in% closest_date365)
    
    filtered_data365
    
    # Find the closest date and filter data
    filtered_data365 <- data1 %>%
      mutate(date_diff365 = abs(difftime(.index, seis_meses_ago, units = "days"))) %>%
      filter(date_diff365 == min(date_diff365)) %>%
      select(-date_diff365)%>%slice(1)  # Remove the date_diff column if no longer needed
    filtered_data365
    
    value365 <- filtered_data365 %>%
      pull(.value)
    value365
    value3651 <- filtered_data365 %>%
      pull(.index)
    value3651 <- format(value3651, "%d-%m-%Y")
    value3651
    
    # Get the latest price by finding the most recent date
    latest_data <- data1 %>%
      arrange(desc(.index)) %>%
      slice(1) %>%
      pull(.value)
    
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
    filtered_data <- data1 %>%
      filter(.index >= start_of_last_month & .index <= end_of_last_month)%>%slice(1)
    
    # Calculate the mean price for the filtered period
    mean_price_last_month <- filtered_data %>%
      pull(.value) %>%
      mean(na.rm = TRUE)
    
    mean_price_last_month
    
    valueend_of_last_month <- format(end_of_last_month, "%d-%m-%Y")
    valueend_of_last_month
    
    valuestart_of_last_month <- format(start_of_last_month, "%d-%m-%Y")
    valuestart_of_last_month
    
    # Combine into a data frame
    combined_data_certo <- data.frame(
      latest_data=round(latest_data,2),
      valueend_of_last_month=valueend_of_last_month,
      mean_price_last_month=round(mean_price_last_month,2),
      valuestart_of_last_month=valuestart_of_last_month,
      value30 = round(value30,2),
      value301 = value301,
      value180 = round(value180,2),
      value1801 = value1801,
      value365 = round(value365,2),
      value3651 = value3651
    )
    
    write.xlsx(combined_data_certo, file = file.path(save_path, "DADOSBOX.xlsx"), sheetName = "Valores", row.names = FALSE)
    # FUTURE FORECAST 15 days
    future_tbl <- data_prepared_tbl %>%
      future_frame(.length_out = "15 days") %>%
      ungroup()
    #print(future_tbl)
    data_prepared_tbl1 <- bind_rows(train_tbl, test_tbl, future_tbl)
    #print(data_prepared_tbl1)
    data_prepared_tbl2<-data_prepared_tbl1 %>% select(DATA, Preco, MA)
    future <- subset(data_prepared_tbl2, data_prepared_tbl2$DATA > last(data_prepared_tbl$DATA))
    #print(future)
    require(forecastML)
    data_forecast <- create_lagged_df(data_prepared_tbl, type = "forecast", outcome_col = 1,
                                      horizons =1:15, lookback = 1:15)
    #print(data_forecast)
    dados15dias<-data_forecast$horizon_15$MA_lag_15
    #print(dados15dias)
    #print(future)
    #print(data_prepared_tbl1)
    d <- future %>% mutate(MA = dados15dias)
    
    #print(d)
    dia1<-d$MA[15]+mean(diff(d$MA[1:15]))
    dia2<-dia1+mean(diff(d$MA[2:15]))
    dia3<-dia2+mean(diff(d$MA[3:15]))
    dia4<-dia3+mean(diff(d$MA[4:15]))
    dia5<-dia4+mean(diff(d$MA[5:15]))
    dia6<-dia5+mean(diff(d$MA[6:15]))
    dia7<-dia6+mean(diff(d$MA[7:15]))
    dia8<-dia7+mean(diff(d$MA[8:15]))
    dia9<-dia8+mean(diff(d$MA[9:15]))
    dia10<-dia9+mean(diff(d$MA[10:15]))
    dia11<-dia10+mean(diff(d$MA[11:15]))
    dia12<-dia11+mean(diff(d$MA[12:15]))
    dia13<-dia12+mean(diff(d$MA[13:15]))
    dia14<-dia13+mean(diff(d$MA[14:15]))
    dia15<-dia14+mean(diff(d$MA[1:15]))
    
    dadosjuntos15dias<-c(dia1,dia2,dia3,
                         dia4,dia5,
                         dia6,dia7,dia8,
                         dia9,dia10,
                         dia11,dia12,dia13,
                         dia14,dia15)
    #print(dadosjuntos15dias)
    d1<- future %>% mutate(MA = dadosjuntos15dias)
    
    #print(d1)
    forecast_tbl1 <- tail(forecast_tbl, 15)
    
    DADOS15DIAS<-modeltime_tbl %>%
      modeltime_calibrate(forecast_tbl) %>%
      modeltime_forecast(
        new_data    = d1,
        actual_data = forecast_tbl1,
        keep_data   = FALSE
      )
    DADOS15DIAS<-data.frame(DADOS15DIAS)
    dados15diasjunto<-bind_rows(resultados,DADOS15DIAS)
    tail(dados15diasjunto)
    dados15<-subset(dados15diasjunto,dados15diasjunto$.key=="actual")
    
    dados151<-subset(dados15diasjunto,dados15diasjunto$.key=="prediction")
    
    dados15$.value<-round(dados15$.value,2)
    dados151$.value<-round(dados151$.value,2)
    require(lubridate)
    
    ano5<-max(dados15$.index) - years(5)
    dados315<-dados15[dados15$.index >= ano5, ]
    dados415<-dados151[dados151$.index >= ano5, ]
    require(dplyr)
    require(highcharter)
    graph151<-highchart(type = "stock") %>% 
      hc_add_series(data = dados315, 
                    type = "line", 
                    hcaes(x = .index, y = round(.value,2)), 
                    name = "Real") %>% 
      hc_add_series(data = dados415, 
                    type = "spline", 
                    hcaes(x = as.Date(.index), y = round(.value,2)), 
                    name = "Predito",
                    id = "fit", # this is for link the arearange series to this one and have one legend
                    lineWidth = 1) %>% 
      hc_add_series(
        data = dados415,
        type = "arearange",
        hcaes(x = as.Date(.index), low = round(.conf_lo,2), high = round(.conf_hi,2)),
        name = "Intervalo",
        linkedTo = "fit", # here we link the legends in one.
        color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
        zIndex = -3 # this is for put the series in a back so the points are showed first
      )
    
    graph151 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text = "Preço do milho (R$/saca de 60 kg)")
    
    library(highcharter)
    library(htmlwidgets)
    save_path152 <- paste0(htmlwidgets::saveWidget(widget = graph151 %>% 
                                                     hc_exporting(enabled = TRUE) %>% 
                                                     hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em",region_state)), #the graph to export
                                                   file = file.path(save_path, "highchart152.html"),
                                                   selfcontained = TRUE))
    #write.xlsx(DADOS15DIAS, file = file.path(save_path, "PREVISAO15DIAS.xlsx"), sheetName = "15 dias", row.names = FALSE)
    # FUTURE FORECAST 7 days
    future_tbl7 <- data_prepared_tbl %>%
      future_frame(.length_out = "7 days") %>%
      ungroup()
    #print(future_tbl)
    data_prepared_tbl17 <- bind_rows(train_tbl, test_tbl, future_tbl7)
    #print(data_prepared_tbl1)
    data_prepared_tbl27<-data_prepared_tbl17 %>% select(DATA, Preco, MA)
    future7 <- subset(data_prepared_tbl27, data_prepared_tbl27$DATA > last(data_prepared_tbl$DATA))
    #print(future)
    require(forecastML)
    data_forecast7 <- create_lagged_df(data_prepared_tbl, type = "forecast", outcome_col = 1,
                                       horizons =1:7, lookback = 1:7)
    #print(data_forecast)
    dados7dias<-data_forecast7$horizon_7$MA_lag_7
    #print(dados15dias)
    #print(future)
    #print(data_prepared_tbl1)
    d7 <- future7 %>% mutate(MA = dados7dias)
    
    #print(d)
    dia1<-d7$MA[7]+mean(diff(d7$MA[1:7]))
    dia2<-dia1+mean(diff(d7$MA[2:7]))
    dia3<-dia2+mean(diff(d7$MA[3:7]))
    dia4<-dia3+mean(diff(d7$MA[4:7]))
    dia5<-dia4+mean(diff(d7$MA[5:7]))
    dia6<-dia5+mean(diff(d7$MA[6:7]))
    dia7<-dia6+mean(diff(d7$MA[1:7]))
    
    dadosjuntos7dias<-c(dia1,dia2,dia3,
                        dia4,dia5,
                        dia6,dia7)
    #print(dadosjuntos15dias)
    d17<- future7 %>% mutate(MA = dadosjuntos7dias)
    
    #print(d1)
    forecast_tbl17 <- tail(forecast_tbl, 15)
    DADOS7DIAS<-modeltime_tbl %>%
      modeltime_calibrate(forecast_tbl) %>%
      modeltime_forecast(
        new_data    = d17,
        actual_data = forecast_tbl17,
        keep_data   = FALSE
      ) 
    DADOS7DIAS<-data.frame(DADOS7DIAS)
    dados7diasjunto<-bind_rows(resultados,DADOS7DIAS)
    tail(dados7diasjunto)
    dados7<-subset(dados7diasjunto,dados7diasjunto$.key=="actual")
    
    dados71<-subset(dados7diasjunto,dados7diasjunto$.key=="prediction")
    
    dados7$.value<-round(dados7$.value,2)
    dados71$.value<-round(dados71$.value,2)
    require(lubridate)
    
    ano5<-max(dados7$.index) - years(5)
    dados37<-dados7[dados7$.index >= ano5, ]
    dados47<-dados71[dados71$.index >= ano5, ]
    require(dplyr)
    require(highcharter)
    graph71<-highchart(type = "stock") %>% 
      hc_add_series(data = dados37, 
                    type = "line", 
                    hcaes(x = .index, y = round(.value,2)), 
                    name = "Real") %>% 
      hc_add_series(data = dados47, 
                    type = "spline", 
                    hcaes(x = as.Date(.index), y = round(.value,2)), 
                    name = "Predito",
                    id = "fit", # this is for link the arearange series to this one and have one legend
                    lineWidth = 1) %>% 
      hc_add_series(
        data = dados47,
        type = "arearange",
        hcaes(x = as.Date(.index), low = round(.conf_lo,2), high = round(.conf_hi,2)),
        name = "Intervalo",
        linkedTo = "fit", # here we link the legends in one.
        color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
        zIndex = -3 # this is for put the series in a back so the points are showed first
      )
    
    graph71 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text = "Preço do milho (R$/saca de 60 kg)")
    
    library(highcharter)
    library(htmlwidgets)
    save_path72 <- paste0(htmlwidgets::saveWidget(widget = graph71 %>% 
                                                    hc_exporting(enabled = TRUE) %>% 
                                                    hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em",region_state)), #the graph to export
                                                  file = file.path(save_path, "highchart72.html"),
                                                  selfcontained = TRUE))
    
    #write.xlsx(DADOS7DIAS, file = file.path(save_path, "PREVISAO7DIAS.xlsx"), sheetName = "7 dias", row.names = FALSE)
    # FUTURE FORECAST 30 days
    future_tbl30 <- data_prepared_tbl %>%
      future_frame(.length_out = "30 days") %>%
      ungroup()
    #print(future_tbl)
    data_prepared_tbl130 <- bind_rows(train_tbl, test_tbl, future_tbl30)
    #print(data_prepared_tbl1)
    data_prepared_tbl230<-data_prepared_tbl130 %>% select(DATA, Preco, MA)
    future30 <- subset(data_prepared_tbl230, data_prepared_tbl230$DATA > last(data_prepared_tbl$DATA))
    #print(future)
    require(forecastML)
    data_forecast30 <- create_lagged_df(data_prepared_tbl, type = "forecast", outcome_col = 1,
                                        horizons =1:30, lookback = 1:30)
    #print(data_forecast)
    dados30dias<-data_forecast30$horizon_30$MA_lag_30
    #print(dados15dias)
    #print(future)
    #print(data_prepared_tbl1)
    d30 <- future30 %>% mutate(MA = dados30dias)
    
    #print(d)
    dia1<-d30$MA[30]+mean(diff(d30$MA[1:30]))
    dia2<-dia1+mean(diff(d30$MA[2:30]))
    dia3<-dia2+mean(diff(d30$MA[3:30]))
    dia4<-dia3+mean(diff(d30$MA[4:30]))
    dia5<-dia4+mean(diff(d30$MA[5:30]))
    dia6<-dia5+mean(diff(d30$MA[6:30]))
    dia7<-dia6+mean(diff(d30$MA[7:30]))
    dia8<-dia7+mean(diff(d30$MA[8:30]))
    dia9<-dia8+mean(diff(d30$MA[9:30]))
    dia10<-dia9+mean(diff(d30$MA[10:30]))
    dia11<-dia10+mean(diff(d30$MA[11:30]))
    dia12<-dia11+mean(diff(d30$MA[12:30]))
    dia13<-dia12+mean(diff(d30$MA[13:30]))
    dia14<-dia13+mean(diff(d30$MA[14:30]))
    dia15<-dia14+mean(diff(d30$MA[15:30]))
    dia16<-dia15+mean(diff(d30$MA[16:30]))
    dia17<-dia16+mean(diff(d30$MA[17:30]))
    dia18<-dia17+mean(diff(d30$MA[18:30]))
    dia19<-dia18+mean(diff(d30$MA[19:30]))
    dia20<-dia19+mean(diff(d30$MA[20:30]))
    dia21<-dia20+mean(diff(d30$MA[21:30]))
    dia22<-dia21+mean(diff(d30$MA[22:30]))
    dia23<-dia22+mean(diff(d30$MA[23:30]))
    dia24<-dia23+mean(diff(d30$MA[24:30]))
    dia25<-dia24+mean(diff(d30$MA[25:30]))
    dia26<-dia25+mean(diff(d30$MA[26:30]))
    dia27<-dia26+mean(diff(d30$MA[27:30]))
    dia28<-dia27+mean(diff(d30$MA[28:30]))
    dia29<-dia28+mean(diff(d30$MA[29:30]))
    dia30<-dia29+mean(diff(d30$MA[1:30]))
    
    dadosjuntos30dias<-c(dia1,dia2,dia3,
                         dia4,dia5,
                         dia6,dia7,
                         dia8,
                         dia9,dia10,
                         dia11,dia12,dia13,
                         dia14,dia15,dia16,
                         dia17,dia18,dia19,dia20,
                         dia21,dia22,
                         dia23,dia24, dia25, dia26,
                         dia27, dia28, dia29,
                         dia30)
    #print(dadosjuntos15dias)
    d130<- future30 %>% mutate(MA = dadosjuntos30dias)
    
    #print(d1)
    forecast_tbl130 <- tail(forecast_tbl, 15)
    
    DADOS30DIAS<-modeltime_tbl %>%
      modeltime_calibrate(forecast_tbl) %>%
      modeltime_forecast(
        new_data    = d130,
        actual_data = forecast_tbl130,
        keep_data   = FALSE
      ) 
    DADOS30DIAS<-data.frame(DADOS30DIAS)
    dados30diasjunto<-bind_rows(resultados,DADOS30DIAS)
    tail(dados30diasjunto)
    dados30<-subset(dados30diasjunto,dados30diasjunto$.key=="actual")
    
    dados301<-subset(dados30diasjunto,dados30diasjunto$.key=="prediction")
    
    dados30$.value<-round(dados30$.value,2)
    dados301$.value<-round(dados301$.value,2)
    require(lubridate)
    
    ano5<-max(dados30$.index) - years(5)
    dados330<-dados30[dados30$.index >= ano5, ]
    dados430<-dados301[dados301$.index >= ano5, ]
    require(dplyr)
    require(highcharter)
    graph301<-highchart(type = "stock") %>% 
      hc_add_series(data = dados330, 
                    type = "line", 
                    hcaes(x = .index, y = round(.value,2)), 
                    name = "Real") %>% 
      hc_add_series(data = dados430, 
                    type = "spline", 
                    hcaes(x = as.Date(.index), y = round(.value,2)), 
                    name = "Predito",
                    id = "fit", # this is for link the arearange series to this one and have one legend
                    lineWidth = 1) %>% 
      hc_add_series(
        data = dados430,
        type = "arearange",
        hcaes(x = as.Date(.index), low = round(.conf_lo,2), high = round(.conf_hi,2)),
        name = "Intervalo",
        linkedTo = "fit", # here we link the legends in one.
        color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
        zIndex = -3 # this is for put the series in a back so the points are showed first
      )
    
    graph301 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text = "Preço do milho (R$/saca de 60 kg)")
    
    library(highcharter)
    library(htmlwidgets)
    save_path302 <- paste0(htmlwidgets::saveWidget(widget = graph301 %>% 
                                                     hc_exporting(enabled = TRUE) %>% 
                                                     hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em",region_state)), #the graph to export
                                                   file = file.path(save_path, "highchart302.html"),
                                                   selfcontained = TRUE))
    #write.xlsx(DADOS30DIAS, file = file.path(save_path, "PREVISAO30DIAS.xlsx"), sheetName = "30 dias", row.names = FALSE)
    # FUTURE FORECAST 45 days
    future_tbl45 <- data_prepared_tbl %>%
      future_frame(.length_out = "45 days") %>%
      ungroup()
    #print(future_tbl)
    data_prepared_tbl145<- bind_rows(train_tbl, test_tbl, future_tbl45)
    #print(data_prepared_tbl1)
    data_prepared_tbl245<-data_prepared_tbl145 %>% select(DATA, Preco, MA)
    future45 <- subset(data_prepared_tbl245, data_prepared_tbl245$DATA > last(data_prepared_tbl$DATA))
    #print(future)
    require(forecastML)
    data_forecast45 <- create_lagged_df(data_prepared_tbl, type = "forecast", outcome_col = 1,
                                        horizons =1:45, lookback = 1:45)
    #print(data_forecast)
    dados45dias<-data_forecast45$horizon_45$MA_lag_45
    #print(dados15dias)
    #print(future)
    #print(data_prepared_tbl1)
    d45 <- future45 %>% mutate(MA = dados45dias)
    
    #print(d)
    dia1<-d45$MA[45]+mean(diff(d45$MA[1:45]))
    dia2<-dia1+mean(diff(d45$MA[2:45]))
    dia3<-dia2+mean(diff(d45$MA[3:45]))
    dia4<-dia3+mean(diff(d45$MA[4:45]))
    dia5<-dia4+mean(diff(d45$MA[5:45]))
    dia6<-dia5+mean(diff(d45$MA[6:45]))
    dia7<-dia6+mean(diff(d45$MA[7:45]))
    dia8<-dia7+mean(diff(d45$MA[8:45]))
    dia9<-dia8+mean(diff(d45$MA[9:45]))
    dia10<-dia9+mean(diff(d45$MA[10:45]))
    dia11<-dia10+mean(diff(d45$MA[11:45]))
    dia12<-dia11+mean(diff(d45$MA[12:45]))
    dia13<-dia12+mean(diff(d45$MA[13:45]))
    dia14<-dia13+mean(diff(d45$MA[14:45]))
    dia15<-dia14+mean(diff(d45$MA[15:45]))
    dia16<-dia15+mean(diff(d45$MA[16:45]))
    dia17<-dia16+mean(diff(d45$MA[17:45]))
    dia18<-dia17+mean(diff(d45$MA[18:45]))
    dia19<-dia18+mean(diff(d45$MA[19:45]))
    dia20<-dia19+mean(diff(d45$MA[20:45]))
    dia21<-dia20+mean(diff(d45$MA[21:45]))
    dia22<-dia21+mean(diff(d45$MA[22:45]))
    dia23<-dia22+mean(diff(d45$MA[23:45]))
    dia24<-dia23+mean(diff(d45$MA[24:45]))
    dia25<-dia24+mean(diff(d45$MA[25:45]))
    dia26<-dia25+mean(diff(d45$MA[26:45]))
    dia27<-dia26+mean(diff(d45$MA[27:45]))
    dia28<-dia27+mean(diff(d45$MA[28:45]))
    dia29<-dia28+mean(diff(d45$MA[29:45]))
    dia30<-dia29+mean(diff(d45$MA[30:45]))
    dia31<-dia30+mean(diff(d45$MA[31:45]))
    dia32<-dia31+mean(diff(d45$MA[32:45]))
    dia33<-dia32+mean(diff(d45$MA[33:45]))
    dia34<-dia33+mean(diff(d45$MA[34:45]))
    dia35<-dia34+mean(diff(d45$MA[35:45]))
    dia36<-dia35+mean(diff(d45$MA[36:45]))
    dia37<-dia36+mean(diff(d45$MA[37:45]))
    dia38<-dia37+mean(diff(d45$MA[38:45]))
    dia39<-dia38+mean(diff(d45$MA[39:45]))
    dia40<-dia39+mean(diff(d45$MA[40:45]))
    dia41<-dia40+mean(diff(d45$MA[41:45]))
    dia42<-dia41+mean(diff(d45$MA[42:45]))
    dia43<-dia42+mean(diff(d45$MA[43:45]))
    dia44<-dia43+mean(diff(d45$MA[44:45]))
    dia45<-dia44+mean(diff(d45$MA[1:45]))
    dadosjuntos45dias<-c(dia1,dia2,dia3,
                         dia4,dia5,
                         dia6,dia7,
                         dia8,
                         dia9,dia10,
                         dia11,dia12,dia13,
                         dia14,dia15,dia16,
                         dia17,dia18,dia19,dia20,
                         dia21,dia22,
                         dia23,dia24, dia25, dia26,
                         dia27, dia28, dia29,
                         dia30,dia31,dia32,dia33,
                         dia34,dia35,
                         dia36,dia37,
                         dia38,
                         dia39,dia40,
                         dia41,dia42,dia43,
                         dia44,dia45
    )
    #print(dadosjuntos15dias)
    d145<- future45 %>% mutate(MA = dadosjuntos45dias)
    
    #print(d1)
    forecast_tbl145 <- tail(forecast_tbl, 15)
    
    DADOS45DIAS<-modeltime_tbl %>%
      modeltime_calibrate(forecast_tbl) %>%
      modeltime_forecast(
        new_data    = d145,
        actual_data = forecast_tbl145,
        keep_data   = FALSE
      ) 
    DADOS45DIAS<-data.frame(DADOS45DIAS)
    dados45diasjunto<-bind_rows(resultados,DADOS45DIAS)
    tail(dados45diasjunto)
    dados45<-subset(dados45diasjunto,dados45diasjunto$.key=="actual")
    
    dados451<-subset(dados45diasjunto,dados45diasjunto$.key=="prediction")
    
    dados45$.value<-round(dados45$.value,2)
    dados451$.value<-round(dados451$.value,2)
    require(lubridate)
    
    ano5<-max(dados45$.index) - years(5)
    dados345<-dados45[dados45$.index >= ano5, ]
    dados445<-dados451[dados451$.index >= ano5, ]
    require(dplyr)
    require(highcharter)
    graph451<-highchart(type = "stock") %>% 
      hc_add_series(data = dados345, 
                    type = "line", 
                    hcaes(x = .index, y = round(.value,2)), 
                    name = "Real") %>% 
      hc_add_series(data = dados445, 
                    type = "spline", 
                    hcaes(x = as.Date(.index), y = round(.value,2)), 
                    name = "Predito",
                    id = "fit", # this is for link the arearange series to this one and have one legend
                    lineWidth = 1) %>% 
      hc_add_series(
        data = dados445,
        type = "arearange",
        hcaes(x = as.Date(.index), low = round(.conf_lo,2), high = round(.conf_hi,2)),
        name = "Intervalo",
        linkedTo = "fit", # here we link the legends in one.
        color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
        zIndex = -3 # this is for put the series in a back so the points are showed first
      )
    
    graph451 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text = "Preço do milho (R$/saca de 60 kg)")
    
    library(highcharter)
    library(htmlwidgets)
    save_path452 <- paste0(htmlwidgets::saveWidget(widget = graph451 %>% 
                                                     hc_exporting(enabled = TRUE) %>% 
                                                     hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em",region_state)), #the graph to export
                                                   file = file.path(save_path, "highchart452.html"),
                                                   selfcontained = TRUE))
    
    DADOStudo<-data.frame(subset_data)
    DADOStudo$Preco<-round(DADOStudo$Preco,2)
    DADOStudo
    preco_B3<-data.frame(preco_B3)
    preco_B3$Preco<-round(preco_B3$Preco,2)
    require(lubridate)
    
    ano5<-max(DADOStudo$DATA) - years(5)
    dados3151<-DADOStudo[DADOStudo$DATA >= ano5, ]
    dados3151
    #ano51<-max(preco_B3$DATA) - years(5)
    dados4151<-preco_B3[preco_B3$DATA >= ano5, ]
    dados4151
    require(dplyr)
    dadoscerto<-merge(x = dados3151, y =dados4151, by = "DATA", all = FALSE)
    dadoscerto
    lm.model <- augment(lm(Preco.x-Preco.y ~ DATA, data = dadoscerto))
    require(dplyr)
    require(highcharter)
    graphtudo2<-highchart(type = "stock") %>% 
      hc_add_series(data = dados3151, 
                    type = "line", 
                    hcaes(x = DATA, y = round(Preco,2)), 
                    name = "Real")%>% 
      hc_add_series(data = dados4151, 
                    type = "spline", 
                    hcaes(x = as.Date(DATA), y = round(Preco,2)), 
                    name = "B3",
                    id = "fit", # this is for link the arearange series to this one and have one legend
                    lineWidth = 2)%>%  
      hc_add_series(data = dadoscerto,
                    type = "spline",
                    hcaes(x = as.Date(DATA), y = round(Preco.x-Preco.y,2)),
                    name = "Basis",
                    linkedTo = "fit", # here we link the legends in one.
                    color ="orange"  # put a semi transparent color
      ) %>%
      hc_add_series(lm.model, "line", hcaes(x = DATA, y = round(.fitted,2)),
                    name = "Basis médio",
                    linkedTo = "fit", # here we link the legends in one.
                    color ="red")  # put a semi transparent color)
    
    graphtudo2 %>% 
      hc_exporting(enabled = TRUE) %>% 
      hc_title(text = "Preço do milho (R$/saca de 60 kg)")
    
    library(highcharter)
    library(htmlwidgets)
    save_pathtudo3 <- paste0(htmlwidgets::saveWidget(widget = graphtudo2 %>% 
                                                       hc_exporting(enabled = TRUE) %>% 
                                                       hc_title(text =paste("Preço do milho (R$/saca de 60 kg) em", region_state)), #the graph to export
                                                     file = file.path(save_path, "highcharttudo3.html"),
                                                     selfcontained = TRUE))
    #write.xlsx(DADOS45DIAS, file = file.path(save_path, "PREVISAO45DIAS.xlsx"), sheetName = "45 dias", row.names = FALSE)
    all_models[[state_region]] <- list(model = save_path, train = train_tbl, test = test_tbl, forecast = forecast_tbl 
    )
  }
  
  # Return the list of paths to saved models
  return(all_models)
}
train_automl(data_tbl, save_dir = "C:/Users/Sergio/OneDrive/ANALISES/FAPEMIG/dashboard/DASHBOARDMILHO/d1/exemplosergio/models11/")

