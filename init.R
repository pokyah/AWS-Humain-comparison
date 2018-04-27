#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
#'output: 
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'title: "R script to build the Humain intercomparison analysis"
#'date: \ 17-04-2018\

#'---TODO:: normalize
#' Tester la différence sur les daily max
#' 
#' Clearness index ==> filtre dataset original pour les valeurs >0.6 ou seuil au-delà duquel,n 30% d'observations (correspondent à périodes d'effets radiatifs)
#' apprendre le modèle sur base de cce jue filtré
#' Valider le modèles sur les daily max
#' Comparaison daily max corr et originaux
#' Recréer une série temporelle continue en réintégrant les data nondaily maxima
#' comparer les paires de stations proches pour l'ensoleillement pour voir si le modèle est transposable
#' Aussi faire une validation à Humian sur les data nov a avril 2018


#+ ---------------------------------
#' ## Script preparation
#' 
#+ preparation, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

# Automagically set the wd and the root of all projects 
if (!require("here")) install.packages("here")
library(here)
wd.chr <- here::here()

# loading the library to manage all the other libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
requiredPackages <- read.csv("./settings/requiredPackages.csv", quote = "", sep = ",", header=TRUE, stringsAsFactors=FALSE)
p_load(char=requiredPackages$packageName, character.only=TRUE )
p_loaded()

# Dynamic Sourcing of all the required functions
source(paste0("../../pokyah/R-utilities/R-utilities.R"))
source_files_recursively.fun("./R")
source_files_recursively.fun("../agrometeor-public/R/")


#+ ---------------------------------
#' ## Data Acquisition
#' In order to get the Pameseb data from the [API](https://app.pameseb.be/fr/), you need your own token to be stored in you [.Renviron file](https://csgillespie.github.io/efficientR/set-up.html#r-startup).
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

## get the list of all Pameseb active stations to populate the 2 stations inputs.
stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr= Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "station",
    stations_ids.chr = "all"
  )
)
#------- 
# Get the Humain Pameseb station (sid=61)
records_pameseb.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "61",
    sensors.chr = "tsa, ens, vvt, sunrise, sunset",
    dfrom.chr = "2015-11-01",
    dto.chr = "2017-11-01"
  )
)

# Get the Humain IRM station (sid=1000)
records_irm_df <- prepare_agromet_API_data.fun(get_from_agromet_API.fun(
  user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
  table_name.chr = "get_rawdata_irm", 
  stations_ids.chr = "1000",
  sensors.chr = "tsa, ens, vvt, sunrise, sunset",
  dfrom.chr = "2015-11-01",
  dto.chr = "2017-11-01"
))

compare_data <- function(records_irm_df, records_pameseb_df, filter.chr){
  
  # Combine the 2 stations datasets in a single dataset for easier data manipulation
  records.df <- h.filter_records(
    records.df = bind_rows(records_pameseb.df, records_irm_df),
    sensor.chr = "tsa",
    dateRange.chr = c(as.Date("2015-11-01"),as.Date("2017-11-01")),
    filter.chr = filter.chr
  )
  cat(paste0("Your dataset contains ", nrow(records.df)/2, " hourly records"))
  number_of_records.num <- nrow(records.df)/2

  #Filter for eventual NA values remaining at ens et vvt
  pameseb61.df <- records.df %>% 
    dplyr::filter(sid== unique(records.df$sid)[1]) %>%
    dplyr::filter(!is.na(vvt)) %>%
    dplyr::filter(!is.na(ens))
  
  irm1000.df <- records.df %>%
    dplyr::filter(sid== unique(records.df$sid)[2]) %>%
    dplyr::filter(!is.na(vvt)) %>%
    dplyr::filter(!is.na(ens))
  
  irm1000.df <- semi_join(irm1000.df, pameseb61.df, by="mtime")
  pameseb61.df <- semi_join(pameseb61.df, irm1000.df, by="mtime")
  
  records.df <- bind_rows(pameseb61.df, irm1000.df)
  h.check_NA(records.df)

  # Add day or not boolean column
  records.df <- h.is_it_day(records.df)

  #+ ---------------------------------
  #' ## localization
  #+ localization, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
  responsiveness.chr = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'"
  localization.map <- leaflet() %>% 
    addProviderTiles(group = "Satellite",
                     providers$Esri.WorldImagery,
                     options = providerTileOptions(opacity = 1)
    ) %>% 
    addProviderTiles(group = "Stamen",
                     providers$Stamen.Toner,
                     options = providerTileOptions(opacity = 0.25)
    ) %>%
    addCircles(data = records.df[1,], radius = 100) %>%
    addLayersControl(baseGroups = c("Stamen", "Satellite"))%>%
    htmlwidgets::onRender(paste0("
                                 function(el, x) {
                                 $('head').append(",responsiveness.chr,");
                                 }"))

  #+ ---------------------------------
  #' ## Temperature data comparison
  #+ temp-data-comparison, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
  
  #' ### Descriptive statistics
  # Compute descriptive statistics for each sensor
  tsa_stats.df <- h.compute_stats(input_records.df = records.df, sensor_name.chr = "tsa")
  vvt_stats.df <- h.compute_stats(input_records.df = records.df, sensor_name.chr = "vvt")
  ens_stats.df <- h.compute_stats(input_records.df = records.df, sensor_name.chr = "ens")
  
  #' ### Browsing the data by sensor
  # Build the scatter plot for each sensors
  ens.scatter.plot <- h.render_plot(records.df = h.make_wide(records.df, "ens"), sensor_name.chr = "ens", plot.chr = "scatter")
  vvt.scatter.plot <- h.render_plot(records.df = h.make_wide(records.df, "vvt"), sensor_name.chr = "vvt", plot.chr = "scatter")
  tsa.scatter.plot <- h.render_plot(records.df = h.make_wide(records.df, "tsa"), sensor_name.chr = "tsa", plot.chr = "scatter")
  
  # Build the time serie plot for each sensors
  ens.time.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "ens", plot.chr = "timeSerie")
  vvt.time.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "vvt", plot.chr = "timeSerie")
  tsa.time.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "tsa", plot.chr = "timeSerie")
  
  # Build the density plot for each sensors
  ens.dens.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "ens", plot.chr = "freq")
  vvt.dens.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "vvt", plot.chr = "freq")
  tsa.dens.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "tsa", plot.chr = "freq")
  
  #' ### Correlation between the 2 stations temperature
  # Compute a simple linear model to explain how tsa IRM & Pameseb are correlated 
  regCleanSum.df <- compute_lm(records.wide.df= h.make_wide(records.df, "tsa"), output="regSum")
  regInfoSm.df <- compute_lm(records.wide.df= h.make_wide(records.df, "tsa"), output="infoSum")
  
  #' ### Bland-Altman Analysis ([explanation](https://pokyah.github.io/howto/assessing-the-agreement-between-two-quantitative-methods-of-measurements-understanding-the-Bland-Altman-analysis/))
  # Compute the Bland Altman plot that express the difference of temperature (Pameseb-IRM) according to mean of temperatures
  bland_altman.plot <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="plot")
  bland_altman.stats.df<- bland.altman.stats(
    (h.make_wide(records.df, "tsa"))[3][[1]], # pameseb61
    (h.make_wide(records.df, "tsa"))[2][[1]]  # irm1000
  )
  
  #+ ---------------------------------
  #' ## Returning the outputs
  #+ ret-outputs, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
  return(list(
    dataset.df = records.df,
    statistics.l = list(
      tsa_stats.df = tsa_stats.df,
      vvt_stats.df = vvt_stats.df,
      ens_stats.df = ens_stats.df
    ),
    localization.map = localization.map,
    plots = list(
      scatters.plot = list(
        ens.scatter.plot = ens.scatter.plot,
        tsa.scatter.plot = tsa.scatter.plot,
        vvt.scatter.plot = vvt.scatter.plot
      ),
      timeseries.plot = list(
        ens.time.plot = ens.time.plot,
        tsa.time.plot = tsa.time.plot,
        vvt.time.plot = vvt.time.plot
      ),
      densities.plot = list(
        ens.dens.plot = ens.dens.plot,
        tsa.dens.plot = tsa.dens.plot,
        vvt.dens.plot = vvt.dens.plot
      ),
      lm.mod = list(
        regCleanSum.df = regCleanSum.df,
        regInfoSm.df = regInfoSm.df
      )
    ),
    blandAltman = list(
      bland_altman.plot = bland_altman.plot,
      bland_altman.stats.df = bland_altman.stats.df 
    )
  ))
}

no_extra_filter <- compare_data(records_irm_df=records_irm_df, records_pameseb_df=records_pameseb_df, filter.chr="no_extra_filter")
low_rad_high_wind <- compare_data(records_irm_df=records_irm_df, records_pameseb_df=records_pameseb_df, filter.chr="low_rad_high_wind")
high_rad_low_wind <- compare_data(records_irm_df=records_irm_df, records_pameseb_df=records_pameseb_df, filter.chr="high_rad_low_wind")

#+ ---------------------------------
#' ## Temperature data interpretation
#+ temp-data-interpreation, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#' ### How is temperature correlated to irradiance and windspeed ?
# Build a 3D Scatter plot to show how temperature (tsa) is linked to irradiance (ens) and windspeed (vvt). The colors express the months of the year
# records.df <- records.df %>% mutate(month = as.factor(month(mtime)))
# tsa_vvt_ens.scatter.plot <- plotly::plot_ly(
#   data = records.df,
#   x = ~tsa,
#   y = ~ens,
#   z = ~vvt,
#   color = ~month,
#   colors = h.ggplot_colours(n=12),
#   marker = list(
#     size = 3
#   )
# )%>%
#   layout(scene = list(xaxis = list(title = 'tsa'),
#                       yaxis = list(title = 'ens'),
#                       zaxis = list(title = 'vvt')))
# 
# records.df <- low_rad_high_wind.records.df %>% mutate(month = as.factor(month(mtime)))
# tsa_vvt_ens.scatter.plot <- plotly::plot_ly(
#   data = records.df,
#   x = ~tsa,
#   y = ~ens,
#   z = ~vvt,
#   color = ~month,
#   colors = h.ggplot_colours(n=12),
#   marker = list(
#     size = 3
#   )
# )%>%
#   layout(scene = list(xaxis = list(title = 'tsa'),
#                       yaxis = list(title = 'ens'),
#                       zaxis = list(title = 'vvt')))
# 
# 
# 
# # Transform each sensor dataset to [wide format](https://www.r-bloggers.com/data-manipulation-with-tidyr/) for later integration with [mlr](http://mlr-org.github.io/mlr/index.html) package data format
# tsa_wide.df <- h.make_wide(records.df, "tsa")
# colnames(no_extra_filter.tsa_wide.df) <- c("mtime", "tsa_1000", "tsa_61")
# ens_wide.df <- h.make_wide(records.df, "ens")
# colnames(ens_wide.df) <- c("mtime", "ens_1000", "ens_61")
# vvt_wide.df <- h.make_wide(records.df, "vvt")
# colnames(vvt_wide.df) <- c("mtime", "vvt_1000", "vvt_61")
# tsa_diffs.df <- data.frame(bland_altman.stats.df$diffs)
# colnames(tsa_diffs.df) <- "tsa_diff"
# no_extra_filter.months.df <- data.frame(as.factor(month(tsa_wide.df$mtime)))
# colnames(months.df) <- "month"
# is_it_day_wide.df <- h.make_wide(records.df, "day")
# colnames(is_it_day_wide.df) <- c("mtime", "day_1000", "day_61")

#+ ---------------------------------
#' ## Temperature difference prediction (Pameseb - IRM) using a correction model based on irradiance and windspeed at Pameseb 
#' IRM uses a mechanically ventilated [Stevenson screen](https://www.weathershop.co.uk/shop/media/catalog/product/cache/6/image/800x/b216e8ab95b1018bca410e090021ba3f/m/s/ms_met01_1.jpg) while Pameseb uses a more simple [custom setup](https://www.pameseb.be/meteo_intro/infos_techniques/temperature.html)
#' Thus it is expected that Pameseb shows warmer temperatures while windspeed is low and irradiance is high.
#' Pameseb is the network we want to make more WMO rules compliant.
#' To do so, we will use IRM as a reference golden standard for temperature and use a correction model for Pameseb.
#' The model will use ens and vvt measured by Pameseb to explain the temperature difference between IRM and Pameseb
#+ tsa_diff_pred, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#' ### Preparing data for tsa_diff prediction model
#' We only keep ens and vvt from Pameseb because this is the network that we want to correct

#' # Binding together the 4 variables of interest : ensPameseb, vvtPameseb, tsaDiff and day/night
#' records.reshaped.df <- bind_cols(ens_wide.df[3], vvt_wide.df[3], tsa_diffs.df, months.df, 1*(is_it_day_wide.df[3]))
#' print(head(records.reshaped.df))
#' h.check_NA(records.reshaped.df)
#' records.reshaped.df <- data.frame(records.reshaped.df)
#' 
#' #' ### Defining the modelization task using [mlr package](https://mlr-org.github.io/mlr-tutorial)
#' 
#' # loading the mlr library
#' library(mlr)
#' 
#' # creating the regression task
#' regr.task = mlr::makeRegrTask(
#'   id = "regr",
#'   data = records.reshaped.df,
#'   target = "tsa_diff"
#' )
#' 
#' # Get some information about the task
#' getTaskDesc(regr.task)
#' 
#' # Remove useless feature : month (it is not an explanatory variable and day or night is already explanied by irradiance)
#' regr.task <- dropFeatures(regr.task, c("month","day_61"))
#' 
#' #' ### Defining the learner
#' 
#' # create the response learner
#' resp.regr.lrn = mlr::makeLearner(
#'   cl = "regr.lm",
#'   id = "re.regr.lm",
#'   predict.type = "response" #could also be "se"
#' )
#' 
#' # create the standard error learner
#' se.regr.lrn = mlr::makeLearner(
#'   cl = "regr.lm",
#'   id = "re.regr.lm",
#'   predict.type = "se"
#' )
#' 
#' #' ### Training the learner
#' 
#' # train the resp learner to create the regr model on our dataset
#' resp.regr.mod = train(resp.regr.lrn, regr.task)
#' 
#' # train the se learner to create the model on our dataset
#' se.regr.mod = train(se.regr.lrn, regr.task)
#' 
#' #' ### Checking model summary
#' 
#' # Get infos about the model
#' resp.regr.mod$learner
#' print(resp.regr.mod$learner.model)
#' print(summary(resp.regr.mod$learner.model))
#' print(resp.regr.mod$features)
#' print(resp.regr.mod$task.desc$size)
#' 
#' #' ### Computing & visualizing the prediction using the model
#' 
#' # Compute the model prediction for tsa_diff using ensPameseb and vvtPameseb for each hourly records
#' resp.task.pred = predict(
#'   object = resp.regr.mod,
#'   task = regr.task
#' )
#' 
#' # Compute the model SE for tsa_diff using ensPameseb and vvtPameseb for each hourly records
#' se.task.pred = predict(
#'   object = se.regr.mod,
#'   task = regr.task
#' )
#' 
#' # Inspect the difference between the true, predicted and SE values
#' print(signif(head(getPredictionTruth(resp.task.pred)),2))
#' print(signif(head(getPredictionResponse(resp.task.pred)),2))
#' print(signif(head(getPredictionSE(se.task.pred)), 2))
#' 
#' # Visualising the prediction of tsa_diff according to ens and vvt
#' resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, task = regr.task)
#' resp.pred.plot
#' 
#' # Visualising the prediction of tsa_diff according to ens only
#' ens.resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, features= "ens_61", task = regr.task)
#' ens.resp.pred.plot
#' 
#' # Visualising the prediction of tsa_diff according to vvt only
#' vvt.resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, features= "vvt_61", task = regr.task)
#' vvt.resp.pred.plot
#' 
#' # Measuring the performance of the model
#' performance(resp.task.pred, measures = list(mse, medse, mae))
#' 
#' # Measuring the time required for training the model
#' performance(resp.task.pred, measures = timetrain, model = resp.regr.mod)
#' 
#' #' ### Model validation using a 200 folds Cross Validation
#' 
#' # Validating the model with a resampling strategy CV 200
#' cv200.rdesc = makeResampleDesc("CV", iters = 200)
#' cv200.rdesc
#' cv200.r = resample(resp.regr.lrn, regr.task, cv200.rdesc)
#' cv200.r$aggr
#' 
#' # Validating the model with a resampling strategy LOO
#' # loo.rdesc = makeResampleDesc("LOO")
#' # loo.rdesc
#' # loo.r = resample(resp.regr.lrn, regr.task, loo.rdesc)
#' 
#' #' ### Vizualizing model predictions
#' #' We do this using plotly.
#' #' ::help:: https://community.plot.ly/t/3d-scatter-3d-regression-line/4149/6
#' #' ::help:: https://plot.ly/r/line-and-scatter/
#' 
#' # Create the prediction grid to plot on a 3D scatter 
#' graph_reso <- 0.5
#' axis_x <- seq(min(records.reshaped.df$ens_61), max(records.reshaped.df$ens_61), by = graph_reso)
#' axis_y <- seq(min(records.reshaped.df$vvt_61), max(records.reshaped.df$vvt_61), by = graph_reso)
#' tsadiff_lm_surface <- expand.grid(ens_61 = axis_x, vvt_61 = axis_y,KEEP.OUT.ATTRS = F)
#' 
#' # predict the values on the prediction grid locations using the model
#' grid.pred <- predict(
#'   object = resp.regr.mod,
#'   newdata = tsadiff_lm_surface
#' )
#' # prediction_data <- as.vector(prediction$data[[1]])
#' # names(prediction_data) <- seq(1, length(prediction_data), by=1)
#' tsadiff_lm_surface$tsa_diff <- as.vector(grid.pred$data[[1]])
#' tsadiff_lm_surface <- acast(tsadiff_lm_surface, vvt_61 ~ ens_61, value.var = "tsa_diff") #y ~ x
#' 
#' tsa_diff_ens61_vvt61.plot <- plot_ly(
#'   data = records.reshaped.df, 
#'   x = ~ens_61, 
#'   y = ~vvt_61, 
#'   z = ~tsa_diff,
#'   marker = list(
#'     size = 3,  
#'     color = ~month,
#'     colors = h.ggplot_colours(n=12)
#'   )
#' ) %>% 
#'   add_markers() %>% 
#'   add_surface(
#'     z = tsadiff_lm_surface,
#'     x = axis_x,
#'     y = axis_y,
#'     type = "surface"
#'   )
#' 
#' tsa_diff_ens61_vvt61.plot
#' 
#' #' ### Maybe that we can find a better correction model. Let's play around with 2 different learners and make a Benchmark
#' 
#' # list the learners our linear regression(regr.lm), a simple neural network(regr.nnet)
#' # lrns = list(
#' #   mlr::makeLearner(
#' #     cl = "regr.lm",
#' #     id = "re.regr.lm",
#' #     predict.type = "response" #could also be "se"
#' #   ),
#' #   mlr::makeLearner(
#' #     cl = "regr.nnet",
#' #     id = "re.regr.nnet"
#' #   )
#' # )
#' # 
#' # # Choose the resampling strategy for cross validation
#' # cv200.rdesc = makeResampleDesc("CV", iters = 200)
#' # 
#' # # Perform the Benchmark
#' # bmr = benchmark(lrns, regr.task, cv200.rdesc)
#' # 
#' # # Inspect benchmark results
#' # getBMRPerformances(bmr)
#' # print(getBMRAggrPerformances(bmr))
#' # getBMRPredictions(bmr, as.df=TRUE)
#' # getBMRModels(bmr)
#' # getBMRLearners(bmr)
#' # getBMRMeasures(bmr)
#' # plotBMRBoxplots(bmr)
#' # # plotBMRSummary(bmr)
#' # # plotBMRRanksAsBarChart(bmr)
#' 
#' #+ ---------------------------------
#' #' ## Pameseb observed temperature correction using the tsa_diff prediction model.
#' #' As we don't really grasp how the neural Network works, we use the lm even if it's aggregated RMSE is slightly higher 
#' #+ tsa_diff_correction, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
#' 
#' #' ### Getting the output of the prediction model (pred_tsa_diff) for each hourly observation and binding together with observed tsa_diff and the tsa measured at Pameseb)
#' 
#' # Building the binding dataframe using multiple regressions outputs...
#' pred_tsa_diff.df <- data.frame(resp.task.pred$data$response)
#' colnames(pred_tsa_diff.df) <- "pred_tsa_diff"
#' tsa_diff.df <- (records.reshaped.df["tsa_diff"])
#' head(tsa_diff.df)
#' head(pred_tsa_diff.df)
#' head(tsa_wide.df)
#' tsa_corr.df <- bind_cols(tsa_wide.df, tsa_diff.df, pred_tsa_diff.df)
#' tsa_corr.df <- data.frame(tsa_corr.df)
#' 
#' #' ### Correcting the measured tsa with the predicted tsa_diff
#' 
#' # adding the corrected tsa to the dataframe
#' tsa_corr.df <- mutate(tsa_corr.df, tsa_61_corr = tsa_61 - pred_tsa_diff)
#' head(tsa_corr.df)
#' 
#' #' ### Comparing the summary statistics between IRM, Pameseb and Pameseb corrected
#' summary(tsa_corr.df$tsa_61)
#' summary(tsa_corr.df$tsa_61_corr)
#' summary(tsa_corr.df$tsa_1000)
#' 
#' #' ### Vizualizing temperature scatter plots for both measured and corrected
#' 
#' # keeping what we need : only the temperatures and not the differences
#' tsa_corr.df <- dplyr::select(tsa_corr.df, one_of(c("mtime","tsa_61","tsa_1000", "tsa_61_corr")))
#' 
#' 
#' #' ### Comparing the observed and corrected time series 
#' # making long format again for easier plotting of the timeserie
#' tsa_corr_long.df <-  data.frame(gather(tsa_corr.df , station, tsa, tsa_61, tsa_1000, tsa_61_corr, -mtime))
#' 
#' # ploting the timeserie
#' tsa_tsa_corr.time.plot <- h.render_plot(tsa_corr_long.df , plot.chr= "timeSerie", "tsa")
#' ggplotly(tsa_tsa_corr.time.plot)
#' 
#' # plotting the new Bland-Altman
#' colnames(tsa_corr_long.df) <- c("mtime", "sid", "tsa")
#' bland_altman.corr.plot <- h.compute_ba(records.wide.df= h.make_wide(tsa_corr_long.df, "tsa"), output="plot")
#' bland_altman.stats.df<- bland.altman.stats(
#'   (h.make_wide(records.df, "tsa"))[3][[1]], # pameseb61
#'   (h.make_wide(records.df, "tsa"))[2][[1]]  # irm1000
#' )

#' It seems that the model cannot efficiently make the correction for the high temperature.
#' Maybe should we only compute a model on a subset of the whole dataframe and apply it only on these cases

#+ ---------------------------------
#' ## Rendering the report
#' We do it using knitr
#+ rendering, echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  



