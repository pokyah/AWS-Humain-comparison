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

#+ ---------------------------------
#' ## Script preparation
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
  #+ ---------------------------------
  # get the list of all Pameseb active stations to populate the 2 stations inputs.
    stations.df <- prepare_agromet_API_data.fun(
      get_from_agromet_API.fun(
        user_token.chr= Sys.getenv("AGROMET_API_V1_KEY"),
        table_name.chr = "station",
        stations_ids.chr = "all"
      )
    )
  #+ ---------------------------------
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
  #+ ---------------------------------
  # Get the Humain IRM station (sid=1000)
    records_irm_df <- prepare_agromet_API_data.fun(get_from_agromet_API.fun(
      user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
      table_name.chr = "get_rawdata_irm", 
      stations_ids.chr = "1000",
      sensors.chr = "tsa, ens, vvt, sunrise, sunset",
      dfrom.chr = "2015-11-01",
      dto.chr = "2017-11-01"
    ))
  #+ ---------------------------------
  # Combine the 2 stations datasets in a single dataset for easier data manipulation
    records.df <- h.filter_records(
      records.df = bind_rows(records_pameseb.df, records_irm_df),
      sensor.chr = "tsa",
      dateRange.chr = c(as.Date("2015-11-01"),as.Date("2017-11-01")),
      filter.chr = "no_extra_filter"
    )
    cat(paste0("Your dataset contains ", nrow(records.df)/2, " hourly records"))
    number_of_records.num <- nrow(records.df)/2
  #+ ---------------------------------
  # Filter for eventual NA values remaining at ens et vvt
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
  #+ ---------------------------------
  # Add day or not boolean column
    records.df <- h.is_it_day(records.df)
  # Add rad_top_atmosphere column + CI
    records.df <- h.rad_top_atm(records.df)
  # Add a daily_max column
    records.df <- records.df %>%
      group_by(sid) %>% 
      group_by(date(mtime)) %>%
      mutate(daily_max = max(tsa)) %>%
      ungroup() # function provided by plotly package
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
#' ## Data comparison
#+ data-comparison, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
  #+ ---------------------------------
    # compare-data function declaration  
      compare_data <- function(records.df, filter.chr){
        #+ ---------------------------------
        #' ### filtering
        #+ filtering, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
          records.df <- h.filter_records(
            records.df = records.df,
            sensor.chr = "tsa",
            dateRange.chr = c(as.Date("2015-11-01"),as.Date("2017-11-01")),
            filter.chr = filter.chr
          )
        #+ ---------------------------------
        #' ### Descriptive statistics
          # Compute descriptive statistics for each sensor
            tsa_stats.df <- h.compute_stats(input_records.df = records.df, sensor_name.chr = "tsa")
            vvt_stats.df <- h.compute_stats(input_records.df = records.df, sensor_name.chr = "vvt")
            ens_stats.df <- h.compute_stats(input_records.df = records.df, sensor_name.chr = "ens")
        #+ ---------------------------------
        #' ### Vizualizing the data by sensor
          #+ ---------------------------------
          #' #### Build the scatter plot for each sensors
            ens.scatter.plot <- h.render_plot(records.df = h.make_wide(records.df, "ens"), sensor_name.chr = "ens", plot.chr = "scatter")
            vvt.scatter.plot <- h.render_plot(records.df = h.make_wide(records.df, "vvt"), sensor_name.chr = "vvt", plot.chr = "scatter")
            tsa.scatter.plot <- h.render_plot(records.df = h.make_wide(records.df, "tsa"), sensor_name.chr = "tsa", plot.chr = "scatter")
          #+ ---------------------------------
          #' #### Build the time serie plot for each sensors
            ens.time.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "ens", plot.chr = "timeSerie")
            vvt.time.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "vvt", plot.chr = "timeSerie")
            tsa.time.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "tsa", plot.chr = "timeSerie")
          #+ ---------------------------------  
          #' #### Build the density plot for each sensors
            ens.dens.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "ens", plot.chr = "freq")
            vvt.dens.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "vvt", plot.chr = "freq")
            tsa.dens.plot <- h.render_plot(records.df = records.df, sensor_name.chr = "tsa", plot.chr = "freq")
          #+ ---------------------------------
          #' #### Compute a simple linear model to explain how tsa IRM & Pameseb are correlated 
            regCleanSum.df <- compute_lm(records.wide.df= h.make_wide(records.df, "tsa"), output="regSum")
            regInfoSm.df <- compute_lm(records.wide.df= h.make_wide(records.df, "tsa"), output="infoSum")
        #+ ---------------------------------
        #' ### Bland-Altman Analysis ([explanation](https://pokyah.github.io/howto/assessing-the-agreement-between-two-quantitative-methods-of-measurements-understanding-the-Bland-Altman-analysis/))
          # Compute the Bland Altman plot that express the difference of temperature (IRM-Pameseb) according to mean of temperatures
            bland_altman.plot <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="plot")
            bland_altman.stats.df <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="table")
            bland_altman.data.df  <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="data")
        #+ ---------------------------------
        #' ### Appending Bland Altman data to records.df
          # Joining using dplyr
            records.df <- dplyr::left_join(records.df, select(bland_altman.data.df, c(1,2,3)), by="mtime")
        #+ ---------------------------------
        #' ### Returning the outputs
        #+ ret-outputs, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
          # building the output list  
            return(list(
              records.df = records.df,
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
  #+ ---------------------------------
    # using the compare_data function on various subsets 
      no_extra_filter <- compare_data(records.df = records.df, filter.chr="no_extra_filter")
      # up10deg <- compare_data(records.df = records.df, filter.chr="up10deg")
      # low_rad_high_wind <- compare_data(records.df = records.df, filter.chr="low_rad_high_wind")
      # low_rad_high_wind_up10 <- compare_data(records.df = records.df, filter.chr="low_rad_high_wind_up10")
      # 
      # high_rad_high_wind <- compare_data(records.df = records.df, filter.chr="high_rad_high_wind")
      # below10deg <- compare_data(records.df = records.df, filter.chr="below10deg")
      # high_rad_high_wind_below10 <- compare_data(records.df = records.df, filter.chr="high_rad_high_wind_below10")
      # 
      # q70_ci <- compare_data(records.df = records.df, filter.chr="q70_ci")
#+ ---------------------------------
#' ## Correcting the tsa-diff
  #+ ---------------------------------
  #' ### Declaration of the Function to build tsa-diff Correction models
  #' We build a model aimed at predicting the T°IRM - T°Pameseb diffs according to tsa and ens observed at Pameseb61
  #+ correction-model, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
    #::todo::build_corr_model <- function(records.df, resampling.chr, subset){
      #+ ---------------------------------
      #' ### Tidying data to keep what we need for resampling strategy elaboration 
        inds.pameseb61.df <- no_extra_filter$records.df %>%
          dplyr::filter(sid=="61") %>%
          dplyr::select(one_of("ci","daily_max"))
        inds.pameseb61.df <- data.frame(mod.pameseb61.df)
        h.check_NA(mod.pameseb61.df)
          #+ ---------------------------------
          #' #### extracting the indices of mtime where tsa = daily_max : validation set for future holdout resampling method
            daily_max_inds.df <- which(mod.pameseb61.df$tsa==mod.pameseb61.df$daily_max)
          #+ ---------------------------------  
          #' #### extracting the indices of mtime where ens > q70(ens) : training set for future holdout resampling method
            high_rad_inds.df <- which(mod.pameseb61.df$ens>=quantile(x = mod.pameseb61.df$ens, probs=0.7 ))  
      #+ ---------------------------------
      #' ### Tidying data to only keep what we need for modelling purpose : tsa_diff + ens + vvt 
        mod.pameseb61.df <- no_extra_filter$records.df %>%
          dplyr::filter(sid=="61") %>%
          dplyr::select(one_of("ens","vvt","diffs"))
        mod.pameseb61.df <- data.frame(mod.pameseb61.df)
        h.check_NA(mod.pameseb61.df)
      #+ ---------------------------------  
      #' #### Defining the modelization task using [mlr package](https://mlr-org.github.io/mlr-tutorial)
        # loading the mlr library
          library(mlr)
        # creating the regression task
          regr.task = mlr::makeRegrTask(
            id = "regr",
            data = mod.pameseb61.df,
            target = "diffs"
          )
      #+ ---------------------------------    
      #' #### Defining the learners
      #+ --------------------------------- 
      #' ##### create the response learner
        resp.regr.lrn = mlr::makeLearner(
          cl = "regr.lm",
          id = "re.regr.lm",
          predict.type = "response"
        )
      #+ --------------------------------- 
      #' ##### Create the standard error learner
        se.regr.lrn = mlr::makeLearner(
          cl = "regr.lm",
          id = "re.regr.lm",
          predict.type = "se"
        )
      #+ ---------------------------------    
      #' #### Training the learners
        # train the resp learner to create the regr model on our dataset
          resp.regr.mod = train(resp.regr.lrn, regr.task)
        # train the se learner to create the model on our dataset
          se.regr.mod = train(se.regr.lrn, regr.task)
      #+ ---------------------------------  
      #' #### Computing & visualizing the predictions using the model
      #+ ---------------------------------      
      #' ##### Compute the model prediction for tsa_diff using ensPameseb and vvtPameseb for each hourly records
        resp.task.pred = predict(
          object = resp.regr.mod,
          task = regr.task
        )
      #+ ---------------------------------      
      #' ##### Compute the model SE for tsa_diff using ensPameseb and vvtPameseb for each hourly records
        se.task.pred = predict(
          object = se.regr.mod,
          task = regr.task
        )
      #+ ---------------------------------  
      #' #### Measuring the performance of the model
        # performance indicators
          resp.regr.perf <- performance(resp.task.pred, measures = list(mse, medse, mae))
      #+ ---------------------------------  
      #' #### Model validation using a custom Holdout strategy - training = ens > q70(ens) & val = daily_max
        # Validating the model with a custom Holdout strategy
          fho.rspl.desc = makeFixedHoldoutInstance(train.inds = high_rad_inds.df , test.inds = daily_max_inds.df, size=nrow(inds.pameseb61.df))
          fho.rspl = resample(resp.regr.lrn, regr.task, fho.rspl.desc, models=TRUE)
      #+ ---------------------------------  
      #' #### Model validation using a 200 folds Cross Validation on the whole dataset
        # Validating the model with a resampling strategy CV 200
          #cv200.rspl.desc = makeResampleDesc("CV", iters = 200)
          #cv200.rspl = resample(resp.regr.lrn, regr.task, cv200.rspl.desc)
      #+ ---------------------------------
      #' #### Vizualizing model predictions   
      #+ --------------------------------- 
      #' ##### 2D Visualisation of the prediction of tsa_diff according to ens and vvt
        # resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, task = regr.task)
      #+ --------------------------------- 
      #' ##### 3D Visualisation of the prediction of tsa_diff according to ens and vvt using plotly - [1](https://community.plot.ly/t/3d-scatter-3d-regression-line/4149/6) & [2](https://plot.ly/r/line-and-scatter/)
        # Create the prediction grid to plot on a 3D scatter
          graph_reso <- 0.5
          axis_x <- seq(min(mod.pameseb61.df$ens), max(mod.pameseb61.df$ens), by = graph_reso)
          axis_y <- seq(min(mod.pameseb61.df$vvt), max(mod.pameseb61.df$vvt), by = graph_reso)
          tsadiff_lm_surface <- expand.grid(ens = axis_x, vvt = axis_y,KEEP.OUT.ATTRS = F)
        # predict the values on the prediction grid locations using the model to create the prediction surface
          grid.pred <- predict(
            object = fho.rspl$models[[1]], #resp.regr.mod
            newdata = tsadiff_lm_surface
          )
          # prediction_data <- as.vector(prediction$data[[1]])
          # names(prediction_data) <- seq(1, length(prediction_data), by=1)
          tsadiff_lm_surface$tsa_diff <- as.vector(grid.pred$data[[1]])
          tsadiff_lm_surface <- acast(tsadiff_lm_surface, vvt ~ ens, value.var = "tsa_diff") #y ~ x
        # Building the 3D plot
          resp.pred.plot.3d <- plot_ly(
            data = mod.pameseb61.df,
            x = ~ens,
            y = ~vvt,
            z = ~diffs,
            marker = list(
              size = 3,
              color = ~month,
              colors = h.ggplot_colours(n=12)
            )
          ) %>%
            add_markers() %>%
            add_surface(
              z = tsadiff_lm_surface,
              x = axis_x,
              y = axis_y,
              type = "surface"
            )
      #+ --------------------------------- 
      #' #### Returning the outputs
      # Create the list containing all the outputs
    #::todo::}
#+ ---------------------------------
#' ## Pameseb61 observed temperature correction using the tsa_diff prediction model.
#' #+ tsa_diff_correction, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
  #+ ---------------------------------
  #' ### Getting the output of the prediction model (pred_tsa_diff) for each hourly observation and binding together with observed tsa_diff and the tsa measured at Pameseb)
    # Building the binding dataframe using multiple regressions outputs...
      pred_tsa_diff.df <- fho.rspl$pred$data$response
      colnames(pred_tsa_diff.df) <- "pred_tsa_diff"
      tsa_diff.df <- (records.reshaped.df["tsa_diff"])
      tsa_corr.df <- bind_cols(tsa_wide.df, tsa_diff.df, pred_tsa_diff.df)
      tsa_corr.df <- data.frame(tsa_corr.df)
  #+ ---------------------------------
  #' ### Correcting the measured tsa with the predicted tsa_diff
    # adding the corrected tsa to the dataframe
      tsa_corr.df <- mutate(tsa_corr.df, tsa_61_corr = tsa_61 - pred_tsa_diff)
  #+ ---------------------------------
  #' ### Vizualizing the corrected Pameseb61 tsa
    #+ ---------------------------------
    #' #### keeping what we need 
      # only the temperatures and not the differences
        tsa_corr.df <- dplyr::select(tsa_corr.df, one_of(c("mtime","tsa_61","tsa_1000", "tsa_61_corr")))
    #+ ---------------------------------  
    #' #### Timeseries
      # making long format again for easier plotting of the timeserie
        tsa_corr_long.df <-  data.frame(gather(tsa_corr.df , station, tsa, tsa_61, tsa_1000, tsa_61_corr, -mtime))
      # ploting the timeserie
        tsa_tsa_corr.time.plot <- h.render_plot(tsa_corr_long.df , plot.chr= "timeSerie", "tsa")
    #+ ---------------------------------
    #' #### Bland-Altman
      colnames(tsa_corr_long.df) <- c("mtime", "sid", "tsa")
      bland_altman.corr.plot <- h.compute_ba(records.wide.df= h.make_wide(tsa_corr_long.df, "tsa"), output="plot")
#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  



