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
    # creating a joining key var and making it numeric
      no_extra_filter$records.df <- rownames_to_column(df = no_extra_filter$records.df, var = "key" )
      no_extra_filter$records.df <- no_extra_filter$records.df %>%
        mutate_at("key", as.numeric)
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
      #' ### Tidying data to only keep what we need for modelling purposes
        # Filtering the dataset.
          mod.pameseb61.df <- no_extra_filter$records.df %>%
            dplyr::filter(sid=="61") %>%
            dplyr::filter(day==TRUE) %>%  
            dplyr::select(one_of("tsa", "daily_max", "ens", "ci", "vvt", "diffs","key"))
          mod.pameseb61.df <- data.frame(mod.pameseb61.df)
          h.check_NA(mod.pameseb61.df)
        #+ ---------------------------------
        #' #### extracting the indices of mtime where tsa = daily_max : validation set for future holdout resampling method
          daily_max_inds.df <- which(mod.pameseb61.df$tsa == mod.pameseb61.df$daily_max)
        #+ ---------------------------------  
        #' #### extracting the indices of mtime where ens > q70(ens) : training set for future holdout resampling method
          high_rad_inds.df <- which(mod.pameseb61.df$ens>=quantile(x = mod.pameseb61.df$ens, probs=0.70 ))
        #+ ---------------------------------  
        #' #### extracting the indices of mtime where ci > q70(ci) : training set for future holdout resampling method
          high_ci_inds.df <- which(mod.pameseb61.df$ci>=quantile(x = mod.pameseb61.df$ci, probs=0.70 ))
        #' ### REmove the useless features
        # Filtering the dataset.
        mod.features.pameseb61.df <- no_extra_filter$records.df %>%
          dplyr::select(one_of("ens", "vvt", "diffs"))
        mod.pameseb61.df <- data.frame(mod.pameseb61.df)
        h.check_NA(mod.pameseb61.df)  
        #+ ---------------------------------
        #' #### Defining the modelization task using [mlr package](https://mlr-org.github.io/mlr-tutorial)
          # loading the mlr library
          library(mlr)
          #+ ---------------------------------
          #' ##### defining a benchmark experiment to compare various methods
          # Define the learners to be compared
          lrns.l = list(makeLearner("regr.lm"),
                      makeLearner("regr.elmNN")
                      )
          # Define the validation strategies that we want to use
          rsmpls.l = list(
            holdout.rdesc = makeResampleDesc("Holdout"),
            cv200.rdesc = makeResampleDesc("CV", iters = 200),
            high_rad.holdout.rdesc = makeFixedHoldoutInstance(
             train.inds = high_rad_inds.df,
             test.inds = daily_max_inds.df,
             size=nrow(mod.features.pameseb61.df)
            ),
            high_ci.holdout.rdesc = makeFixedHoldoutInstance(
             train.inds = high_ci_inds.df,
             test.inds = daily_max_inds.df,
             size=nrow(mod.features.pameseb61.df)
            )
          )
          # defines the tasks (all the same but mlr needs a task by resampling strategy)
          regr.tasks.l = list(
            regr.task1 = mlr::makeRegrTask(
              id = "regr1",
              data = mod.features.pameseb61.df,
              target = "diffs"
            ),
            regr.task2 = mlr::makeRegrTask(
              id = "regr2",
              data = mod.features.pameseb61.df,
              target = "diffs"
            ),
            regr.task3 = mlr::makeRegrTask(
              id = "regr3",
              target = "diffs"
            ),
            regr.task4 = mlr::makeRegrTask(
              id = "regr4",
              data = mod.features.pameseb61.df,
              target = "diffs"
            ))
          # Conduct a benchmark experiment
          set.seed(1985)
          bmr.l <- benchmark(learners = lrns.l, tasks = regr.tasks.l, resamplings = rsmpls.l)

          

        #' #+ ---------------------------------    
        #' #' #### Defining the learners
        #' #+ --------------------------------- 
        #' #' ##### create the response learner
        #'   resp.regr.lrn = mlr::makeLearner(
        #'     cl = "regr.lm",
        #'     id = "re.regr.lm",
        #'     predict.type = "response"
        #'   )
        #' #+ --------------------------------- 
        #' #' ##### Create the standard error learner
        #'   se.regr.lrn = mlr::makeLearner(
        #'     cl = "regr.lm",
        #'     id = "re.regr.lm",
        #'     predict.type = "se"
        #'   )
        #' #+ ---------------------------------    
        #' #' #### Training the learners
        #'   # train the resp learner to create the regr model on our dataset
        #'     resp.regr.mod = train(resp.regr.lrn, regr.task)
        #'   # train the se learner to create the model on our dataset
        #'     se.regr.mod = train(se.regr.lrn, regr.task)
        #' #+ ---------------------------------  
        #' #' #### Computing & visualizing the predictions using the model
        #' #+ ---------------------------------      
        #' #' ##### Compute the model prediction for tsa_diff using ensPameseb and vvtPameseb for each hourly records
        #'   resp.task.pred = predict(
        #'     object = resp.regr.mod,
        #'     task = regr.task
        #'   )
        #' #+ ---------------------------------      
        #' #' ##### Compute the model SE for tsa_diff using ensPameseb and vvtPameseb for each hourly records
        #'   se.task.pred = predict(
        #'     object = se.regr.mod,
        #'     task = regr.task
        #'   )
        #' #+ ---------------------------------  
        #' #' #### Measuring the performance of the model
        #'   # performance indicators
        #'     resp.regr.perf <- performance(resp.task.pred, measures = list(mse, medse, mae))
        #+ ---------------------------------  
        #' #### Model validation using a custom Holdout strategy - training = ens > q60(ens) & val = daily_max
          # Validating the model with a custom Holdout strategy
            q70.fho.rspl.desc = makeFixedHoldoutInstance(train.inds = high_rad_inds.df, test.inds = daily_max_inds.df, size=nrow(inds.pameseb61.df))
            q70.fho.rspl = resample(resp.regr.lrn, regr.task, fho.rspl.desc, models=TRUE)
        #+ ---------------------------------  
        #' #### Model validation using a 200 folds Cross Validation on the whole dataset
          # Validating the model with a resampling strategy CV 200
            #cv200.rspl.desc = makeResampleDesc("CV", iters = 200)
            #cv200.rspl = resample(resp.regr.lrn, regr.task, cv200.rspl.desc)
        #+ --------------------------------- 
        #' #### Returning the outputs
        # Create the list containing all the outputs
      #::todo::}
  #+ ---------------------------------
  #' ## Pameseb61 observed temperature correction using the tsa_diff prediction model.
  #' #+ tsa_diff_correction, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
    #+ ---------------------------------
    #' ### Getting the output of the prediction model (pred_tsa_diff) for each hourly observation and binding together with observed tsa_diff and the tsa measured at Pameseb)
      # Extracting the prediction of the test set (there is an option to also keep the pred made on the training set)
        pred_diffs.df <- data.frame(pred_fiffs = q70.fho.rspl$pred$data$response)
      # Filtering the mtime original dataset to only keep the keys of observations that were used as validation set
        sub.val.mod.pameseb61.df <- select(subset(mod.pameseb61.df, row.names(mod.pameseb61.df) %in% daily_max_inds.df), "key")    
      # binding cols of the corrected data with the keys of validation set of the original dataframe use for modelling
        pred_diffs.df <- pred_diffs.df %>%
          bind_cols(sub.val.mod.pameseb61.df)
      # joining the validation set containing the corrected data to the original dataframe
        test.key.df <- no_extra_filter$records.df %>%
          #dplyr::filter(sid=="61") %>%
          left_join(pred_diffs.df, by="key")
    #+ ---------------------------------
    #' ### Merging corrected data observations with non-corrected data (i.e. only at observations corresponding to daily_max - which are our validation set) 
      # for BA but is quite useless because ba will compute these
        # test.key.df <- test.key.df %>%
        # mutate(corr_diffs = coalesce(pred_fiffs, diffs))
    #+ ---------------------------------
    #' ### Calculating the corrected temperature using the corr_diffs (i.e. only at observations corresponding to daily_max)
      test.key.df <- test.key.df %>%
        #dplyr::filter(sid=="61") %>%
          mutate_at(.vars="pred_fiffs", funs(replace(.,is.na(.), 0))) %>%
          mutate(corr_tsa = tsa + pred_fiffs)
    #+ ---------------------------------
    #' ### Vizualizing the corrected Pameseb61 tsa
      #+ ---------------------------------  
      #' #### Timeseries
        corr_tsa.time.plot <- h.render_plot(records.df = test.key.df, sensor_name.chr = "corr_tsa", plot.chr = "timeSerie")
      #+ ---------------------------------
      #' #### Bland-Altman
        corr_bland_altman.plot <- h.compute_ba(records.wide.df= h.make_wide(test.key.df, "corr_tsa"), output="plot")
        corr_bland_altman.stats.df <- h.compute_ba(records.wide.df= h.make_wide(test.key.df, "corr_tsa"), output="table")
#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  



