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
  #' We build a model aimed at predicting the T°IRM - T°Pameseb diffs according to tsa and ens or CI observed at Pameseb61
  #+ correction-model, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
    #::todo::build_corr_model <- function(records.df, resampling.chr, subset){
      #+ ---------------------------------
      #' ### Tidying data to only keep what we need for modelling purposes
        # Filtering the dataset.
          mod.pameseb61.df <- no_extra_filter$records.df %>%
            dplyr::filter(sid=="61") %>%
            dplyr::select(one_of("tsa", "daily_max", "ens", "ci", "vvt", "diffs","key","day", "means", "mtime"))
          mod.pameseb61.df <- data.frame(mod.pameseb61.df)
          h.check_NA(mod.pameseb61.df)
        #+ ---------------------------------
        #' #### extracting the indices of mtime where tsa = daily_max : validation set for future holdout resampling method
          # daily_max_inds.df <- which(mod.pameseb61.df$tsa == mod.pameseb61.df$daily_max)
          daily_max_inds.df <- mod.pameseb61.df %>%
            dplyr::filter(tsa == daily_max) %>%
            dplyr::select(key)
        #+ ---------------------------------  
        #' #### extracting the indices of mtime where ens > q70(ens(day)) : training set for future holdout resampling method
          # high_rad_inds.df <- which(mod.pameseb61.df$ens>=quantile(x = mod.pameseb61.df$ens, probs=0.70 ))
          high_rad_inds.df <- mod.pameseb61.df %>%
            dplyr::filter(day == TRUE) %>%
            dplyr::filter(ens >= quantile(x = .$ens, probs=0.70 )) %>%
            dplyr::select(key)
        #+ ---------------------------------  
        #' #### extracting the indices of mtime where ci > q70(ci(day)) : training set for future holdout resampling method
          # high_ci_inds.df <- which(mod.pameseb61.df$ci>=quantile(x = mod.pameseb61.df$ci, probs=0.70 ))
          high_ci_inds.df <- mod.pameseb61.df %>%
            dplyr::filter(day == TRUE, ci >= quantile(x = .$ci, probs=0.70 )) %>%
            dplyr::select(key)
      #' ### We have the indices, we can remove the useless features by keeping only ens, vvt and diffs for station Pameseb61
      # Filtering the dataset.
        mod.feat.pameseb61.df <- no_extra_filter$records.df %>%
          dplyr::filter(sid=="61") %>%
          dplyr::select(one_of("ens", "vvt", "diffs" ))
        mod.feat.pameseb61.df <- data.frame(mod.feat.pameseb61.df)
        h.check_NA(mod.feat.pameseb61.df)  
      #+ ---------------------------------
      #' ### Defining the modelization task using [mlr package](https://mlr-org.github.io/mlr-tutorial)
        # loading the mlr library
          library(mlr)
          set.seed(1985)
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
               train.inds = high_rad_inds.df[[1]],
               test.inds = daily_max_inds.df[[1]],
               size=nrow(mod.feat.pameseb61.df)
              ),
              high_ci.holdout.rdesc = makeFixedHoldoutInstance(
               train.inds = high_ci_inds.df[[1]],
               test.inds = daily_max_inds.df[[1]],
               size=nrow(mod.feat.pameseb61.df)
              )
            )
          # defines the tasks (all the same but mlr needs a task by resampling strategy)
            regr.tasks.l = list(
              regr.task1 = mlr::makeRegrTask(
                id = "regr.holdout",
                data = mod.feat.pameseb61.df,
                target = "diffs"
              ),
              regr.task2 = mlr::makeRegrTask(
                id = "regr.cv200",
                data = mod.feat.pameseb61.df,
                target = "diffs"
              ),
              regr.task3 = mlr::makeRegrTask(
                id = "regr.high_rad",
                data = mod.feat.pameseb61.df,
                target = "diffs"
              ),
              regr.task4 = mlr::makeRegrTask(
                id = "regr.high_ci",
                data = mod.feat.pameseb61.df,
                target = "diffs"
              ))
          # Conduct the benchmark experiment
            bmr.l <- benchmark(learners = lrns.l, tasks = regr.tasks.l, resamplings = rsmpls.l)
          # Get the predictions from bmr
          predictions.l <- getBMRPredictions(bmr.l)
    #+ ---------------------------------
    #' ### Pameseb61 observed temperature corrections using the tsa_diff prediction models.
    #' #+ tsa_diff_correction, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
      #+ ---------------------------------
      #' #### Binding the output of the prediction models with the original data
        # Extracting the prediction of the test set (there is an option to also keep the pred made on the training set)
          cv200_pred_diffs.df <- predictions.l$regr.cv200$regr.lm$data %>%
            dplyr::select(one_of(c("id","response"))) %>%
            dplyr::rename(response.cv200 = response)
          
          high_ci_pred_diffs.df <- predictions.l$regr.high_ci$regr.lm$data %>%
            dplyr::select(one_of(c("id","response"))) %>%
            dplyr::rename(response.high_ci = response)
          
          high_rad_pred_diffs.df <- predictions.l$regr.high_rad$regr.lm$data %>%
            dplyr::select(one_of(c("id","response"))) %>%
            dplyr::rename(response.high_rad = response)
          
        # Joining mlr pred output id column with original dataframe key column (original being the Pameseb61 one)
          test.mod.pameseb61.df <- left_join(mod.pameseb61.df, cv200_pred_diffs.df, by = c("key"="id"))
          test.mod.pameseb61.df <- left_join(test.mod.pameseb61.df, high_ci_pred_diffs.df, by = c("key"="id"))
          test.mod.pameseb61.df <- left_join(test.mod.pameseb61.df, high_rad_pred_diffs.df, by = c("key"="id"))
          
        # Preds diffs were computed on validation sets ==> NA values will be replace by the true diff (i.e. we only correct the diffs on the mtime obs corresponding to the validation set)
          test.mod.pameseb61.df <- test.mod.pameseb61.df %>%
            mutate(co.response.cv200 = coalesce(response.cv200, diffs)) %>%
            mutate(co.response.high_ci = coalesce(response.high_ci, diffs)) %>%
            mutate(co.response.high_rad = coalesce(response.high_rad, diffs))
      #+ ---------------------------------
      #' #### Calculating the corrected temperatures
          test.mod.pameseb61.df <- test.mod.pameseb61.df %>%
            mutate(corr.cv200 = tsa + co.response.cv200) %>%
            mutate(corr.high_ci = tsa + co.response.high_ci) %>%
            mutate(corr.high_rad = tsa + co.response.high_rad)
      #+ ---------------------------------
      #' ### Vizualizing the corrected Pameseb61 tsa
        #+ ---------------------------------  
        #' #### Timeseries
          # gathering data
          test <- test.mod.pameseb61.df %>% 
            select(matches("corr."), "tsa", "mtime") %>%
            rename(tsa_61 = tsa) %>%
            gather(tsa_status, tsa, -mtime)
          # adding the RMI data
          irm_tsa <- data.frame(no_extra_filter$records.df %>%
            dplyr::filter(sid=="1000") %>%
            dplyr::select(one_of("mtime", "sid", "tsa")) %>%
            rename(tsa_status = sid)) 
          test <- bind_rows(test, irm_tsa)
          corr_tsa.time.plot <- h.render_plot(records.df = test, sensor_name.chr = "tsa", plot.chr = "timeSerie")
        #+ ---------------------------------
        #' #### Bland-Altman
          corr_bland_altman.plot <- h.compute_ba(
            records.wide.df= h.make_wide(
              input_records.df = ,
              sensor_name.chr = "tsa"
            ),
            output="plot"
          )
          corr_bland_altman.stats.df <- h.compute_ba(records.wide.df= h.make_wide(test, "corr_tsa"), output="table")
#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  



