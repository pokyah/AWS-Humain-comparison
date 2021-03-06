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
      mutate(date = date(mtime))
    records.df <- records.df %>%
      group_by(sid) %>% 
      group_by(date) %>%
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
          #' #### Build the august zoomed time serie plot for each sensors
            august.ens.time.plot <- h.render_plot(records.df = dplyr::filter(records.df, mtime %in% as.POSIXct("2016-08-15T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-09-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")), sensor_name.chr = "ens", plot.chr = "timeSerie")
            august.vvt.time.plot <- h.render_plot(records.df = dplyr::filter(records.df, mtime %in% as.POSIXct("2016-08-15T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-09-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")), sensor_name.chr = "vvt", plot.chr = "timeSerie")
            august.tsa.time.plot <- h.render_plot(records.df = dplyr::filter(records.df, mtime %in% as.POSIXct("2016-08-15T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-09-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")), sensor_name.chr = "tsa", plot.chr = "timeSerie")
          #+ ---------------------------------
          #' #### Build the august zoomed time serie plot for each sensors
            january.ens.time.plot <- h.render_plot(records.df = dplyr::filter(records.df, mtime %in% as.POSIXct("2016-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-01-16T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")), sensor_name.chr = "ens", plot.chr = "timeSerie")
            january.vvt.time.plot <- h.render_plot(records.df = dplyr::filter(records.df, mtime %in% as.POSIXct("2016-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-01-16T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")), sensor_name.chr = "vvt", plot.chr = "timeSerie")
            january.tsa.time.plot <- h.render_plot(records.df = dplyr::filter(records.df, mtime %in% as.POSIXct("2016-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-01-16T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")), sensor_name.chr = "tsa", plot.chr = "timeSerie")
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
            
            bland_altman.plot <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="plot")
            bland_altman.stats.df <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="table")
            bland_altman.data.df  <- h.compute_ba(records.wide.df= h.make_wide(records.df, "tsa"), output="data")
        #+ ---------------------------------
        #' ### Appending Bland Altman data to records.df
          # Joining using dplyr
            records.df <- dplyr::left_join(records.df, dplyr::select(bland_altman.data.df, c(1,2,3)), by="mtime")
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
                august.timeseries.plot = list(
                  august.ens.time.plot = august.ens.time.plot,
                  august.tsa.time.plot = august.tsa.time.plot,
                  august.vvt.time.plot = august.vvt.time.plot
                ),
                january.timeseries.plot = list(
                  january.ens.time.plot = january.ens.time.plot,
                  january.tsa.time.plot = january.tsa.time.plot,
                  january.vvt.time.plot = january.vvt.time.plot
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

      night_only <- compare_data(records.df = records.df, filter.chr="night_only")
    # creating a joining key var and making it numeric
      night_only$records.df <- rownames_to_column(df = night_only$records.df, var = "key" )
      night_only$records.df <- night_only$records.df %>%
        mutate_at("key", as.numeric)
      
      daily_max_pm2_only <- compare_data(records.df = records.df, filter.chr="daily_max_only")
    # creating a joining key var and making it numeric
      daily_max_pm2_only$records.df <- rownames_to_column(df = daily_max_pm2_only$records.df, var = "key" )
      daily_max_pm2_only$records.df <- daily_max_pm2_only$records.df %>%
        mutate_at("key", as.numeric)
      
    # ::todo::  non_daily_max_pm2_only <- compare_data(records.df = records.df, filter.chr="non_daily_max_only")
    # creating a joining key var and making it numeric

      
      
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
            dplyr::select(one_of("tsa", "daily_max", "ens", "ci", "vvt", "diffs","key", "day", "mtime", "sid"))
          mod.pameseb61.df <- data.frame(mod.pameseb61.df)
          h.check_NA(mod.pameseb61.df)
        #+ ---------------------------------
        #' #### extracting the indices of mtime where tsa = daily_max : validation set for future holdout resampling method
          # daily_max_inds.df <- which(mod.pameseb61.df$tsa == mod.pameseb61.df$daily_max)
          daily_max_inds.df <- mod.pameseb61.df %>%
            dplyr::filter(tsa == daily_max) %>%
            dplyr::select(key)
          daily_max_mp2_inds.df <- dplyr::bind_rows(
            daily_max_inds.df +1,
            daily_max_inds.df -1,
            daily_max_inds.df +2,
            daily_max_inds.df -2,
            daily_max_inds.df) %>%
          mutate(key=sort(key))
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
      #' ### We have the indices, we can remove the useless features by keeping only ens, vvt, tsa and diffs for station Pameseb61
      # Filtering the dataset.
        mod.feat.pameseb61.df <- no_extra_filter$records.df %>%
          dplyr::filter(sid=="61") %>%
          dplyr::select(one_of("ens", "vvt", "tsa", "diffs" ))
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
                          makeLearner("regr.elmNN"),
                          makeLearner("regr.fnn")
                          )
          
          # defines the tasks (all the same but mlr needs a task by resampling strategy)
            hci.regr.tasks.l = list(
              regr.task.all.hci = mlr::makeRegrTask(
                id = "regr.all.hci",
                data = mod.feat.pameseb61.df,
                target = "diffs"
              ),
              regr.task.ens.hci = mlr::makeRegrTask(
                id = "regr.ens.hci",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("ens", "diffs"))),
                target = "diffs"
              ),
              regr.task.vvt.hci = mlr::makeRegrTask(
                id = "regr.vvt.hci",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("vvt", "diffs"))),
                target = "diffs"
              ),
              regr.task.tsa.hci = mlr::makeRegrTask(
                id = "regr.tsa.hci",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("tsa", "diffs"))),
                target = "diffs"
              ),
              regr.task.ens_vvt.hci = mlr::makeRegrTask(
                id = "regr.ens_vvt.hci",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("ens", "vvt", "diffs"))),
                target = "diffs"
              ),
              regr.task.ens_tsa.hci = mlr::makeRegrTask(
                id = "regr.ens_tsa.hci",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("ens", "tsa", "diffs"))),
                target = "diffs"
              ),
              regr.task.vvt_tsa.hci = mlr::makeRegrTask(
                id = "regr.vvt_tsa.hci",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("vvt", "tsa", "diffs"))),
                target = "diffs"
              )
            )
            
            rnd.regr.tasks.l = list(
              regr.task.all.rnd = mlr::makeRegrTask(
                id = "regr.all.rnd",
                data = mod.feat.pameseb61.df,
                target = "diffs"
              ),
              regr.task.ens.rnd = mlr::makeRegrTask(
                id = "regr.ens.rnd",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("ens", "diffs"))),
                target = "diffs"
              ),
              regr.task.vvt.rnd = mlr::makeRegrTask(
                id = "regr.vvt.rnd",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("vvt", "diffs"))),
                target = "diffs"
              ),
              regr.task.tsa.rnd = mlr::makeRegrTask(
                id = "regr.tsa.rnd",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("tsa", "diffs"))),
                target = "diffs"
              ),
              regr.task.ens_vvt.rnd = mlr::makeRegrTask(
                id = "regr.ens_vvt.rnd",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("ens", "vvt", "diffs"))),
                target = "diffs"
              ),
              regr.task.ens_tsa.hci = mlr::makeRegrTask(
                id = "regr.ens_tsa.rnd",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("ens", "tsa", "diffs"))),
                target = "diffs"
              ),
              regr.task.vvt_tsa.rnd = mlr::makeRegrTask(
                id = "regr.vvt_tsa.rnd",
                data = dplyr::select(mod.feat.pameseb61.df, one_of(c("vvt", "tsa", "diffs"))),
                target = "diffs"
              )
            )
            
          # Define the validation strategies that we want to use
            fixed_holdout_strategy = mlr::makeFixedHoldoutInstance(
              train.inds = high_ci_inds.df[[1]],
              test.inds = daily_max_mp2_inds.df[[1]],
              size=nrow(mod.feat.pameseb61.df)
            )
            
            hci.rsmpls.l = list(
              regr.all.hci = fixed_holdout_strategy,
              regr.ens.hci = fixed_holdout_strategy,
              regr.vvt.hci = fixed_holdout_strategy,
              regr.tsa.hci = fixed_holdout_strategy,
              regr.ens_vvt.hci = fixed_holdout_strategy,
              regr.ens_tsa.hci = fixed_holdout_strategy,
              regr.vvt_tsa.hci = fixed_holdout_strategy
            )
            
            rnd.rsmpls.l = list(
              regr.all.rnd = makeResampleDesc("Holdout", predict="both"),
              regr.ens.rnd = makeResampleDesc("Holdout", predict="both"),
              regr.vvt.rnd = makeResampleDesc("Holdout", predict="both"),
              regr.tsa.rnd =  makeResampleDesc("Holdout", predict="both"),
              regr.ens_vvt.rnd = makeResampleDesc("Holdout", predict="both"),
              regr.ens_tsa.rnd = makeResampleDesc("Holdout", predict="both"),
              regr.vvt_tsa.rnd = makeResampleDesc("Holdout", predict="both")
            )
          
          # Conduct the benchmark experiment
            hci.bmr.l <- benchmark(learners = lrns.l, tasks = hci.regr.tasks.l, resamplings = hci.rsmpls.l, models=TRUE)
            rnd.bmr.l <- benchmark(learners = lrns.l, tasks = rnd.regr.tasks.l, resamplings = rnd.rsmpls.l, models=TRUE)

    #+ ---------------------------------
    #' ### Pameseb61 observed temperature corrections using the tsa_diff prediction models.
    #' #+ tsa_diff_correction, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
      #+ ---------------------------------
      #' #### For each learner, binding the output of the prediction models with the original data
        # function declaration
          bind_orig_corr <- function(learner.chr, predictions.l, tasks.l){
            # Extracting the prediction of the test set (there is an option to also keep the pred made on the training set)
            task_ids.l <- lapply(tasks.l,function(x) getTaskDesc(x)$id)  
            names(task_ids.l) <- sapply(task_ids.l, function(x) x)
            tasks_preds.l <- lapply((task_ids.l),
                   function(x) 
                     predictions.l[[x]][[learner.chr]][["data"]] %>%
                      dplyr::select(one_of(c("id","response"))) %>%
                      dplyr::rename(key=id)
            )
            # Joining mlr pred output id column with original dataframe used for modelisation by key column (original being the Pameseb61 one)
            # https://stackoverflow.com/questions/33177118/append-a-data-frame-to-a-list
            # https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r#32066419
            # https://stackoverflow.com/questions/26219501/mutate-multiple-columns-in-a-dataframe
            a <- Reduce(function(...) merge(..., by="key", all.x=TRUE), c(list(orig = mod.pameseb61.df), tasks_preds.l))
            colnames(a) <- c("key",  colnames(mod.pameseb61.df)[-7], names(task_ids.l) )
            # Preds diffs were computed on validation sets ==> NA values will be replace by the true diff (i.e. we only correct the diffs on the mtime obs corresponding to the validation set)
            b <- a %>%
              mutate_at(.vars = names(task_ids.l), function(x)coalesce(x, 0) )
            #+ ---------------------------------
            #' #### Calculating the corrected temperatures and removing the response columns
            c <- b %>%
              mutate_at(.vars = names(task_ids.l), .funs= funs(.+tsa) ) %>%
              dplyr::select(one_of(c("mtime", "sid", "tsa", names(task_ids.l))) )
            #+ ---------------------------------
            #' #### Structuring RMI data in the same way for later joining
            d <- no_extra_filter$records.df %>%
              dplyr::filter(sid=="1000") %>%
              dplyr::select(one_of("mtime", "sid", "tsa")) 
            d[,names(task_ids.l)] <- 0
            d <- d %>%
              mutate_at(.vars = names(task_ids.l), .funs= funs(.+tsa))
            d <- data.frame(d)
            #+ ---------------------------------
            #' #### joining RMI & Pameseb data
            e <- bind_rows(
              c,
              d
            )
            #dupliactes problem hack
            e <- unique(e)
          }
          # applying the binding orig+corr for each learner and storing in a list
          rnd.corrections.l <- lapply(list(regr.lm = "regr.lm"), bind_orig_corr, predictions.l = getBMRPredictions(rnd.bmr.l), tasks.l = rnd.regr.tasks.l)
          hci.corrections.l <- lapply(list(regr.fnn = "regr.fnn"), bind_orig_corr, predictions.l = getBMRPredictions(hci.bmr.l), tasks.l = hci.regr.tasks.l)
      #+ ---------------------------------
      #' ### Vizualizing the multiple corrected Pameseb61 tsa
        #+ ---------------------------------  
        #' #### Build Timeseries plot for each validation strategy
          make_ts_corr_plots_per_learner <- function(learner.chr, learners.corrections.l){
            make_ts_corr_plots_per_correction <- function(correction.chr, corrections.df){
              full.ts.plot <- h.render_plot(
                plot.chr="timeSerie",
                records.df = 
                  corrections.df %>% 
                    dplyr::select(
                      one_of(c("mtime","sid", correction.chr))) %>%
                      rename_("tsa" = correction.chr),
                  sensor_name.chr = "tsa"
                )
              january.ts.plot <- h.render_plot(
                plot.chr="timeSerie",
                records.df = 
                  corrections.df %>% 
                  dplyr::select(
                    one_of(c("mtime","sid", correction.chr))) %>%
                  dplyr::filter(
                    mtime %in% as.POSIXct("2016-01-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-01-16T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")) %>%
                  rename_("tsa" = correction.chr),
                sensor_name.chr = "tsa"
              )
              august.ts.plot <- h.render_plot(
                plot.chr="timeSerie",
                records.df = 
                  corrections.df %>% 
                  dplyr::select(
                    one_of(c("mtime","sid", correction.chr))) %>%
                  dplyr::filter(mtime %in% as.POSIXct("2016-08-15T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2"):as.POSIXct("2016-09-01T00:00:00", format = "%Y-%m-%dT%H:%M:%S", tz = "GMT-2")) %>%
                  rename_("tsa" = correction.chr),
                sensor_name.chr = "tsa"
              )
              return(list(
                full.ts.plot = full.ts.plot,
                august.ts.plot = august.ts.plot,
                january.ts.plot = january.ts.plot
              ))
              
            }
            corrections_names.l <- as.list(colnames(dplyr::select(learners.corrections.l[[learner.chr]], matches(paste0(strsplit(learner.chr, "\\.")[[1]][1],".")))))
            names(corrections_names.l) <- colnames(dplyr::select(learners.corrections.l[[learner.chr]], matches(paste0(strsplit(learner.chr, "\\.")[[1]][1],"."))))
            corrections.ts.corr.plots.l <- lapply(corrections_names.l, make_ts_corr_plots_per_correction, learners.corrections.l[[learner.chr]]) 
          }
          
          rnd.ts.corr.plots.l <- lapply(list(regr.lm = "regr.lm"), make_ts_corr_plots_per_learner, rnd.corrections.l)
          hci.ts.corr.plots.l <- lapply(list(regr.fnn = "regr.fnn"), make_ts_corr_plots_per_learner, hci.corrections.l)
        #+ ---------------------------------
        #' #### Build one Bland-Altman per validation srategy
          make_ba_corr_plots_per_learner <- function(learner.chr, learners.corrections.l){
            make_ba_corr_plots_per_correction <- function(correction.chr, corrections.df){
              bland_altman.plot <- h.compute_ba(
                records.wide.df= h.make_wide(
                  dplyr::select(
                    corrections.df,
                    one_of(c("mtime","sid", correction.chr))),
                  sensor_name.chr = correction.chr
                ),
                output="plot"
              )
              bland_altman.stat <- h.compute_ba(
                records.wide.df= h.make_wide(
                  dplyr::select(
                    corrections.df,
                    one_of(c("mtime","sid", correction.chr))),
                  sensor_name.chr = correction.chr
                ),
                output="table"
              )
              return(list(ba_plot = bland_altman.plot, ba_stats = bland_altman.stat))
            }
            corrections_names.l <- as.list(colnames(dplyr::select(learners.corrections.l[[learner.chr]], matches(paste0(strsplit(learner.chr, "\\.")[[1]][1],".")))))
            names(corrections_names.l) <- colnames(dplyr::select(learners.corrections.l[[learner.chr]], matches(paste0(strsplit(learner.chr, "\\.")[[1]][1],"."))))
            corrections.ba.corr.plots.l <- lapply(corrections_names.l, make_ba_corr_plots_per_correction, learners.corrections.l[[learner.chr]]) 
          }
      
          rnd.ba.corr.plots.l <- lapply(list(regr.lm = "regr.lm"), make_ba_corr_plots_per_learner, rnd.corrections.l)
          hci.ba.corr.plots.l <- lapply(list(regr.fnn = "regr.fnn"), make_ba_corr_plots_per_learner, hci.corrections.l)
 
#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  



