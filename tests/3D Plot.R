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