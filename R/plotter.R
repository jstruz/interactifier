plotter<-function(my_df,var1, var2,outcome){
  selected_df<-my_df[,c(var1,var2,outcome)]
  colnames(selected_df)<-c("x","y","z")
  x_values <- unlist(selected_df[,"x"])
  y_values <- unlist(selected_df[,"y"])
  z_values <- unlist(selected_df[,"z"])
  x_grid <- seq(from = min(x_values), to = max(x_values), length = 50)
  y_grid <- seq(from = min(y_values), to = max(y_values), length = 50)
  xy_grid <- crossing(x_grid,y_grid)
  beta_hat <- lm(z_values~x_values*y_values) %>% 
    coef()
  fitted_values <- xy_grid %>% 
    mutate(z_grid = beta_hat[1] + beta_hat[2]*x_grid + beta_hat[3]*y_grid+beta_hat[4]*x_grid*y_grid)
  z_grid <- fitted_values %>% 
    pull(z_grid) %>%
    matrix(nrow = length(x_grid)) %>%
    t() 
  ?add_surface
  # Plot using plotly -------------------------------------------------------
  ##3D scatter plot
  selected_df%>%
    plot_ly(x=~x,y=~y,z=~z,alpha=0.5,type="scatter3d") %>%
    ## Add regression plane:
    add_surface(
      x = x_grid,
      y = y_grid,
      z = z_grid
    ) %>%
    ##Add titile and labels to the graph
    layout(
      title = "3D scatterplot and regression plane",
      scene = list(
        zaxis = list(title = paste("z:",outcome)),
        yaxis = list(title = paste("y:",var2)),
        xaxis = list(title = paste("x:",var1))
      )
    )
}
