## ----include = FALSE--------------------------------------------------------------------------------------------------
library(knitr)
# library(kableExtra)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(width = 120)
)

## ----eval = F, echo = F-----------------------------------------------------------------------------------------------
# # see Chaco_Data_stgam.R
# # usethis::use_data(chaco)
# # usethis::use_data(chaco, overwrite = TRUE)

## ----loadpackages, warning = F, message = F, results=F----------------------------------------------------------------
library(cols4all)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(cowplot)
# load the package and data
library(stgam)
data("chaco")

## ----eval = F, warning = F, message = F-------------------------------------------------------------------------------
# help(chaco)

## ----eval = T---------------------------------------------------------------------------------------------------------
# examine what is loaded 
chaco

## ---------------------------------------------------------------------------------------------------------------------
chaco <- 
  chaco |> 
  # scale location and retain original coordinates
  mutate(Xo = X, Yo = Y) |>
  mutate(X = X/1000, Y = Y/1000)  

## ----eval = F---------------------------------------------------------------------------------------------------------
# chaco

## ----dataplot, fig.height = 4, fig.width = 7, fig.cap = "Spatial variation of the `ndvi` variable ."------------------
# map the data layers
chaco |> 
  ggplot() + geom_sf(aes(col = ndvi)) +
  scale_color_viridis_c(option = "magma") +
  theme_bw()  +xlab("") + ylab("")

## ----databox, fig.height = 2.5, fig.width = 7, fig.cap = "Boxplots of the continous variables in the data."-----------
# boxplots
chaco |> 
  st_drop_geometry() |>
  select(id, ndvi, tmax, pr) |>
  pivot_longer(-id) |>
  ggplot(aes(x = value), fil) + 
  geom_boxplot(fill="dodgerblue") +
  facet_wrap(~name, scales = "free") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

## ----datahist, fig.height = 2.5, fig.width = 7, fig.cap = "Histograms of the continuous variables in the data."-------
# histograms
chaco |> 
  st_drop_geometry() |>
  select(id, ndvi, tmax, pr) |>
  pivot_longer(-id) |>
  ggplot(aes(x = value), fil) + 
  geom_histogram(aes(y=after_stat(density)),bins = 30, 
                 fill="tomato", col="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  facet_wrap(~name, scales = "free") +
  theme_bw()

## ----correls----------------------------------------------------------------------------------------------------------
# correlations
chaco |>
  st_drop_geometry() |>
  select(ndvi, tmax, pr) |>
  cor() |> round(3)

## ----ols--------------------------------------------------------------------------------------------------------------
# an OLS model
m_ols <- lm(ndvi ~ tmax + pr, data = chaco)
summary(m_ols)
anova(m_ols)

## ----simplot, fig.height = 4, fig.width = 7, fig.cap = "Simulated `x` and `y` data."----------------------------------
# create simulated data 
set.seed(12)
x  <- runif(500)
mu <- sin(2 * (4 * x - 2)) + 2 * exp(-(16 ^ 2) * ((x - .5) ^ 2))
y  <- rnorm(500, mu, .3)
# plot x and y
ggplot() + 
	geom_point(aes(x,y)) +
  theme_bw()

## ----simgamplot, fig.height = 4, fig.width = 7, fig.cap = "Simulated `x` and `y` data with GAM a Spline fitted."------
# a GAM illustration with a spline using s
gam_s_example <- gam(y~s(x))
# extract the smooth fit
y.s <- gam_s_example$fitted.values
# plot
ggplot() +
  geom_point(aes(x,y), col = "grey") +
	geom_line(aes(x, y = y.s), lwd = 1) +
  theme_bw()

## ----gam.1------------------------------------------------------------------------------------------------------------
# the first GAM 
gam.1 <- gam(ndvi~s(month), data = chaco)
summary(gam.1)

## ----smoothplot1, fig.height = 4, fig.width = 7, fig.cap = "A plot of the temporal smooth."---------------------------
# create a data frame with x, predicted y, standard error
x <- chaco$date
y <- gam.1$fitted.values
se <- predict(gam.1, se = TRUE, chaco)$se.fit
u <- y+se
l <- y-se
df <- data.frame(x, y, u, l)
# plot!
ggplot(df, aes(x, y, ymin = l, ymax = u)) + 
  geom_ribbon(fill = "lightblue") + 
  geom_line() + 
  theme_bw() + 
  xlab("Date") + ylab("NDVI") 

## ----smoothplot2, fig.height = 5, fig.width = 7, fig.cap = "A `mgcv` map of the spatial smooth."----------------------
# the second GAM
gam.2 <- gam(ndvi~s(X,Y), data = chaco)
summary(gam.2)
plot(gam.2, asp = 1)

## ----create_lgrid-----------------------------------------------------------------------------------------------------
# create a grid object from the Chaco data
chaco_grid <- 
  st_make_grid(chaco, square=FALSE,n=20) |> 
  st_sf()
# rename the geometry, sort row names 
st_geometry(chaco_grid) = "geometry"
rownames(chaco_grid) <- 1:nrow(chaco_grid)
# create and add coordinates X and Y / 1000
coords <- chaco_grid |> st_centroid() |> st_coordinates() 
chaco_grid <- chaco_grid |> bind_cols(coords/1000)

## ----eval = F---------------------------------------------------------------------------------------------------------
# chaco_grid
# plot(st_geometry(chaco_grid))

## ----smoothplot2a, fig.height = 5, fig.width = 7, fig.cap = "A map of the smoothed (predicted) response variable over hexagon grid."----
# predict over the grid
yhat <- predict(gam.2, newdata = chaco_grid)
chaco_grid |> 
  mutate(yhat = yhat) |>
  # and plot
  ggplot() + 
  geom_sf(aes(fill = yhat), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "NDVI") +
  # apply and modify plot theme
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"))

## ----gam.3------------------------------------------------------------------------------------------------------------
# the third GAM
gam.3 <- gam(ndvi~te(X,Y,month, d = c(2,1), bs=c('tp','cr')), data = chaco)
summary(gam.3)

## ----eval = F---------------------------------------------------------------------------------------------------------
# plot(gam.3, asp = 1)

## ----smoothplot3, fig.height = 6, fig.width = 7, fig.cap = "A plot of a spatial smooth over 7 approximately annual time periods."----
# create time slices
years <- sort(unique(chaco$year))
# calculate over the grid for each time slice
res_out <- matrix(nrow = nrow(chaco_grid), ncol = 0)
for (i in 1:length(years)){
  # convert years to months - here getting month 6
  month.val <- ((years[i]-min(years)) * 12) + 6
  res.i <- calculate_vcs(input_data = chaco_grid |> mutate(month = month.val),
                         mgcv_model = gam.3,
                         terms = NULL)
  res_out <- cbind(res_out, res.i$yhat)
}
# name with years and join to the grid
colnames(res_out) <- paste0("Y_", years)
chaco_grid |> 
  cbind(res_out) |>
  # select the variables and pivot longer
  select(-X, -Y) |>
  pivot_longer(-geometry) |>
  # make the new object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = paste0("Y_", years))) |>
  # and plot 
  ggplot() + 
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "Predicted\nNDVI") +
  # facet
  facet_wrap(~name, ncol = 4) +
  # apply and modify plot theme
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.17), 
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())

## ----gam.4------------------------------------------------------------------------------------------------------------
# the fourth GAM
gam.4 <- gam(ndvi~s(X,Y) + s(month), data = chaco)
summary(gam.4)

## ----smoothplot4, fig.height = 4, fig.width = 7, fig.cap = "A `mgcv` plot of the spatial and temporal smooths."-------
plot(gam.4, page = 1)

## ----gam.5------------------------------------------------------------------------------------------------------------
# the fifth GAM
gam.5 <- gam(ndvi~s(X,Y) + s(month) + tmax, data = chaco)
summary(gam.5)

## ----eval = F---------------------------------------------------------------------------------------------------------
# plot(gam.5, page = 1)

