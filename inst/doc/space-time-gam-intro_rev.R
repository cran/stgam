## ----include = FALSE--------------------------------------------------------------------------------------------------
library(knitr)
# library(kableExtra)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(width = 120)
)

## ----eval = F, echo = F-----------------------------------------------------------------------------------------------
# #### Data Prep
# # install.packages("stgam", dependencies = TRUE)
# # remotes::install_github("lexcomber/stgam")
# # load the package
# # library(stgam)
# library(sf)
# library(dplyr)
# library(tidyverse)
# # load the data
# setwd("~/Dropbox/Lex_GGP_GAM/stgam/")
# ## see data_prep_stgam_v1.0.0.R for data creation
# load("vignettes/hp_data.RData")
# load("vignettes/lb.RData")
# # data(productivity)
# # data(us_data)
# ## LB data
# london_boroughs <- data.frame(
#   Borough <- c(
#     "City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent",
#     "Bromley", "Camden", "Croydon", "Ealing", "Enfield", "Greenwich",
#     "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow", "Havering",
#     "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea",
#     "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham",
#     "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets",
#     "Waltham Forest", "Wandsworth", "Westminster"
#   ),
#   LAD_Code <- c(
#     "E09000001", "E09000002", "E09000003", "E09000004", "E09000005",
#     "E09000006", "E09000007", "E09000008", "E09000009", "E09000010", "E09000011",
#     "E09000012", "E09000013", "E09000014", "E09000015", "E09000016",
#     "E09000017", "E09000018", "E09000019", "E09000020",
#     "E09000021", "E09000022", "E09000023", "E09000024", "E09000025",
#     "E09000026", "E09000027", "E09000028", "E09000029", "E09000030",
#     "E09000031", "E09000032", "E09000033"
#   ),
#   stringsAsFactors = FALSE
# )
# # plot(st_geometry(lb))
# # tidying
# lb$name <- as.character(lb$NAME)
# index <- which(lb$name == "South Bucks")
# lb <- lb[-index, ]
# index <- which(lb$name == "Thurrock")
# lb <- lb[-index, ]
# index <- which(lb$name == "City of Westminster")
# lb$name[index] = "Westminster"
# # checking
# # dim(lb)
# # sort(lb$name)
# # dim(london_boroughs )
# # sort(london_boroughs$Borough)
# # head(lb)
# # joining
# lb <- lb |> inner_join(london_boroughs, by = c("name" = "Borough")) |>
#   mutate(lad = LAD_Code) |>
#   select(name, lad)
# #### final creation
# usethis::use_data(lb)
# 
# ## 2. HP data
# # get rid of observations outside lb
# hp_sf <- st_as_sf(hp_data, coords = c("X", "Y"), crs = 27700)
# int <- st_intersects(hp_sf, lb, sparse = F)
# index <- which(rowSums(int) == 0)
# hp_sf[index,]
# sort(unique(lb$lad))
# hp_data <- hp_data[-index,]
# #### final creation
# # usethis::use_data(hp_data)
# usethis::use_data(hp_data, overwrite = TRUE)

## ----loadpackages, warning = F, message = F, results=F----------------------------------------------------------------
library(cols4all)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(cowplot)
# load the package and data
library(stgam)
data("hp_data")
data("lb")

## ----eval = F, warning = F, message = F-------------------------------------------------------------------------------
# help(hp_data)
# help(lb)

## ----eval = T---------------------------------------------------------------------------------------------------------
# examine what is loaded 
hp_data

## ---------------------------------------------------------------------------------------------------------------------
hp_data <- 
  hp_data |> 
  # create continuous time variable
  mutate(days = as.numeric(dot - min(dot))/100) |>
  relocate(days, .after = dot) |>
  # scale location and retain original coordinates
  mutate(Xo = X, Yo = Y) |>
  mutate(X = X/1000, Y = Y/1000)

## ----eval = F---------------------------------------------------------------------------------------------------------
# hp_data

## ----dataplot, fig.height = 4, fig.width = 7, fig.cap = "The spatial variation of the `priceper` variable (house price per square metre in £s)."----
# map the data layers
lb |> 
  ggplot() + geom_sf() +
  geom_point(data = hp_data, aes(x = Xo, y = Yo, col = priceper)) +
  scale_color_viridis_c(option = "magma") +
  theme_bw()  +xlab("") + ylab("")

## ----message = F, warning=F-------------------------------------------------------------------------------------------
# transform to km
lb <- st_transform(lb, pipeline = "+proj=pipeline +step +proj=unitconvert +xy_out=km")
# remove the projection to avoid confusing ggplot
st_crs(lb) <- NA

## ----databox, fig.height = 5, fig.width = 8, fig.cap = "Boxplots of the numeric variables in the data."---------------
# boxplots
hp_data |> 
  select(lad, price, priceper, tfa, days, beds, cef, pef, X, Y) |>
  pivot_longer(-lad) |>
  ggplot(aes(x = value), fil) + 
  geom_boxplot(fill="dodgerblue") +
  facet_wrap(~name, scales = "free") +
  theme_bw()

## ----datahist, fig.height = 5, fig.width = 8, fig.cap = "Histograms of the numeric variables in the data."------------
# histograms
hp_data |> 
  select(lad, price, priceper, tfa, days, beds, cef, pef, X, Y) |>
  pivot_longer(-lad) |>
  ggplot(aes(x = value), fil) + 
  geom_histogram(aes(y=after_stat(density)),bins = 30, 
                 fill="tomato", col="white") +
  geom_density(alpha=.5, fill="#FF6666") +
  facet_wrap(~name, scales = "free") +
  theme_bw()

## ----correls----------------------------------------------------------------------------------------------------------
# correlations
hp_data |> 
  select(priceper, price, tfa,  beds, cef, pef) |>
  cor() |> round(3)

## ----ols--------------------------------------------------------------------------------------------------------------
# an OLS model
m_ols <- lm(priceper ~ cef + pef + beds, data = hp_data)
summary(m_ols)
anova(m_ols)

## ----eval = F---------------------------------------------------------------------------------------------------------
# # Dummy with LADs
# m_dummy1 <- lm(priceper ~ tfa:lad, data = hp_data)
# summary(m_dummy1)
# anova(m_dummy1)

## ----eval = F---------------------------------------------------------------------------------------------------------
# # Dummy with Time
# m_dummy2 <- lm(priceper ~ tfa:days, data = hp_data)
# summary(m_dummy2)
# anova(m_dummy2)

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
gam.1 <- gam(priceper~s(days), data = hp_data)
summary(gam.1)

## ----smoothplot1, fig.height = 4, fig.width = 7, fig.cap = "A plot of the temporal smooth."---------------------------
# create a data frame with x, predicted y, standard error
x <- hp_data$dot
y <- gam.1$fitted.values
se <- predict(gam.1, se = TRUE, hp_data)$se.fit
u <- y+se
l <- y-se
df <- data.frame(x, y, u, l)
# plot!
ggplot(df, aes(x, y, ymin = l, ymax = u)) + 
  geom_ribbon(fill = "lightblue") + 
  geom_line() + 
  theme_bw() + 
  xlab("Date") + ylab("priceper") 

## ----smoothplot2, fig.height = 5, fig.width = 7, fig.cap = "A `mgcv` plot of the spatial smooth."---------------------
# the second GAM
gam.2 <- gam(priceper~s(X,Y), data = hp_data)
summary(gam.2)
plot(gam.2, asp = 1)

## ----create_lgrid-----------------------------------------------------------------------------------------------------
# 1. create a grid object the study area from the LB  data
l_grid <- 
  st_make_grid(lb, square=FALSE,n=50) |> 
  st_sf() |> 
  st_join(lb) |> 
  filter(!is.na(name))
# rename the geometry, sort row names 
st_geometry(l_grid) = "geometry"
rownames(l_grid) <- 1:nrow(l_grid)
# create and add coordinates X and Y
coords <- l_grid |> st_centroid() |> st_coordinates() 
l_grid <- l_grid |> bind_cols(coords) 

## ----eval = F---------------------------------------------------------------------------------------------------------
# l_grid
# plot(st_geometry(l_grid))

## ----smoothplot2a, fig.height = 5, fig.width = 7, fig.cap = "A map of the smoothed (predicted) response variable over hexagon grid."----
# 2. predict over the grid
yhat <- predict(gam.2, newdata = l_grid)
l_grid |> mutate(yhat = yhat) |>
  # 4.and plot
  ggplot() + 
  geom_sf(aes(fill = yhat), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "priceper") +
  # add context
  geom_sf(data = lb, fill = NA) +
  # apply and modify plot theme
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1, "cm"))

## ----gam.3------------------------------------------------------------------------------------------------------------
# the third GAM
gam.3 <- gam(priceper~te(X,Y,days, d = c(2,1), bs=c('tp','cr')), data = hp_data)
summary(gam.3)

## ----eval = F---------------------------------------------------------------------------------------------------------
# plot(gam.3, asp = 1)

## ----smoothplot3, fig.height = 6, fig.width = 7, fig.cap = "A plot of a spatial smooth over 7 approximately annual time periods."----
# 1. create time intervals (see the creation of days variable above)
pred_days = seq(365, 2555, 365)/100
# 2. create coefficient estimates for each time period (n = 7) 
res_out <- NULL
for (i in pred_days){
  res.i <- calculate_vcs(input_data = l_grid |> mutate(days = i),
                         mgcv_model = gam.3,
                         terms = NULL)
  res_out <- cbind(res_out, res.i$yhat)
}
# 3. name with years and join to the grid
colnames(res_out) <- paste0("Y_", 2018:2024)

l_grid |> cbind(res_out) |>
  # select the variables and pivot longer
  select(-name, -lad, -X, -Y) |>
  pivot_longer(-geometry) |>
  # make the new days object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = paste0("Y_", 2018:2024))) |>
  
  # 4. and plot 
  ggplot() + 
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_seq("brewer.yl_or_rd", name = "Predicted \n'priceper'") +
  # facet
  facet_wrap(~name, ncol = 3) +
  # apply and modify plot theme
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    legend.position.inside = c(0.7, 0.15), 
	  legend.text=element_text(size=10),
	  legend.title=element_text(size=12),
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
	  legend.key.width = unit(1.5, "cm"),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())   

## ----gam.4------------------------------------------------------------------------------------------------------------
# the fourth GAM
gam.4 <- gam(priceper~s(X,Y) + s(days), data = hp_data)
summary(gam.4)

## ----smoothplot4, fig.height = 4, fig.width = 7, fig.cap = "A `mgcv` plot of the spatial and temporal smooths."-------
plot(gam.4, page = 1)

## ----gam.5------------------------------------------------------------------------------------------------------------
# the fifth GAM
gam.5 <- gam(priceper~s(X,Y) + s(days) + pef, data = hp_data)
summary(gam.5)

## ----eval = F---------------------------------------------------------------------------------------------------------
# plot(gam.5, page = 1)

## ----eval = F---------------------------------------------------------------------------------------------------------
# gam.5  <- gam(priceper~s(X,Y) + s(days) + pef, data = hp_data)
# summary(gam.5)

## ----gam.5.new--------------------------------------------------------------------------------------------------------
gam.5.new <- gam(priceper ~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) + pef,
                data = hp_data |> mutate(Intercept = 1))
summary(gam.5.new)

## ----data_prep--------------------------------------------------------------------------------------------------------
hp_data <- 
  hp_data |> 
  mutate(Intercept = 1)

## ----gam.t------------------------------------------------------------------------------------------------------------
gam.t <- gam(priceper~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) + 
              pef + s(days, by = pef), 
            data = hp_data)
summary(gam.t)

## ----vcs.t------------------------------------------------------------------------------------------------------------
vcs <- calculate_vcs(input_data = hp_data, mgcv_model = gam.t, terms = c("Intercept", "pef"))
head(vcs)

## ----smoothplot.t, fig.height = 4, fig.width = 7, fig.cap = "A plot of the temporal smooth for `pef`."----------------
vcs |> 
  mutate(u = b_pef + se_pef, 
         l = b_pef - se_pef) |>
  ggplot(aes(x = dot, y = b_pef, ymin = l, ymax = u)) + 
  geom_ribbon(fill = "lightblue") + 
  geom_line() + 
  theme_bw() + 
  xlab("Date") + ylab("pef") 

## ----gam.s------------------------------------------------------------------------------------------------------------
gam.s <- gam(priceper~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) + 
              pef + s(X,Y, by = pef), 
            data = hp_data)
summary(gam.s)

## ----smoothplot.s, fig.height = 4, fig.width = 7, fig.cap = "Maps of the spatial smooth for `pef` over the original observations and the hexagonal grid."----
# 1.over observation locations
vcs <- calculate_vcs(input_data = hp_data, 
                     mgcv_model = gam.s, 
                     terms = c("Intercept", "pef"))
tit <-expression(paste(""*beta[`pef`]*"")) 
p1 <- 
  ggplot() + geom_sf(data = lb, col = "lightgrey") +
  geom_point(data = vcs, aes(x = X, y = Y, colour = b_pef), alpha = 1) + 
  scale_colour_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(1, "cm"),) + 
  xlab("") + ylab("")
# 2. over grid - recall it needs an intercept term and a days value!
vcs <- calculate_vcs(input_data = l_grid |> mutate(Intercept = 1, days = mean(hp_data$days)), 
                     mgcv_model = gam.s, 
                     terms = c("Intercept", "pef"))
p2 <- 
  ggplot() + 
  geom_sf(data = vcs, aes(fill = b_pef), col = NA) + 
  scale_fill_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  theme_bw()+
  theme(legend.position = "bottom", 
        legend.key.width = unit(1, "cm"),) + 
  xlab("") + ylab("")
plot_grid(p1, p2)

## ----gam.st1----------------------------------------------------------------------------------------------------------
gam.st1 <- gam(priceper~0 + Intercept + s(X,Y,by=Intercept) + s(days, by=Intercept) +  
                pef + te(X,Y,days,d = c(2,1),bs=c('tp','cr'), by = pef), 
              data = hp_data)
summary(gam.st1)

## ----smoothplot.st1, warning=F, message=F, fig.height = 5, fig.width = 9, fig.cap = "Summaries of coefficient estimates from the the space-time smooths for `pef`."----
# calculate the varying coefficient estimates
vcs <- calculate_vcs(input_data = hp_data, mgcv_model = gam.st1, terms = c("Intercept", "pef"))
# temporal trends
p_time <- 
  vcs |> 
  select(dot, b_Intercept, b_pef) |> 
  pivot_longer(-dot) |>
  mutate(name = recode(name, 
                       "b_Intercept" = '""*beta[Intercept]',
                       "b_pef" = '""*beta[pef]')) |>
  ggplot(aes(x = dot, y = value)) +
  geom_point(alpha = 0.1) + 
  geom_smooth() + 
  facet_wrap(~name,  labeller = label_parsed, scale = "free", ncol = 1) +
  theme_bw() + xlab("Year") + ylab("") 
# spatial trends
tit <-expression(paste(""*beta[`Intercept`]*"")) 
p_sp1 <- 
  ggplot() + geom_sf(data = lb, col = "lightgrey") +
  geom_point(data = vcs, aes(x = X, y = Y, colour = b_Intercept), alpha = 1) + 
  scale_colour_continuous_c4a_seq("brewer.yl_gn_bu", name = tit) +
  theme_bw() + 
  xlab("") + ylab("")
tit <-expression(paste(""*beta[`pef`]*"")) 
p_sp2 <- 
  ggplot() + geom_sf(data = lb, col = "lightgrey") +
  geom_point(data = vcs, aes(x = X, y = Y, colour = b_pef), alpha = 1) + 
  scale_colour_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  theme_bw() + 
  xlab("") + ylab("")

plot_grid(p_time, plot_grid(p_sp1, p_sp2, ncol = 1), nrow = 1, rel_widths = c(3.5,6))

## ----smoothplot.st1.2, fig.height = 6, fig.width = 7, fig.cap = "The changes over time of the spatial distrubtuion of the `pef` coefficient estimate."----
# 1. create time intervals (as above)
pred_days = seq(365, 2555, 365)/100
# 2. create coefficient estimates for each time period (n = 7) 
res_out <- matrix(nrow = nrow(l_grid), ncol = 0)
for (i in pred_days){
  res.i <- calculate_vcs(input_data = l_grid |> mutate(days = i),
                         mgcv_model = gam.st1,
                         terms = c("Intercept", "pef"))
  # select just the coefficient estimates of interest
  res.i <- res.i |> st_drop_geometry() |> select(starts_with("b_pef")) 
  res_out <- cbind(res_out, res.i)
}

# 3. name with years and join to the grid
colnames(res_out) <- paste0("Y", "_", 2018:2024)
# define a title
tit <-expression(paste(""*beta[`pef`]*"")) 
l_grid |> cbind(res_out) |>
  # select the variables and pivot longer
  select(starts_with("Y_")) |> 
  # rename
  rename(`2018` = "Y_2018", `2019` = "Y_2019", `2020` = "Y_2020",
         `2021` = "Y_2021", `2022` = "Y_2022", `2023` = "Y_2023", `2024` = "Y_2024") |>
  pivot_longer(-geometry) |>
  # make the new days object a factor (to enforce plotting order)
  mutate(name = factor(name, 2018:2024)) |>
   # 4. and plot 
  ggplot() + 
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_div(name = tit) +
  # facet
  facet_wrap(~name, ncol = 3) +
  # apply and modify plot theme
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    legend.position.inside = c(0.7, 0.15), 
	  legend.text=element_text(size=10),
	  legend.title=element_text(size=12),
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
	  legend.key.width = unit(1.5, "cm"),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())  

## ----gam.st2----------------------------------------------------------------------------------------------------------
gam.st2 <- gam(priceper~0 + Intercept + 
                 s(X,Y,by=Intercept) + s(days, by=Intercept) + 
                 pef + s(X,Y, by = pef) + s(days, by = pef), 
               data = hp_data)
summary(gam.st2)

## ----smoothplot.st2, fig.height = 8, fig.width = 7, fig.cap = "`mgcv` plots of the seperate space and time smooths for the `pef` predictor variable."----
plot(gam.st2, pages = 1)

## ---------------------------------------------------------------------------------------------------------------------
df <- data.frame(Model = c("Time", "Space", "Space-Time I", "Space-Time II"),
                 GCV = c(gam.t$gcv.ubre, gam.s$gcv.ubre, gam.st1$gcv.ubre, gam.st2$gcv.ubre))
# rank the models 
df |> arrange(GCV)

## ---------------------------------------------------------------------------------------------------------------------
head(hp_data)

## ----eval_stvc, eval = F, cache = T, warning=F, message=F-------------------------------------------------------------
# library(doParallel)
# t1 <- Sys.time()
# stvc_mods <- evaluate_models(
#   input_data = hp_data,
#   target_var = "priceper",
#   vars = c("pef", "beds"),
#   coords_x = "X",
#   coords_y = "Y",
#   VC_type = "STVC",
#   time_var = "days",
#   ncores = 2)
# Sys.time() - t1  # about 10 minutes (less with more cores!)

## ----echo = F, eval = T-----------------------------------------------------------------------------------------------
# precomputed to get through CRAN checks
# save(stvc_mods, file = "stvc_mods.RData")
load("stvc_mods.RData")

## ---------------------------------------------------------------------------------------------------------------------
mod_comp <- gam_model_rank(stvc_mods, n= 10)
# have a look
mod_comp |> select(-f) 

## ---------------------------------------------------------------------------------------------------------------------
f <- as.formula(mod_comp$f[1])
f

## ----final_mod, cache = T---------------------------------------------------------------------------------------------
# specify the model
gam.m <- gam(f, data = hp_data, method = "REML")
# check k
k.check(gam.m)

## ---------------------------------------------------------------------------------------------------------------------
summary(gam.m)

## ---------------------------------------------------------------------------------------------------------------------
vcs <- calculate_vcs(input_data = hp_data, 
                    mgcv_model = gam.m, 
                    terms = c("Intercept", "pef", "beds"))

## ----warning=F, message=FALSE-----------------------------------------------------------------------------------------
vcs |> select(starts_with("b_")) |>
  apply(2, summary) |> round(1)

## ----final_time, fig.height = 3, fig.width = 8, fig.cap = "The changes over time of the coefficient estimates of the final model."----
vcs |> 
  select(dot, starts_with("b_")) |> 
  rename(`Intercept` = b_Intercept,
         `Potential Energy Efficiency` = b_pef,
         `Bedrooms` = b_beds) |>
  pivot_longer(-dot) |>
  mutate(name = factor(name, 
                       levels=c("Intercept","Potential Energy Efficiency", "Bedrooms"))) |>
  group_by(dot, name) |>
  summarise(
    lower = quantile(value, 0.25),
    median = median(value),
    upper = quantile(value, 0.75)
  ) |>
  ggplot(aes(x = dot, y = median)) +
  geom_point(col = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~name, scale = "free_y") +
  theme_bw() + xlab("") + ylab("") +
  theme(strip.background = element_rect(fill="white"))


## ----svccoefs, fig.height = 6, fig.width = 7, fig.cap = "The varying `pef` (Potential Energy Efficiency) coefficient estimates over space and time."----
# make spatial data  
vcs_sf <- 
  vcs |> 
  st_as_sf(coords = c("X", "Y"), remove = F) 
# plot 
ggplot()  + 
  geom_sf(data = lb) +
  geom_sf(data = vcs_sf, aes(col = b_pef)) + 
  scale_colour_continuous_c4a_div(palette="brewer.rd_yl_bu", 
                                  name = "Potential\nEnergy Efficiency") + 
  facet_wrap(~yot) + 
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    legend.position.inside = c(0.7, 0.15), 
	  legend.text=element_text(size=10),
	  legend.title=element_text(size=12),
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
	  legend.key.width = unit(1.5, "cm"),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())  

## ----svccoefs2, cache = T,  fig.height = 6, fig.width = 7, fig.cap = "The varying `pef` (Potential energy efficiency rating) coefficient estimates over time and the grid surface."----
# create time slices
years <- 2018:2024
# calculate over the grid for each time slice
res_out <- matrix(nrow = nrow(l_grid), ncol = 0)
for (i in 1:length(years)){
  # convert years to days
  day.val = (years[i]-2018) * 365 / 100
  res.i <- calculate_vcs(input_data = l_grid |> mutate(days = day.val),
                         mgcv_model = gam.m,
                         terms = c("Intercept", "pef", "beds"))
  # select all the coefficient estimates 
  res.i <- 
    res.i |> 
    st_drop_geometry() |> 
    select(starts_with("b_"), 
           starts_with("se_")) 
  # rename them
  names(res.i) <- paste0(names(res.i), "_", years[i])
  # bind to the result
  res_out <- cbind(res_out, res.i)
  cat(years[i], "\t")
}
# title
tit <-expression(paste(""*beta[`beds`]*"")) 
# join to the grid
l_grid |> cbind(res_out) |>
  # select the variables and pivot longer
  select(starts_with("b_pef")) |> 
  # rename
  rename(`2018` = "b_pef_2018", `2019` = "b_pef_2019", 
         `2020` = "b_pef_2020", `2021` = "b_pef_2021", 
         `2022` = "b_pef_2022", `2023` = "b_pef_2023", 
         `2024` = "b_pef_2024") |>  
  pivot_longer(-geometry) |>
  # make the new days object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = 2018:2024)) |>
   # 4. and plot 
  ggplot() + 
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
  # facet
  facet_wrap(~name, ncol = 3) +
  # apply and modify plot theme
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.direction = "horizontal",
    legend.position.inside = c(0.7, 0.15), 
	  legend.text=element_text(size=10),
	  legend.title=element_text(size=12),
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
	  legend.key.width = unit(1.5, "cm"),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())  

## ----eval = F---------------------------------------------------------------------------------------------------------
# gam_m <- gam(y~s(X,Y, by = x, k = 40), data = input_data)

