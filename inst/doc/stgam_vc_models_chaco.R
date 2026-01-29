## ----include = FALSE--------------------------------------------------------------------------------------------------
library(knitr)
library(kableExtra)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 6, fig.width = 7,
  options(width = 120)
)
cache.val = TRUE

## ----loadpackages, warning = F, message = F, results=F----------------------------------------------------------------
library(cols4all)
library(cowplot)
library(dplyr)
library(ggplot2)
library(gratia)
library(sf)
library(tidyr)
library(mgcv)
# load the package and data
library(stgam)
data("chaco")
chaco <- 
  chaco |> 
  # scale location and retain original coordinates
  mutate(Xo = X, Yo = Y) |>
  mutate(X = X/1000, Y = Y/1000)  

## ---------------------------------------------------------------------------------------------------------------------
head(chaco)

## ----echo = F, eval = T-----------------------------------------------------------------------------------------------
load("model_summaries.RData")

## ----eval = T---------------------------------------------------------------------------------------------------------
m  <- gam(ndvi~s(X,Y) + s(month) + pr, data = chaco)
summary(m)

## ----new--------------------------------------------------------------------------------------------------------------
m <- gam(ndvi ~ 
           0 + Intercept + s(X,Y,by=Intercept) + s(month, by=Intercept) + 
           pr,
         data = chaco |> mutate(Intercept = 1))
summary(m)

## ----data_prep--------------------------------------------------------------------------------------------------------
chaco <- 
  chaco |> 
  mutate(Intercept = 1)

## ----m1, eval = F, echo = T-------------------------------------------------------------------------------------------
# # initial model
# m1 <- gam(
#   ndvi ~ 0 +
#     Intercept + te(X,Y,month, by=Intercept,bs = c("tp", "cr"), d = c(2,1)) +
#     tmax + te(X,Y,month, by = tmax, bs = c("tp", "cr"), d = c(2,1)) +
#     pr + te(X,Y,month, by = pr, bs = c("tp", "cr"), d = c(2,1)),
#   data = chaco,
#   method = "REML",
#   family = gaussian()
# )

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# appraise(m1, method = "simulate")

## ----appm1, echo = F, eval = T, warning = F, message = F, out.width = "100%", fig.cap = "Diagnostic plots of `m1`."----
library(cowplot)
img1 <- ggdraw() + draw_image("appraise_plot_1.png")
img2 <- ggdraw() + draw_image("appraise_plot_2.png") 
img3 <- ggdraw() + draw_image("appraise_plot_3.png")
img4 <- ggdraw() + draw_image("appraise_plot_4.png")
plot_grid(img1, img2, img3, img4, ncol = 2)

## ----echo = F, eval = T-----------------------------------------------------------------------------------------------
tab <- data.frame(
  Family = c("Gaussian", "Gamma", "Inverse Gaussian", "Poisson", "Binomial"),
  Response = c("continuous, symmetric",
               "continuous, positive, skewed",
               "continuous, positive, heavy-tailed",
               "count data",
               "0–1 or proportion data"),
  Link = c("identity", "log", "log", "log", "logit"),
  Example = c("residuals roughly normal",
              "costs, durations, prices",
              "reaction times, dispersions",
              "integer counts",
              "success/failure outcomes")
)

kable(tab,  
       booktabs = T, row.names = F, linesep = "", 
      caption = paste0("\\label{tab:tab1}Common families and their properties."))  |>
  kable_styling(latex_options = "hold_position") 

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# summary(m1)

## ----echo = F, eval = T-----------------------------------------------------------------------------------------------
cat(sum_m1, sep = "\n")

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# # extract the terms
# f1 <- predict(m1, type = "terms")
# # generate numeric summaries
# apply(f1, 2, function(v) c(sd = sd(v), range = diff(range(v)))) |> round(3)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
effect_m1

## ----m2, eval = F, echo = T-------------------------------------------------------------------------------------------
# # second model with separate space-time effects for pr
# m2 <- gam(
#   ndvi ~ 0 +
#     Intercept + te(X,Y,month, by=Intercept, bs = c("tp", "cr"), d = c(2,1)) +
#     tmax + te(X,Y,month, by = tmax, bs = c("tp", "cr"), d = c(2,1)) +
#     pr + s(X,Y, by = pr) + s(month, by = pr),
#   data = chaco,
#   method = "REML",
#   family = gaussian()
# )

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# effect_size(m2)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
effect_m2

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# gam.check(m2)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
tab <- data.frame(
  `Smooth type` = c(
    "1D temporal smooth (e.g., days, years)",
    "2D spatial smooth (e.g., te(X, Y))",
    "3D space–time smooth (e.g., te(X, Y, time))",
    "Factor-by smooths (e.g., by = region)"
  ),
  `Heuristic` = c(
    "`k` ≈ min(n/10, 20–40)",
    "`k` ≈ 50–200 total basis functions",
    "`k` = product of marginal bases, e.g. `k = c(50, 20)`",
    "`k` ≈ same as main smooth per level"
  ),
  Notes = c(
    "Typically start around 10 basis functions; if long time series (> 1000 points), may go to 50–100",
    "Typically start around 50 basis functions; choose higher if data cover large/complex region",
    "mgcv handles tensor product decomposition; but do not let any single dimension be too small",
    "You can use fewer if levels are many and data per level small"
  ),
  check.names = FALSE
)

kable(tab,  
       booktabs = T, row.names = F, linesep = "", 
      caption = paste0("\\label{tab:tab2}Heuristics for choosing `k` in common smooth types."))  |>
  kable_styling(latex_options = "hold_position") 


## ----m3, eval = F, echo = T-------------------------------------------------------------------------------------------
# # third model with increased k for the pr temporal smooth
# m3 <- gam(
#   ndvi ~ 0 +
#     Intercept + te(X,Y,month, by=Intercept, bs = c("tp", "cr"), d = c(2,1)) +
#     tmax + te(X,Y,month, by = tmax, bs = c("tp", "cr"), d = c(2,1)) +
#     pr + s(X,Y, by = pr) + s(month, k=20, by = pr),
#   data = chaco,
#   method = "REML",
#   family = gaussian()
# )

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# # check the model smooths
# k.check(m3)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
k_check_m3

## ----m4, eval = F, echo = T-------------------------------------------------------------------------------------------
# # fourth model with increased k for the pr temporal smooth
# m4 <- gam(
#   ndvi ~ 0 +
#     Intercept + te(X,Y,month, by=Intercept, bs = c("tp", "cr"), d = c(2,1)) +
#     tmax + te(X,Y,month, by = tmax, bs = c("tp", "cr"), d = c(2,1)) +
#     pr + s(X,Y, by = pr) + s(month, k=40, by = pr),
#   data = chaco,
#   method = "REML",
#   family = gaussian()
# )
# # check the model smooths
# k.check(m4)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
k_check_m4

## ----m5, eval = F, echo = T-------------------------------------------------------------------------------------------
# # fifth model with increased k for the pr temporal smooth
# m5 <- gam(
#   ndvi ~ 0 +
#     Intercept + te(X,Y,month, by=Intercept, bs = c("tp", "cr"), d = c(2,1)) +
#     tmax + te(X,Y,month, by = tmax, bs = c("tp", "cr"), d = c(2,1)) +
#     pr + s(X,Y, by = pr) + s(month, k=80, by = pr),
#   data = chaco,
#   method = "REML",
#   family = gaussian()
# )
# # check the model smooths
# k.check(m5)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
k_check_m5

## ----m6, eval = F, echo = T-------------------------------------------------------------------------------------------
# # sixth model with increased k for the pr temporal smooth
# m6 <- gam(
#   ndvi ~ 0 +
#     Intercept + te(X,Y,month, by=Intercept, bs = c("tp", "cr"), d = c(2,1)) +
#     tmax + te(X,Y,month, by = tmax, bs = c("tp", "cr"), d = c(2,1)) +
#     pr + s(X,Y, by = pr) + s(month, k=120, by = pr),
#   data = chaco,
#   method = "REML",
#   family = gaussian()
# )
# # check the model smooths
# k.check(m6)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
k_check_m6

## ----eval = F---------------------------------------------------------------------------------------------------------
# appraise(m6, method = "simulate")

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# effect_size(m6)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
effect_m6

## ----echo = T, eval = F-----------------------------------------------------------------------------------------------
# summary(m6)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
cat(sum_m6, sep = "\n")

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# # original model
# effect_size(m1)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
effect_m1

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# # tuned model
# effect_size(m6)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
effect_m6

## ----eval = F, echo = T-----------------------------------------------------------------------------------------------
# # AIC comparison
# m1$aic; m6$aic

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
aic_m1; aic_m6 

## ----vcs, eval = F, echo = T------------------------------------------------------------------------------------------
# vcs <- calculate_vcs(input_data = chaco,
#                      mgcv_model = m6,
#                      terms = c("Intercept", "tmax", "pr"))

## ----warning=F, message=FALSE-----------------------------------------------------------------------------------------
vcs |> 
  # drop the geometry if the object is spatial
  st_drop_geometry() |>
  select(starts_with("b_")) |>
  apply(2, summary) |> 
  round(4)

## ----final_time, fig.height = 3, fig.width = 7, fig.cap = "The variations over time of the final model coefficient estimates."----
vcs |> 
  st_drop_geometry() |>
  select(date, starts_with("b_")) |> 
  rename(`Intercept` = b_Intercept,
         `Max Temperature` = b_tmax,
         `Precipitation` = b_pr) |>
  pivot_longer(-date) |>
  group_by(date, name) |>
  summarise(
    lower = quantile(value, 0.25),
    median = median(value),
    upper = quantile(value, 0.75)
  ) |>
  ggplot(aes(x = date, y = median)) +
  geom_point(col = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~name, scale = "free_y") +
  theme_bw() + xlab("") + ylab("") +
  theme(strip.background = element_rect(fill="white"))


## ----svccoefs, fig.height = 6, fig.width = 7, fig.cap = "The varying `tmax` (Maximum temperature) coefficient estimates over space and time."----
# title
tit <-expression(paste(""*beta[`tmax`]*"")) 
# plot 
ggplot()  + 
  geom_sf(data = vcs, aes(col = b_tmax)) + 
  scale_colour_continuous_c4a_div(palette="brewer.rd_yl_bu",name = tit) + 
  facet_wrap(~year) + 
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.17), 
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())  

## ----create_lgrid, warning=FALSE, message=FALSE-----------------------------------------------------------------------
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
# add dummy variables to the grid 
chaco_grid <- 
  chaco_grid |> 
  mutate(Intercept =  NA_real_, 
         tmax = NA_real_,
         pr = NA_real_)

## ----eval = F---------------------------------------------------------------------------------------------------------
# chaco_grid
# plot(st_geometry(chaco_grid))

## ----svccoefs2, eval = F, echo = T------------------------------------------------------------------------------------
# # create time slices
# years <- sort(unique(vcs$year))
# # calculate over the grid for each time slice
# res_out <- matrix(nrow = nrow(chaco_grid), ncol = 0)
# for (i in 1:length(years)){
#   # convert years to months - here getting month 6
#   month.val <- ((years[i]-min(years)) * 12) + 6
#   res.i <- calculate_vcs(input_data = chaco_grid |> mutate(month = month.val),
#                          mgcv_model = m6,
#                          terms = c("Intercept", "tmax", "pr"))
#   # select all the coefficient estimates
#   res.i <-
#     res.i |>
#     st_drop_geometry() |>
#     select(starts_with("b_"),
#            starts_with("se_"),
#            starts_with("t_"))
#   # rename coefficients estimates
#   names(res.i) <- paste0(names(res.i), "_", years[i])
#   # bind to the result
#   res_out <- cbind(res_out, res.i)
#   cat(years[i], "\t")
# }

## ----svccoefs3, cache = cache.val,  fig.height = 6, fig.width = 7, fig.cap = "The varying `tmax` (Maximum temperature) coefficient estimates over space and time and the grid surface."----
# join to the grid
chaco_grid |> 
  cbind(res_out) |>
  # select the variables and pivot longer
  select(starts_with("b_tmax")) |> 
  # rename and select with transmute
  transmute(`2012` = b_tmax_2012, `2013` = b_tmax_2013,
            `2014` = b_tmax_2014, `2015` = b_tmax_2015,
            `2016` = b_tmax_2016, `2017` = b_tmax_2017,
            `2018` = b_tmax_2018, `2019` = b_tmax_2019,
            `2020` = b_tmax_2020, `2021` = b_tmax_2021,
            `2022` = b_tmax_2022
            ) |>  
  pivot_longer(-geometry) |>
  # make the new time object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = years)) |>
  # 4. and plot 
  ggplot() + 
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
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

## ----echo = F, eval = F-----------------------------------------------------------------------------------------------
# # save summaries for CRAN vignette
# p_m1 <- appraise(m1, method = "simulate")
# 
# sum_m1 <- summary(m1)
# effect_m1 <- apply(f1, 2, function(v) c(sd = sd(v), range = diff(range(v)))) |> round(3)
# 
# effect_m2 <- effect_size(m2)
# 
# # check the model smooths
# k_check_m3 <- k.check(m3)
# 
# k_check_m4 <- k.check(m4)
# 
# k_check_m5 <- k.check(m5)
# k_check_m6 <- k.check(m6)
# 
# aic_m1 <- m1$aic
# aic_m2 <- m2$aic
# aic_m3 <- m3$aic
# aic_m4 <- m4$aic
# aic_m5 <- m5$aic
# aic_m6 <- m6$aic
# 
# setwd("~/Library/CloudStorage/Dropbox/Lex_GGP_GAM/stgam/vignettes")
# save(p_m1, sum_m1, effect_m1, effect_m2, k_check_m3,
#      k_check_m4, k_check_m5, k_check_m6, sum_m6,
#      aic_m1, aic_m2, aic_m3, aic_m4, aic_m5, aic_m6,
#      vcs, years, res_out,
#      file = "model_summaries.RData")

## ----echo = T, eval = F-----------------------------------------------------------------------------------------------
# data.frame(Model = paste0("m", c(1:6)),
#            AIC = c(m1$aic, m2$aic, m3$aic, m4$aic, m5$aic, m6$aic)) |>
#   # rank the models
#   arrange(AIC)

## ----eval = T, echo = F-----------------------------------------------------------------------------------------------
data.frame(Model = paste0("m", c(1:6)),
           AIC = c(aic_m1, aic_m2, aic_m3, aic_m4, aic_m5, aic_m6)) |> 
  # rank the models 
  arrange(AIC)

## ---------------------------------------------------------------------------------------------------------------------
chaco |> st_drop_geometry() |> head()

## ----eval_stvc, eval = F, cache = cache.val, warning=F, message=F-----------------------------------------------------
# library(doParallel)
# # ncores <- detectCores()-1
# t1 <- Sys.time()
# stvc_mods <- evaluate_models(
#   input_data = chaco |> st_drop_geometry(),
#   target_var = "ndvi",
#   family = "gaussian()",
#   vars = c("tmax", "pr"),
#   coords_x = "X",
#   coords_y = "Y",
#   VC_type = "STVC",
#   time_var = "month",
#   k_set = FALSE,
#   spatial_k = NULL,
#   temporal_k = NULL,
#   k_increase = TRUE,
#   k2edf_ratio = 1.5,
#   k_multiplier = 2,
#   max_iter = 10,
#   ncores = 15
# )
# Sys.time() - t1  # about 29 minutes on M4 Mac with 64GB of RAM

## ----echo = F, eval = T-----------------------------------------------------------------------------------------------
# precomputed to get through CRAN checks
# save(stvc_mods, file = "stvc_mods.RData")
load("stvc_mods.RData")

## ---------------------------------------------------------------------------------------------------------------------
mod_comp <- gam_model_rank(stvc_mods, n = 10)
# have a look
mod_comp |> select(-c(f, ks)) 

## ---------------------------------------------------------------------------------------------------------------------
f <- as.formula(mod_comp$f[1])
f

## ----final_mod, cache = cache.val-------------------------------------------------------------------------------------
# specify the model
gam.m <- gam(f, data = chaco, method = "REML")
# check k
k.check(gam.m)

## ---------------------------------------------------------------------------------------------------------------------
summary(gam.m)

## ----final_time2, eval = T, echo = T, message = F, warning=F, fig.height = 3, fig.width = 7---------------------------
# extract the VCs
vcs <- calculate_vcs(input_data = chaco,
                     mgcv_model = gam.m, 
                     terms = c("Intercept", "tmax", "pr"))
# examine
vcs |> 
  # drop the geometry if the object is spatial
  st_drop_geometry() |>
  select(starts_with("b_")) |>
  apply(2, summary) |> 
  round(5)

## ----plot_final_time2, eval = T, echo = T, message = F, warning=F, fig.height = 3, fig.width = 7, fig.cap = "The variations over time of the model coefficient estimates."----

# plot over time
vcs |> 
  st_drop_geometry() |>
  select(date, starts_with("b_")) |> 
  rename(`Intercept` = b_Intercept,
         `Max Temperature` = b_tmax,
         `Precipitation` = b_pr) |>
  pivot_longer(-date) |>
  group_by(date, name) |>
  summarise(
    lower = quantile(value, 0.25),
    median = median(value),
    upper = quantile(value, 0.75)
  ) |>
  ggplot(aes(x = date, y = median)) +
  geom_point(col = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~name, scale = "free_y") +
  theme_bw() + xlab("") + ylab("") +
  theme(strip.background = element_rect(fill="white"))


## ----svccoefs4, fig.height = 6, fig.width = 7, fig.cap = "The varying `tmax` (Maximum temperature) coefficient estimates over space and time."----
# title
tit <-expression(paste(""*beta[`tmax`]*"")) 
# plot 
ggplot()  + 
  geom_sf(data = vcs, aes(col = b_tmax)) + 
  scale_colour_continuous_c4a_div(palette="brewer.rd_yl_bu",name = tit) + 
  facet_wrap(~year) + 
  theme_bw() + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.17), 
	  strip.background = element_rect(fill="white", colour = "white"), 
	  strip.text = element_text(size = 8, margin = margin(b=4)),
		axis.title=element_blank(),
		axis.text=element_blank(),
    axis.ticks=element_blank())  

## ----svccoefs5, eval = T, echo = T, cache = cache.val, fig.cap = "The varying `tmax` (Maximum temperature) coefficient estimates over space and time and the grid surface."----
# create time slices
years <- sort(unique(vcs$year))
# calculate over the grid for each time slice
res_out <- matrix(nrow = nrow(chaco_grid), ncol = 0)
for (i in 1:length(years)){
  # convert years to months - here getting month 6
  month.val <- ((years[i]-min(years)) * 12) + 6
  res.i <- calculate_vcs(input_data = chaco_grid |> mutate(month = month.val),
                         mgcv_model = gam.m,
                         terms = c("Intercept", "tmax", "pr"))
  # select all the coefficient estimates 
  res.i <- 
    res.i |> 
    st_drop_geometry() |> 
    select(starts_with("b_"), 
           starts_with("se_"),
           starts_with("t_")) 
  # rename coefficients estimates
  names(res.i) <- paste0(names(res.i), "_", years[i])
  # bind to the result
  res_out <- cbind(res_out, res.i)
  # cat(years[i], "\t")
}
# join to the grid
chaco_grid |> 
  cbind(res_out) |>
  # select the variables and pivot longer
  select(starts_with("b_tmax")) |> 
  # rename and select with transmute
  transmute(`2012` = b_tmax_2012, `2013` = b_tmax_2013,
            `2014` = b_tmax_2014, `2015` = b_tmax_2015,
            `2016` = b_tmax_2016, `2017` = b_tmax_2017,
            `2018` = b_tmax_2018, `2019` = b_tmax_2019,
            `2020` = b_tmax_2020, `2021` = b_tmax_2021,
            `2022` = b_tmax_2022
            ) |>  
  pivot_longer(-geometry) |>
  # make the new time object a factor (to enforce plotting order)
  mutate(name = factor(name, levels = years)) |>
  # 4. and plot 
  ggplot() + 
  geom_sf(aes(fill = value), col = NA) +
  # adjust default shading
  scale_fill_continuous_c4a_div("brewer.rd_yl_bu", name = tit) +
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

## ----echo = F, eval = F-----------------------------------------------------------------------------------------------
# # save summaries for CRAN vignette
# # 1. save PNGs to local directory
# library(patchwork)
# p_m1 <- appraise(m1, method = "simulate")
# plots <- patchwork:::get_patches(p_m1)$plots
# for (i in seq_along(plots)) {
#   ggsave(
#     filename = paste0("appraise_plot_", i, ".png"),
#     plot = plots[[i]],
#     width = 6,
#     height = 4
#   )
# }
# # 2. other stuff
# # sum_m1 <- summary(m1)
# sum_m1 <- capture.output(summary(m1))
# # cat(sum_m1, sep = "\n")
# 
# effect_m1 <- apply(f1, 2, function(v) c(sd = sd(v), range = diff(range(v)))) |> round(3)
# 
# effect_m2 <- effect_size(m2)
# 
# # check the model smooths
# k_check_m3 <- k.check(m3)
# 
# k_check_m4 <- k.check(m4)
# 
# k_check_m5 <- k.check(m5)
# 
# k_check_m6 <- k.check(m6)
# effect_m6 <- effect_size(m6)
# 
# # sum_m6 <- summary(m6)
# sum_m6 <- capture.output(summary(m6))
# # cat(sum_m6, sep = "\n")
# 
# aic_m1 <- m1$aic
# aic_m2 <- m2$aic
# aic_m3 <- m3$aic
# aic_m4 <- m4$aic
# aic_m5 <- m5$aic
# aic_m6 <- m6$aic
# 
# setwd("~/Library/CloudStorage/Dropbox/Lex_GGP_GAM/stgam/vignettes")
# save(sum_m1, effect_m1, effect_m2, k_check_m3,
#      k_check_m4, k_check_m5, k_check_m6, effect_m6, sum_m6,
#      aic_m1, aic_m2, aic_m3, aic_m4, aic_m5, aic_m6,
#      vcs, years, res_out,
#      file = "model_summaries.RData",
#      compress = "xz")

