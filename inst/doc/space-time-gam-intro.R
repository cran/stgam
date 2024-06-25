## ----include = FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(width = 90)
)

## ----eval = F---------------------------------------------------------------------------
#  remotes::install_github("lexcomber/stgam")

## ----warning=F, message=F---------------------------------------------------------------
# load the packages
library(stgam)
library(cols4all)   # for nice shading in graphs and maps
library(cowplot)    # for managing plots
library(dplyr)      # for data manipulation 
library(ggplot2)    # for plotting and mapping
library(glue)       # for model construction 
library(mgcv)       # for GAMs
library(sf)         # for spatial data
library(doParallel) # for parallelising operations
library(purrr)      # for model construction
library(tidyr)      # for model construction 
# load the data 
data(productivity)
data(us_data) 

## ----locationplot, message = F, warning=F, fig.height = 4, fig.width = 7, fig.cap = "The US States and the geoemtric centroids used as locations."----
ggplot() +  geom_sf(data = us_data, fill = NA) +
  geom_point(data = productivity |> filter(year == "1970"), aes(x = X, y = Y)) +
  theme_bw() + ylab("") + xlab("")

## ---------------------------------------------------------------------------------------
head(productivity)

## ---------------------------------------------------------------------------------------
# define intercept term
productivity <- productivity |> mutate(Intercept = 1)
# create the SVC
svc.gam = gam(privC ~ 0 +
                Intercept   + s(X, Y, bs = 'gp', by = Intercept) + 
                unemp + s(X, Y, bs = "gp", by = unemp) + 
                pubC  + s(X, Y, bs = "gp", by = pubC), 
              data = productivity |> filter(year == "1970"))

## ----ch2gamcheck, fig.height = 7, fig.width = 7, fig.cap = "The GAM GP SVC diagnostic plots."----
# check 
gam.check(svc.gam)
# model summary
summary(svc.gam)

## ---------------------------------------------------------------------------------------
get_b2<- productivity |> filter(year == "1970") |> mutate(Intercept = 0, unemp = 0, pubC = 1)
res <- productivity |> filter(year == "1970") |> 
  mutate(b2 = predict(svc.gam, newdata = get_b2))

## ---------------------------------------------------------------------------------------
get_b0 <- productivity |> filter(year == "1970") |> mutate(Intercept = 1, unemp = 0, pubC = 0)
res <- res |> mutate(b0 = predict(svc.gam, newdata = get_b0))
get_b1 <- productivity |> filter(year == "1970") |> mutate(Intercept = 0, unemp = 1, pubC = 0)
res <- res |> mutate(b1 = predict(svc.gam, newdata = get_b1))

## ----eval = T---------------------------------------------------------------------------
res |> select(b0, b1, b2) |>
  apply(2, summary) |> round(2)

## ---------------------------------------------------------------------------------------
terms = c("Intercept", "unemp", "pubC")
res <-  calculate_vcs(model = svc.gam, 
                      terms = terms, 
                      data = productivity |> filter(year == "1970"))
summary(res[, paste0("b_",terms)])

## ----ch2svccoefs, fig.height = 7, fig.width = 7, fig.cap = "The spatially varying coefficient (SVC) estimates."----
# join the data 
map_results <-
  us_data |> left_join(res |> select(GEOID, b_Intercept, b_unemp, b_pubC), by = "GEOID")
# plot the insignificant coefficient estimates
tit =expression(paste(""*beta[0]*""))
p1 <- 
  ggplot(data = map_results, aes(fill=b_Intercept)) + 
  geom_sf() + 
  scale_fill_continuous_c4a_div(palette="brewer.spectral",name=tit) + 
  coord_sf() +
  ggtitle("Intercept: not significant")
tit =expression(paste(""*beta[1]*""))
p2 <- 
  ggplot(data = map_results, aes(fill=b_unemp)) + 
  geom_sf() + 
  scale_fill_continuous_c4a_div(palette="brewer.spectral",name=tit) + 
  coord_sf() +
  ggtitle("Unemployment: not significant")
# plot the significant pubC coefficient estimates
tit =expression(paste(""*beta[2]*" "))
p3 <- 
  ggplot(data = map_results, aes(fill=b_pubC)) + 
  geom_sf() + 
  scale_fill_continuous_c4a_div(palette="brewer.prgn",name=tit) + 
  coord_sf() +
  ggtitle("Public captial: significant")
plot_grid(p1, p2, p3, ncol = 1)

## ---------------------------------------------------------------------------------------
# create the TVC
tvc.gam = gam(privC ~ 0 +
                Intercept   + s(year, bs = 'gp', by = Intercept) + 
                unemp + s(year, bs = "gp", by = unemp) + 
                pubC  + s(year, bs = "gp", by = pubC), 
              data = productivity)

## ----eval = F---------------------------------------------------------------------------
#  gam.check(tvc.gam)
#  summary(tvc.gam)

## ---------------------------------------------------------------------------------------
terms = c("Intercept", "unemp", "pubC")
res <-  calculate_vcs(model = tvc.gam, 
                      terms = terms, 
                      data = productivity)
summary(res[, paste0("b_",terms)])

## ----ch2tvccoefs, eval = T, echo = T, fig.height = 3, fig.width = 7, message=F, warning=F, fig.cap = "Trends in the temporally varying coefficient estimates."----
res |> 
  filter(state == "ARIZONA") |> 
  select(year, b_Intercept, b_unemp, b_pubC) |> 
  pivot_longer(-year) |>
  ggplot(aes(x = year, y = value)) +
  geom_point() + geom_line() +
  facet_wrap(~name, scale = "free") +
  theme_bw() + xlab("Year") + ylab("") 

## ---------------------------------------------------------------------------------------
stvc.gam = gam(privC ~ 0 +
                 Intercept   + s(X, Y, year, bs = 'gp', by = Intercept) + 
                 unemp + s(X, Y, year, bs = "gp", by = unemp) + 
                 pubC  + s(X, Y, year, bs = "gp", by = pubC), 
               data = productivity)

## ----eval = F---------------------------------------------------------------------------
#  gam.check(stvc.gam)
#  summary(stvc.gam)

## ---------------------------------------------------------------------------------------
terms = c("Intercept", "unemp", "pubC")
res <-  calculate_vcs(model = stvc.gam, 
                      terms = terms, 
                      data = productivity)
summary(res[, paste0("b_",terms)])

## ----eval = T---------------------------------------------------------------------------
res |> 
  select(year, b_Intercept, b_unemp, b_pubC) |>
  group_by(year) |>
  summarise(med_b0 = median(b_Intercept),
            med_b1 = median(b_unemp),
            med_b2 = median(b_pubC))

## ----ch2stvccoefsbox, echo = T, fig.height = 4, fig.width = 7, fig.cap = "The temporal variation of the Public capital coefficient estimates over 17 years.", fig.pos = 'h'----
# inputs to plot
res |> select(starts_with("b"), year) |> 
  mutate(year = "All Years") -> tmp
cols = c(c4a("tableau.red_gold", n = 17, reverse = T), "grey")
tit =expression(paste(""*beta[`Private Capital`]*""))
# plot
res |> select(starts_with("b"), year) |> 
  rbind(tmp) |> 
  mutate(year = factor(year)) |> 
  ggplot(aes(y = year, x = b_pubC, fill = year)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_fill_manual(values=cols, guide = "none") +
  theme_bw() + xlab(tit) + ylab("Time") 

## ----ch2stvccoefsmap, message = F, warning = F, fig.height = 4, fig.width = 7, fig.cap = "The spatial variation of the Unemployment coefficient estimates over time."----
tit =expression(paste(""*beta[`Public Capital`]*""))
# join the data 
map_results <-
  us_data |> left_join(res |> select(GEOID, year, b_Intercept, b_unemp, b_pubC), by = "GEOID")
# create the plot
map_results |>
  ggplot() + geom_sf(aes(fill = b_pubC), col = NA) +
	scale_fill_binned_c4a_seq(palette="scico.lajolla", name = tit) + 
  facet_wrap(~year) +
	theme_bw() + xlab("") + ylab("") + 
	theme(
	  strip.background =element_rect(fill="white"), 
	  strip.text = element_text(size = 8, margin = margin()),
	  legend.position = "inside", legend.position.inside = c(0.7, 0.1),
	  legend.direction = "horizontal",
	  legend.key.width = unit(1, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  

