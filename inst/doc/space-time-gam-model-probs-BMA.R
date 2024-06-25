## ----include = FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  options(width = 90),
  options(mc.cores=2)
)

## ----eval = F---------------------------------------------------------------------------
#  stvc.gam = gam(privC ~ 0 +
#                   Intercept   + s(X, Y, year, bs = 'gp', by = Intercept) +
#                   unemp + s(X, Y, year, bs = "gp", by = unemp) +
#                   pubC  + s(X, Y, year, bs = "gp", by = pubC),
#                 data = productivity)

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

## ----eval = T---------------------------------------------------------------------------
# define intercept term
productivity <- productivity |> mutate(Intercept = 1)
# define grid of combinations (nrow = 180)
terms_gr = expand.grid(intcp = 1:5, unemp = 1:6, pubC = 1:6) 
# examine a random slice
terms_gr |> slice_sample(n = 6)

## ---------------------------------------------------------------------------------------
# define a function to make the equations
makeform_prod <- function(intcp, unemp, pubC, bs='gp') {
  #coords <- 	c("X,Y", 	"X2,Y2")[coords]
  intx <- c("",
            glue("+s(year,bs='{bs}',by=Intercept)"), 
            glue("+s(X,Y,bs='{bs}',by=Intercept)"), 
            glue("+s(X,Y,bs='{bs}',by=Intercept) + s(year,bs='{bs}',by=Intercept)"), 
            glue("+s(X,Y,year,bs='{bs}',by=Intercept)"))[intcp]
  unempx <- c("", 
              "+ unemp",
              glue("+s(year,bs='{bs}',by=unemp)"), 	
              glue("+s(X,Y,bs='{bs}',by=unemp)"), 	
              glue("+s(X,Y,bs='{bs}',by=unemp) + s(year,bs='{bs}',by=unemp)"),
              glue("+s(X,Y,year,bs='{bs}',by=unemp)"))[unemp]
  pubCx <- c("", 
             "+ pubC",
             glue("+s(year,bs='{bs}',by=pubC)"),
             glue("+s(X,Y,bs='{bs}',by=pubC)"),
             glue("+s(X,Y,bs='{bs}',by=pubC) + s(year,bs='{bs}',by=pubC)"),
             glue("+s(X,Y,year,bs='{bs}',by=pubC)"))[pubC]
  return(formula(glue("privC~Intercept-1{unempx}{intx}{pubCx}")))
}

## ---------------------------------------------------------------------------------------
makeform_prod(intcp = 5, unemp = 2, pubC = 4, bs='gp')

## ---------------------------------------------------------------------------------------
do_gam = function(i){
  	f <- makeform_prod(intcp = terms_gr$intcp[i],
  	                 unemp = terms_gr$unemp[i],
  	                 pubC = terms_gr$pubC[i],
  	                 bs='gp')
  	m = gam(f,data=productivity)
    bic = BIC(m)
    index = data.frame(intcp = terms_gr$intcp[i],
                       unemp = terms_gr$unemp[i],
                       pubC = terms_gr$pubC[i])
    f = paste0('privC~', as.character(f)[3] )			
    return(data.frame(index, bic, f))
    #return(bic)
}

## ----eval = F---------------------------------------------------------------------------
#  terms_gr[100,]
#  do_gam(100)

## ----dogam, eval = T--------------------------------------------------------------------
t1 = Sys.time()
res_gam <- NULL 
for(i in 1:nrow(terms_gr)) {
  res.i = do_gam(i)
  res_gam = rbind(res_gam, res.i)
}
Sys.time() - t1

## ----dogam2, eval = F, warning=F, message=F---------------------------------------------
#  # set up the parallelisation
#  library(doParallel)
#  cl <- makeCluster(detectCores()-1)
#  registerDoParallel(cl)
#  # do the parallel loop
#  t1 = Sys.time()
#  res_gam <-
#    foreach(i = 1:nrow(terms_gr),
#            .combine = 'rbind',
#            .packages = c("glue", "mgcv", "purrr")) %dopar% {
#              do_gam(i)
#              }
#  Sys.time() - t1
#  # release the cores
#  stopCluster(cl)
#  # have a look
#  head(res_gam)

## ---------------------------------------------------------------------------------------
# sort the results
mod_comp <- tibble(
    res_gam) |>
    rename(BIC = bic) |>
    arrange(BIC) 
# transpose the indices to to model terms 
# rank and return the top 10 results
int_terms <- \(x) c("Fixed","s_T", "s_S", "s_T + S_S", "s_ST")[x]
var_terms <- \(x) c("---", "Fixed","s_T", "s_S", "s_T + s_S", "s_ST")[x]
mod_comp_tab <- 
  mod_comp |> 
  slice_head(n = 10) |> 
  mutate(across(unemp:pubC,var_terms)) |>
  mutate(intcp = int_terms(intcp)) |>
  rename(`Intercept` = intcp,
         `Unemployment.` = unemp,
         `Public Captial` = pubC) |>
  mutate(Rank = 1:n()) |>
  relocate(Rank) |>
  select(-f) 
# determine the relative probabilities 
# ie relative to the top ranked model
p1_vec = NULL
for(i in 2:10) {
  p1 = exp(-(mod_comp_tab$BIC[i]-mod_comp_tab$BIC[1])/2)
  p1 = p1/(1+p1)
  p1_vec = c(p1_vec, p1)
}
mod_comp_tab$`Pr(M)` = c("--", paste0(format(round(p1_vec*100, digits=1), nsmall = 1), "%"))

## ----eval = T---------------------------------------------------------------------------
mod_comp_tab

## ----evalmods---------------------------------------------------------------------------
stvc_res_gam = evaluate_models(data = productivity, 
                               target_var = "privC",
                               covariates = c("unemp", "pubC"),
                               coords_x = "X",
                               coords_y = "Y",
                               STVC = TRUE,
                               time_var = "year") 

## ----eval = F---------------------------------------------------------------------------
#  head(res_gam)
#  head(stvc_res_gam)

## ---------------------------------------------------------------------------------------
stvc_mods = gam_model_probs(stvc_res_gam, n = 10)
stvc_mods

## ----evalmods2--------------------------------------------------------------------------
svc_res_gam = evaluate_models(data = productivity |> filter(year == "1970"), 
                              target_var = "privC",
                              covariates = c("unemp", "pubC"),
                              coords_x = "X",
                              coords_y = "Y",
                              STVC = FALSE,
                              time_var = NULL) 
# head(svc_res_gam)
svc_mods = gam_model_probs(svc_res_gam, n = 10)
svc_mods

## ----eval = F---------------------------------------------------------------------------
#  productivity <- productivity |> mutate(Intercept = 1)
#  f = as.formula(svc_mods$f[1])
#  svc.gam = gam(f, data = productivity |> filter(year == "1970"))
#  summary(svc.gam)

## ----dobma------------------------------------------------------------------------------
# SVC with absolute probabilities
svc_bma <- do_bma(model_table = svc_mods, 
                  terms = c("Intercept", "unemp", "pubC"),
                  thresh = 0.1,
                  relative = FALSE, 
                  data = productivity |> filter(year == "1970"))
# STVC with relative probabilities
stvc_bma <- do_bma(model_table = stvc_mods, 
                  terms = c("Intercept", "unemp", "pubC"),
                  thresh = 0.1,
                  relative = TRUE, 
                  data = productivity)

## ----bmamap1, fig.height = 4, fig.width = 7, fig.cap = "The spatial variation of the Public captial generated using a Bayesian Model Avaergaing approach."----
# join
svc_bma_sf <-
  us_data |> select(GEOID) |>
  left_join(productivity |> 
              filter(year == "1970") |> select(GEOID, year) |> 
              cbind(svc_bma)) |>
  relocate(geometry, .after = last_col())
#  map
tit =expression(paste(""*beta[`Public Capital`]*" "))
ggplot(data = svc_bma_sf, aes(fill=pubC)) +
  geom_sf() +
  scale_fill_continuous_c4a_div(palette="brewer.blues",name=tit) +
  coord_sf() +
  theme_void()

## ----bmamap2, message = F, warning = F, fig.height = 8, fig.width = 7, fig.cap = "The spatial variation of coefficient estimatess for BMA Unemployment and Public captial over time, generated  Bayesian Model Avaergaing approach."----
# link the data
stvc_bma_sf <-
  us_data |> select(GEOID) |>
  left_join(productivity |> 
              select(GEOID, year) |> 
              cbind(stvc_bma)) |>
  relocate(geometry, .after = last_col())

# create the plots
tit =expression(paste(""*beta[`Unemployment`]*""))
p1 = stvc_bma_sf |>
  ggplot() + geom_sf(aes(fill = unemp), col = NA) +
	scale_fill_binned_c4a_seq(palette="scico.lajolla", name = tit) + 
  facet_wrap(~year) +
	theme_bw() + xlab("") + ylab("") + 
	theme(
	  strip.background =element_rect(fill="white"), 
	  strip.text = element_text(size = 8, margin = margin()),
	  legend.position = c(.7, .1), 
	  legend.direction = "horizontal",
	  legend.key.width = unit(1.15, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  
p2 = stvc_bma_sf |>
  ggplot() + geom_sf(aes(fill = pubC), col = NA) +
	scale_fill_binned_c4a_seq(palette="scico.lajolla", name = tit) + 
  facet_wrap(~year) +
	theme_bw() + xlab("") + ylab("") + 
	theme(
	  strip.background =element_rect(fill="white"), 
	  strip.text = element_text(size = 8, margin = margin()),
	  legend.position = c(.7, .1), 
	  legend.direction = "horizontal",
	  legend.key.width = unit(1.15, "cm"),
		axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())  
plot_grid(p1, p2, nrow = 2)

