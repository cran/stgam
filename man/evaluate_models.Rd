% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate_models.R
\name{evaluate_models}
\alias{evaluate_models}
\title{Evaluates multiple models with each predictor variable specified in different ways in order to determining model form}
\usage{
evaluate_models(
  input_data,
  target_var,
  vars,
  coords_x,
  coords_y,
  VC_type = "SVC",
  time_var = NULL,
  ncores = 2
)
}
\arguments{
\item{input_data}{he data to be used used to create the GAM model in (\code{data.frame} or \code{tibble} format), containing an Intercept column to allow it be treated as an addressable term in the model.}

\item{target_var}{the name of the target variable.}

\item{vars}{a vector of the predictor variable names (without the Intercept).}

\item{coords_x}{the name of the X, Easting or Longitude variable in \code{input_data}.}

\item{coords_y}{the name of the Y, Northing or Latitude variable in \code{input_data}.}

\item{VC_type}{the type of varying coefficient model: options are "TVC" for temporally varying, "SVC" for spatially varying  and "STVC" for space-time .}

\item{time_var}{the name of the time variable if undertaking STVC model evaluations.}

\item{ncores}{the number of cores to use in parallelised approaches (default is 2 to overcome CRAN package checks). This can be determined for your computer by running parallel::detectCores()-1. Parallel approaches are only undertaken if the number of models to evaluate is greater than 30.}
}
\value{
a \code{data.frame} with indices for each predictor variable, a GCV score (\code{gcv}) for each model and the associated formula (\code{f}), which  should be passed to the \code{gam_model_rank} function.
}
\description{
Evaluates multiple models with each predictor variable specified in different ways in order to determining model form
}
\examples{
require(dplyr)
require(doParallel)
# define input data
data("hp_data")
input_data <-
  hp_data |>
  # create Intercept as an addressable term
  mutate(Intercept = 1)
# evaluate different model forms
svc_mods <-
  evaluate_models(
    input_data = input_data,
    target_var = "priceper",
    vars = c("pef"),
    coords_x = "X",
    coords_y = "Y",
    VC_type = "SVC",
    time_var = NULL,
    ncores = 2
  )
head(svc_mods)
}
