% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6n1t1o2a1f1c.R
\name{basedag_data}
\alias{basedag_data}
\title{Data generator from our TODO paper.}
\source{
TODO
}
\usage{
basedag_data(
  data_N,
  z1_x_coef,
  z3_x_coef,
  x_y_coef,
  z1_y_coef,
  z2_y_coef,
  y_z4_coef,
  x_z4_coef,
  y_intercept,
  sigma_z1,
  sigma_z2,
  sigma_z3,
  sigma_z4,
  sigma_x,
  y_aux_list,
  data_family,
  data_link,
  lb = -Inf,
  ub = Inf,
  oversample = 1.3,
  seed = NULL,
  testing_data = TRUE,
  ...
)
}
\arguments{
\item{data_N}{Number of samples to simulate.}

\item{z1_x_coef}{Mean effect size of z1 on x}

\item{z3_x_coef}{Mean effect size of z3 on x}

\item{x_y_coef}{Mean effect size of x on y}

\item{z1_y_coef}{Mean effect size of z1 on y}

\item{z2_y_coef}{Mean effect size of z2 on y}

\item{y_z4_coef}{Mean effect size of y on z4}

\item{x_z4_coef}{Mean effect size of x on z4}

\item{y_intercept}{Intercept of the linear predictor term of y.}

\item{sigma_z1}{Standard deviation of z1}

\item{sigma_z2}{Standard deviation of z2}

\item{sigma_z3}{Standard deviation of z3}

\item{sigma_z4}{Standard deviation of z4}

\item{sigma_x}{Standard deviation of x}

\item{y_aux_list}{A named list that contains all auxiliary parameters for the
rng function related to the data_family likelihood.}

\item{data_family}{RNG function or string identifier for the used likelihood
family to be looked up via \link{\code{rng_lookup}}.}

\item{data_link}{Link function or string identifier for the used link
function to be looked up via \link{\code{link_lookup}}.}

\item{lb}{Lower bound to truncate y values.}

\item{ub}{Upper bound to truncate y values.}

\item{oversample}{Ratio of oversampling to prevent multiple sampling rounds
when lb or ub are used.}

\item{seed}{A random seed to make the generation deterministic.}

\item{testing_data}{True if a second dataset should be generated, eg. to be
used as testing data.}

\item{...}{Catch-all ellipses that is added to the output.}
}
\value{
A named list containing a dataset and collection of parameters and
        other outputs of interest in data_gen_output. Can also contain a
        testing_data dataset if testing_data is set to TRUE.
}
\description{
The DAG consists of a treatment x, an outcome y, a fork z1, an ancestor of
y z2, an ancestor of y z3 and a collider z4.
All variables besides y are generated from normal distributions with the
respective parameters.
}
\examples{

data <- basedag_data(
  data_N = 10,
  z1_x_coef = 1,
  z3_x_coef = 1,
  x_y_coef = 1,
  z1_y_coef = 1,
  z2_y_coef = 1,
  y_z4_coef = 1,
  x_z4_coef = 1,
  y_intercept = 1,
  sigma_z1 = 1,
  sigma_z2 = 1,
  sigma_z3 = 1,
  sigma_z4 = 1,
  sigma_x = 1,
  y_aux_list = list(sd = 2.5),
  "gaussian",
  "identity",
  oversample = 1.3,
  seed = NULL,
  testing_data = TRUE
)
hist(data$dataset$y)
head(data$dataset)

}
