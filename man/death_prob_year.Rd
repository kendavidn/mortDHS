% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/death_prob_year.R
\name{death_prob_year}
\alias{death_prob_year}
\title{Compute Death Probabilities for a Specified Year at a Specified Age}
\usage{
death_prob_year(
  data,
  age,
  year,
  surv_stat = "mm2",
  DOB_CMC = "mm4",
  obs_time = "obs_time"
)
}
\arguments{
\item{data}{The relevant data frame}

\item{age}{The age for which the statistic is desired}

\item{year}{The year for which the statistic is desired}

\item{surv_stat}{String name for the column containing survival status. Requires that deceased is coded as 0, and alive is coded as 1. Defaults to 'mm2', the original DHS variable name.}

\item{DOB_CMC}{String name for column containing dates of birth in century month code. Defaults to 'mm4', the original DHS variable name.}

\item{obs_time}{String name for the column containing observation times, as can be generated by the mortDHS_obs_time function. Defaults to 'obs_time'}
}
\value{
A single numerical value
}
\description{
Calculates the probability of death for a user-specified year and a user-specified age.

For example, the question 'what was the probability of death at age 1 in the year 1995?' can be answered with this function.
The computation of this not straightforward, since one individual may be aged 1 for a single day in 1995, while another may be aged 1 for the entire year of 1995.
The formula used here is based on that found in Chapter 2 of Demography: Measuring and Modeling Population Processes by Preston et.al (tinyurl.com/demograsource).

Requires a data frame that contains the variables date of birth in century month code, survival status and observation time, as generated by the mortDHS_obs_time function in this package.
}
\examples{
# compute probability of death at age 0 in the year 2000
death_prob_year(malawisib, 0, 2000)

# same example as above, with each param more explicitly defined
death_prob_year(data = malawisib, age = 0, year = 2000,
DOB_CMC = 'mm4', surv_stat = 'mm2', obs_time = 'obs_time')
}
\seealso{
\code{\link[mortDHS]{death_prob_year_range}}
}
