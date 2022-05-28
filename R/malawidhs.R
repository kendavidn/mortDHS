#' @title Malawi DHS Individual Recode Files from Four Surveys
#'
#' @description Contains data from 77,349 respondents from the Malawi 2004 through 2015 DHS surveys.
#'
#' The dataset contains the following variables, needed to compute mortality statistics:
#'
#' caseid: Unique identifier for each respondent
#'
#' v008: Date of interview in century month code
#'
#' The following sibling variables, with 20 subvariables (mm1_01, mm1_02...mm1_20) for each of a maximum of 20 possible siblings per respondent:
#'
#' mm1: Sex of sibling (1 = male, 2 = female)
#'
#' mm2: Survival status of sibling  (0 = deceased, 1 = alive )
#'
#' mm4: Sibling's date of birth in century month code
#'
#' mm8: Sibling's date of death in century month code
#'
#' @name malawidhs
#' @docType data
#' @usage malawidhs
#'
#' @format dataframe
NULL

