##====== Script summary ===========================================
# action: Inspect Thomas' survey data
# author: Valentin Guye
# date: "09/04/2023"
# output: 
##=================================================================

# Workstation set-up ------------------------------------------------------
library(tidyverse)
library(aws.s3)
library(sf)
library(dismo)
library(units)
library(readxl)
library(xlsx)
library(stringr)
library(here)
library(DescTools)
aws.signature::use_credentials()
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

sur <- read.xlsx(here("input_data", "surveys_thomas", "Household survey questionnaire_Ghana_final version"))