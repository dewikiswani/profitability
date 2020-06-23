library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)
library(shinyWidgets)
library(dplyr)
library(openxlsx)


indonesia <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/prov sampai desa.csv",
                      stringsAsFactors = F)
komoditas <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/komoditas.csv",
                      stringsAsFactors = F)
peneliti <- read.csv("C:/dw/ICRAF/project R/theme 2/profitability/data/lusita 2.0/peneliti.csv",
                     stringsAsFactors = F)