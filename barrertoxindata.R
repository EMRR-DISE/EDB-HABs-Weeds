#Organize data for the barrier HABs monitoring

library(tidyverse)
library(lubridate)
library(readxl)

HABreports = read_excel("data/BarrierHABreport.xlsx")
nutrients = read_csv("data/WQDataReport.csv")
str(nutrients)
nutrients =
