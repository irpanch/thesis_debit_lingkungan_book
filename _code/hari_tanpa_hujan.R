# cari trend hari tanpa hujan (dengan nilai batas tertentu)

library(ggplot2)
library(dplyr)
library(lubridate)
library(hydroTSM)
library(plotly)
library(lattice)
require(lattice)
library(tidyverse)

data <- read.csv("rekap_hujan_harian2.csv")
View(data)
str(data)

data$Date <- as.Date(data[,1],format="%d-%b-%y")

# rubah data sta. cicalengka  menjadi numerik
data[,5] <- as.numeric(as.character(data[,5]))

# cek data lagi, semuanya harus numerik
str(data)

# setting batasan
threshold <- 0

data %>% select(Date, Cipanas.Margamukti) %>% 
  filter(Cipanas.Margamukti > threshold) %>% 
  mutate(prev_date=lag(Date, n=1)) %>% tail
  drop_na() 
