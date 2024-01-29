# cari trend hari tanpa hujan (dengan nilai batas tertentu)

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(ggtext)

# set working directory. ctrl+shift+H

data <- read.csv("rekap_hujan_harian2.csv")
# View(data)
str(data)

data$Date <- as.Date(data[,1],format="%d-%b-%y")

# rubah data sta. cicalengka  menjadi numerik
data[,5] <- as.numeric(as.character(data[,5]))

# cek data lagi, semuanya harus numerik
str(data)

# setting batasan
threshold <- 0

# sta_cipanas_margamukti --------------------------------------------------

hari_kering_cipanas <- data %>% select(Date, Cipanas.Margamukti) %>% 
  filter(Cipanas.Margamukti > threshold) %>% 
  mutate(prev_date=lag(Date, n=1)) 

# hilangkan NA
hari_kering_cipanas <- na.omit(hari_kering_cipanas)

kemarau_sta_cipanas <- hari_kering_cipanas %>% mutate(drought_length=as.numeric(Date-prev_date)-1,
                               year=year(Date)) %>% 
  select(year,length=drought_length)

kemarau_sta_cipanas %>% 
  filter(year==2007) %>% 
  ggplot(aes(x=length))+geom_histogram()

kemarau_sta_cipanas %>%
  group_by(year) %>% 
  summarize(median = median(length),
            mean = mean(length),
            max = max(length),
            uquartile = quantile(length, prob=0.75)) %>% 
  ggplot(aes(x=year, y=mean))+
  geom_line()+
  geom_smooth(se=F) +
  labs(x="Tahun",
       y="Rata-rata Hari\nDiantara Kejadian Hujan",
       title = "Rata-rata Hari kemarau telah 
       <span style = 'color:blue'>berkurang</span>
       dalam 16 Tahun Terakhir 
       di Sta. Hujan Cipanas-Margamukti") +
  scale_x_continuous(breaks = seq(2007,2022,3))+
  theme_classic()+
  theme(plot.title.position = "panel",
        plot.title = element_textbox_simple(size=24, 
                                            margin=margin(b=10)),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 12))



# sta_ciparay -------------------------------------------------------------
hari_kering_ciparay <- data %>% select(Date, Ciparay) %>% 
  filter(Ciparay > threshold) %>% 
  mutate(prev_date=lag(Date, n=1)) 

# hilangkan NA
hari_kering_ciparay<- na.omit(hari_kering_ciparay)

kemarau_sta_ciparay <- hari_kering_ciparay %>% mutate(drought_length=as.numeric(Date-prev_date)-1,
                                                      year=year(Date)) %>% 
  select(year,length=drought_length)

kemarau_sta_ciparay %>% 
  filter(year==2007) %>% 
  ggplot(aes(x=length))+geom_histogram()

kemarau_sta_ciparay %>%
  group_by(year) %>% 
  summarize(median = median(length),
            mean = mean(length),
            max = max(length),
            uquartile = quantile(length, prob=0.75)) %>% 
  ggplot(aes(x=year, y=mean))+
  geom_line()+
  geom_smooth(se=F) +
  labs(x="Tahun",
       y="Rata-rata Hari\nDiantara Kejadian Hujan",
       title = "Rata-rata Hari kemarau telah 
       <span style = 'color:blue'>berkurang</span>
       dalam 16 Tahun Terakhir 
       di Sta. Hujan Ciparay") +
  scale_x_continuous(breaks = seq(2007,2022,3))+
  theme_classic()+
  theme(plot.title.position = "panel",
        plot.title = element_textbox_simple(size=24, 
                                            margin=margin(b=10)),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 12))




# sta_dago_bengkok --------------------------------------------------------
hari_kering_dago <- data %>% select(Date, Dago.Bengkok) %>% 
  filter(Dago.Bengkok > threshold) %>% 
  mutate(prev_date=lag(Date, n=1)) 

# hilangkan NA
hari_kering_dago<- na.omit(hari_kering_dago)

kemarau_sta_dago <- hari_kering_dago %>% mutate(drought_length=as.numeric(Date-prev_date)-1,
                                                      year=year(Date)) %>% 
  select(year,length=drought_length)

kemarau_sta_dago %>% 
  ggplot(aes(x=length))+geom_histogram()

kemarau_sta_dago%>%
  group_by(year) %>% 
  summarize(median = median(length),
            mean = mean(length),
            max = max(length)) %>% 
  ggplot(aes(x=year, y=mean))+
  geom_line()+
  geom_smooth(se=F) +
  labs(x="Tahun",
       y="Rata-rata Hari\nDiantara Kejadian Hujan",
       title = "Rata-rata Hari kemarau telah 
       <span style = 'color:blue'>berkurang</span>
       dalam 16 Tahun Terakhir 
       di Sta. Hujan Dago Bengkok") +
  scale_x_continuous(breaks = seq(2007,2022,3))+
  theme_classic()+
  theme(plot.title.position = "panel",
        plot.title = element_textbox_simple(size=24, 
                                            margin=margin(b=10)),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 12))

