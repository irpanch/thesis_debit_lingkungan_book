# remove list history
rm(list=ls())

# load library ------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(hydroTSM)
library(plotly)
library(lattice)
require(lattice)


# setting label -----------------------------------------------------------

dbt <- "Debit"
hjn <- "Hujan"
dbt_title <- expression("Debit Harian"~(m^3/dtk))
dbt_labs <- expression("Debit"~(m^3/dtk))
hjn_title <- "Nilai Curah Hujan (mm)"
hjn_labs <- "Curah Hujan (mm)"

jenis_data <- hjn        # rubah sesuai jenis data yang ada, hujan atau debit?
judul_data <- hjn_title  # rubah sesuai jenis data yang ada, hujan atau debit?
judul_label <- hjn_labs  # rubah sesuai jenis data yang ada, hujan atau debit?


# import and checking data  -----------------------------------------------

# import data
data <- read.csv("_data/rekap_hujan_harian2.csv")
str(data) # cek struktur data

# rubah format kolom satu menjadi tanggal (semula karakter)
data$Date <- as.Date(data[,1],format="%d-%b-%y")

# cek nama kolom
names(data)

# rubah data sta. cicalengka  menjadi numerik
data[,5] <- as.numeric(as.character(data[,5]))

summary(data)

g <- ggplot(data,aes(x=Date, y=Cipanas.Margamukti))+geom_line(col="blue3") + 
  labs(title=judul_data,subtitle = "Sta_Cipanas-Margamukti",x = "Waktu",y=judul_label) +
  theme_update(plot.title=element_text(hjust=0.5))+
  theme_update(plot.subtitle=element_text(hjust=0.5))+
  theme_update(axis.title.y=element_text(angle=90))
g 

index_max <- data[,2] == max(data[,2])
index_min <- data[,2] == min(data[,2])

# buat grafik time series data dengan ggplot
library(lubridate)

## tambah kolom hari, tahun, bulan
data$DOY <- as.numeric(yday(data$Dates))
data$YEAR <- as.numeric(format(data$Dates,"%Y"))
data$MONTH <- as.numeric(format(data$Dates,"%m"))


# set matrixplot ----------------------------------------------------------
# set matrixplot
zoo_rr_data <- zoo(data[,2],data$Date) # cipanas
zoo_rr_data_2 <- zoo(data[,3],data$Date) # dago
zoo_rr_data_3 <- zoo(data[,4],data$Date) # paseh
zoo_rr_data_4 <- zoo(data[,5],data$Date) # cicalengka
zoo_rr_data_5 <- zoo(data[,6],data$Date) # ciparay
zoo_rr_data_6 <- zoo(data[,7],data$Date) # ujungberung
zoo_rr_data_7 <- zoo(data[,8],data$Date) # cisondari


sapply(zoo_rr_data, class)

smry(zoo_rr_data)



# extract jumlah hujan bulanan ----------------------------------------------------
# extract total monthly precipitation
## cipanas
bulanan_zoo_rr_data <- daily2monthly(zoo_rr_data, FUN=sum, na.rm = T)
bulanan_rata2_zoo <- daily2monthly(zoo_rr_data, FUN=mean, na.rm = T)

## dago
bulanan_zoo_rr_data2 <- daily2monthly(zoo_rr_data_2, FUN=sum, na.rm = T)
bulanan_rata2_zoo2 <- daily2monthly(zoo_rr_data_2, FUN=mean, na.rm = T)

## paseh
bulanan_zoo_rr_data3 <- daily2monthly(zoo_rr_data_3, FUN=sum, na.rm = T)
bulanan_rata2_zoo3 <- daily2monthly(zoo_rr_data_3, FUN=mean, na.rm = T)

## cicalengka
bulanan_zoo_rr_data4 <- daily2monthly(zoo_rr_data_4, FUN=sum, na.rm = T)
bulanan_rata2_zoo4 <- daily2monthly(zoo_rr_data_4, FUN=mean, na.rm = T)

## ciparay
bulanan_zoo_rr_data5 <- daily2monthly(zoo_rr_data_5, FUN=sum, na.rm = T)
bulanan_rata2_zoo5 <- daily2monthly(zoo_rr_data_5, FUN=mean, na.rm = T)

## ujungberung
bulanan_zoo_rr_data6 <- daily2monthly(zoo_rr_data_6, FUN=sum, na.rm = T)
bulanan_rata2_zoo6 <- daily2monthly(zoo_rr_data_6, FUN=mean, na.rm = T)

## cisondari
bulanan_zoo_rr_data7 <- daily2monthly(zoo_rr_data_7, FUN=sum, na.rm = T)
bulanan_rata2_zoo7 <- daily2monthly(zoo_rr_data_7, FUN=mean, na.rm = T)

# buat matrix hujan bulanan


# matrixplot cipanas -----------------------------------------------------------------
mat <- matrix(bulanan_zoo_rr_data, ncol=12, byrow = T)
mat_rata2 <- matrix(bulanan_rata2_zoo, ncol=12, byrow = T)
colnames(mat) <- month.abb
rownames(mat) <- unique(format(time(bulanan_zoo_rr_data), "%Y"))

colnames(mat_rata2) <- month.abb
rownames(mat_rata2) <- unique(format(time(bulanan_rata2_zoo), "%Y"))

# matrixplot untuk jumlah curah hujan
print(matrixplot(mat, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Cipanas")) #ganti judul kalau data hujan.

# matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Cipanas")) #ganti judul kalau data hujan.

write.csv(mat,"_output/jumlah_hujan_bulanan_cipanas.csv")
write.csv(mat_rata2,"_output/rata2_hujan_bulanan_cipanas.csv")

# matrixplot dago --------------------------------------------------------------------

mat_2 <- matrix(bulanan_zoo_rr_data2, ncol=12, byrow = T)
mat_rata2_2 <- matrix(bulanan_rata2_zoo2, ncol=12, byrow = T)

colnames(mat_2) <- month.abb
rownames(mat_2) <- unique(format(time(bulanan_zoo_rr_data2), "%Y"))

colnames(mat_rata2_2) <- month.abb
rownames(mat_rata2_2) <- unique(format(time(bulanan_rata2_zoo2), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_2, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Dago Bengkok")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_2, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Dago Bengkok")) #ganti judul kalau data hujan.

write.csv(mat_2,"_output/jumlah_hujan_bulanan_dago.csv")
write.csv(mat_rata2_2,"_output/rata2_hujan_bulanan_dago.csv")

# matrixplot paseh -------------------------------------------------------------------

mat_3 <- matrix(bulanan_zoo_rr_data3, ncol=12, byrow = T)
mat_rata2_3 <- matrix(bulanan_rata2_zoo3, ncol=12, byrow = T)

colnames(mat_3) <- month.abb
rownames(mat_3) <- unique(format(time(bulanan_zoo_rr_data3), "%Y"))

colnames(mat_rata2_3) <- month.abb
rownames(mat_rata2_3) <- unique(format(time(bulanan_rata2_zoo3), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_3, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Paseh")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_3, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Paseh")) #ganti judul kalau data hujan.

write.csv(mat_3,"_output/jumlah_hujan_bulanan_paseh.csv")
write.csv(mat_rata2_3,"_output/rata2_hujan_bulanan_paseh.csv")

# matrixplot cicalengka --------------------------------------------------------------

mat_4 <- matrix(bulanan_zoo_rr_data4, ncol=12, byrow = T)
mat_rata2_4 <- matrix(bulanan_rata2_zoo4, ncol=12, byrow = T)

colnames(mat_4) <- month.abb
rownames(mat_4) <- unique(format(time(bulanan_zoo_rr_data4), "%Y"))

colnames(mat_rata2_4) <- month.abb
rownames(mat_rata2_4) <- unique(format(time(bulanan_rata2_zoo4), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_4, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Cicalengka")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_4, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Cicalengka")) #ganti judul kalau data hujan.

write.csv(mat_4,"_output/jumlah_hujan_bulanan_cicalengka.csv")
write.csv(mat_rata2_4,"_output/rata2_hujan_bulanan_cicalengka.csv")


# matrixplot ciparay -----------------------------------------------------------------

mat_5 <- matrix(bulanan_zoo_rr_data5, ncol=12, byrow = T)
mat_rata2_5 <- matrix(bulanan_rata2_zoo5, ncol=12, byrow = T)

colnames(mat_5) <- month.abb
rownames(mat_5) <- unique(format(time(bulanan_zoo_rr_data5), "%Y"))

colnames(mat_rata2_5) <- month.abb
rownames(mat_rata2_5) <- unique(format(time(bulanan_rata2_zoo5), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_5, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Ciparay")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_5, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Ciparay")) #ganti judul kalau data hujan.


write.csv(mat_5,"_output/jumlah_hujan_bulanan_ciparay.csv")
write.csv(mat_rata2_5,"_output/rata2_hujan_bulanan_ciparay.csv")

# matrixplot ujung_berung -----------------------------------------------------------------

mat_6 <- matrix(bulanan_zoo_rr_data6, ncol=12, byrow = T)
mat_rata2_6 <- matrix(bulanan_rata2_zoo6, ncol=12, byrow = T)

colnames(mat_6) <- month.abb
rownames(mat_6) <- unique(format(time(bulanan_zoo_rr_data6), "%Y"))

colnames(mat_rata2_6) <- month.abb
rownames(mat_rata2_6) <- unique(format(time(bulanan_rata2_zoo6), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_6, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan Sta. Ujung Berung")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_6, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Ujung Berung")) #ganti judul kalau data hujan.

write.csv(mat_6,"_output/jumlah_hujan_bulanan_ujung_berung.csv")
write.csv(mat_rata2_6,"_output/rata2_hujan_bulanan_ujung_berung.csv")

# matrixplot cisondari -----------------------------------------------------------------

mat_7 <- matrix(bulanan_zoo_rr_data7, ncol=12, byrow = T)
mat_rata2_7 <- matrix(bulanan_rata2_zoo7, ncol=12, byrow = T)

colnames(mat_7) <- month.abb
rownames(mat_7) <- unique(format(time(bulanan_zoo_rr_data7), "%Y"))

colnames(mat_rata2_7) <- month.abb
rownames(mat_rata2_7) <- unique(format(time(bulanan_rata2_zoo7), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_7, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan (mm) Sta. Cisondari")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_7, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Cisondari")) #ganti judul kalau data hujan.

write.csv(mat_7,"_output/jumlah_hujan_bulanan_cisondari.csv")
write.csv(mat_rata2_7,"_output/rata2_hujan_bulanan_cisondari.csv")

