# remove list history
rm(list=ls())

# set working directory : ctrl+shift+h <<<<--Penting!

###----------------------settingan awal---------------###

library(ggplot2)
library(dplyr)
library(lubridate)
library(hydroTSM)
library(plotly)
library(lattice)
require(lattice)

dbt <- "Debit"
hjn <- "Hujan"
dbt_title <- expression("Debit Harian"~(m^3/dtk))
dbt_labs <- expression("Debit"~(m^3/dtk))
hjn_title <- "Nilai Curah Hujan (mm)"
hjn_labs <- "Curah Hujan (mm)"

jenis_data <- hjn        # rubah sesuai jenis data yang ada, hujan atau debit?
judul_data <- hjn_title  # rubah sesuai jenis data yang ada, hujan atau debit?
judul_label <- hjn_labs  # rubah sesuai jenis data yang ada, hujan atau debit?

# import data
data <- read.csv("rekap_hujan_harian.csv")
str(data) # cek struktur data

# rubah format kolom satu menjadi tanggal (semula karakter)
data$Date <- as.Date(data[,1],format="%d-%b-%y")

# cek nama kolom
names(data)

# rubah data sta. rancaekek menjadi numerik
data[,4] <- as.numeric(as.character(data[,4]))

summary(data)

g <- ggplot(data,aes(x=Date, y=sta_cibeureum))+geom_line(col="blue3") + 
  labs(title=judul_data,subtitle = "Sta_cibeuruem",x = "Waktu",y=judul_label) +
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


# buat matrixplot
zoo_rr_data <- zoo(data[,2],data$Date) # cibeureum
zoo_rr_data_2 <- zoo(data[,3],data$Date) # cipaku
zoo_rr_data_3 <- zoo(data[,4],data$Date) # rancaekek
zoo_rr_data_4 <- zoo(data[,5],data$Date) # ciherang
zoo_rr_data_5 <- zoo(data[,6],data$Date) # dago_pakar


sapply(zoo_rr_data, class)

smry(zoo_rr_data)

# extract total monthly precipitation
## cibeureum
bulanan_zoo_rr_data <- daily2monthly(zoo_rr_data, FUN=sum, na.rm = T)
bulanan_rata2_zoo <- daily2monthly(zoo_rr_data, FUN=mean, na.rm = T)

## cipaku
bulanan_zoo_rr_data2 <- daily2monthly(zoo_rr_data_2, FUN=sum, na.rm = T)
bulanan_rata2_zoo2 <- daily2monthly(zoo_rr_data_2, FUN=mean, na.rm = T)

## rancaekek
bulanan_zoo_rr_data3 <- daily2monthly(zoo_rr_data_3, FUN=sum, na.rm = T)
bulanan_rata2_zoo3 <- daily2monthly(zoo_rr_data_3, FUN=mean, na.rm = T)

## ciherang
bulanan_zoo_rr_data4 <- daily2monthly(zoo_rr_data_4, FUN=sum, na.rm = T)
bulanan_rata2_zoo4 <- daily2monthly(zoo_rr_data_4, FUN=mean, na.rm = T)

## dago_pakar
bulanan_zoo_rr_data5 <- daily2monthly(zoo_rr_data_5, FUN=sum, na.rm = T)
bulanan_rata2_zoo5 <- daily2monthly(zoo_rr_data_5, FUN=mean, na.rm = T)

# buat matrix hujan bulanan
## cibeureum
mat <- matrix(bulanan_zoo_rr_data, ncol=12, byrow = T)
mat_rata2 <- matrix(bulanan_rata2_zoo, ncol=12, byrow = T)

colnames(mat) <- month.abb
rownames(mat) <- unique(format(time(bulanan_zoo_rr_data), "%Y"))

colnames(mat_rata2) <- month.abb
rownames(mat_rata2) <- unique(format(time(bulanan_rata2_zoo), "%Y"))

# matrixplot untuk jumlah curah hujan
print(matrixplot(mat, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan Rata-Rata (mm) Sta. Cibeureum")) #ganti judul kalau data hujan.

# matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Cibeureum")) #ganti judul kalau data hujan.



## cipaku
mat_2 <- matrix(bulanan_zoo_rr_data2, ncol=12, byrow = T)
mat_rata2_2 <- matrix(bulanan_rata2_zoo2, ncol=12, byrow = T)

colnames(mat_2) <- month.abb
rownames(mat_2) <- unique(format(time(bulanan_zoo_rr_data2), "%Y"))

colnames(mat_rata2_2) <- month.abb
rownames(mat_rata2_2) <- unique(format(time(bulanan_rata2_zoo2), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_2, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan Rata-Rata (mm) Sta. Cipaku")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_2, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Cipaku")) #ganti judul kalau data hujan.


## rancaekek
mat_3 <- matrix(bulanan_zoo_rr_data3, ncol=12, byrow = T)
mat_rata2_3 <- matrix(bulanan_rata2_zoo3, ncol=12, byrow = T)

colnames(mat_3) <- month.abb
rownames(mat_3) <- unique(format(time(bulanan_zoo_rr_data3), "%Y"))

colnames(mat_rata2_3) <- month.abb
rownames(mat_rata2_3) <- unique(format(time(bulanan_rata2_zoo3), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_3, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan Rata-Rata (mm) Sta. Rancaekek")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_3, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Rancaekek")) #ganti judul kalau data hujan.


## ciherang
mat_4 <- matrix(bulanan_zoo_rr_data4, ncol=12, byrow = T)
mat_rata2_4 <- matrix(bulanan_rata2_zoo4, ncol=12, byrow = T)

colnames(mat_4) <- month.abb
rownames(mat_4) <- unique(format(time(bulanan_zoo_rr_data4), "%Y"))

colnames(mat_rata2_4) <- month.abb
rownames(mat_rata2_4) <- unique(format(time(bulanan_rata2_zoo4), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_4, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan Rata-Rata (mm) Sta. Ciherang")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_4, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Ciherang")) #ganti judul kalau data hujan.

## dago_pakar
mat_5 <- matrix(bulanan_zoo_rr_data5, ncol=12, byrow = T)
mat_rata2_5 <- matrix(bulanan_rata2_zoo5, ncol=12, byrow = T)

colnames(mat_5) <- month.abb
rownames(mat_5) <- unique(format(time(bulanan_zoo_rr_data5), "%Y"))

colnames(mat_rata2_5) <- month.abb
rownames(mat_rata2_5) <- unique(format(time(bulanan_rata2_zoo5), "%Y"))

### matrixplot untuk jumlah curah hujan
print(matrixplot(mat_5, ColorRamp = "Precipitation",
                 main="Jumlah Curah Hujan Bulanan Rata-Rata (mm) Sta. Dago Pakar")) #ganti judul kalau data hujan.

### matrixplot untuk rata-rata curah hujan
print(matrixplot(mat_rata2_5, ColorRamp = "Precipitation",
                 main="Curah Hujan Bulanan Rata-Rata (mm) Sta. Dago Pakar")) #ganti judul kalau data hujan.




