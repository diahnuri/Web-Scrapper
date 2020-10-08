install.packages("pbapply")
install.packages("data.table")
install.packages("urltools")
library(jsonlite)
library(rvest)
library(pbapply)
library(data.table)
library(urltools)
# URL dinamis
# "http://h1bdata.info/cities.php?term=" dimana term nya diisikan dengan huruf misal BOSTON
# kita bisa manfaatkan letters pada R yang berisi abjad a-z
letters
json.cities<-paste0('http://h1bdata.info/cities.php?term=', letters)
# json.cities berisi url dengan term yang dinamis dari a-z
json.cities
# scraping menyimpan semua hasil pencarian untuk semua kota yang berawalan dari a sampai z
allCities<-unlist(pblapply(json.cities,fromJSON))
# note: menscraping semua kota untuk semua tahun 
# akan memakan waktu scraping yang sangat lama
# untuk mengurangi waktu scraping kita bisa tambahkan parameter scraping lainnya seperti kota (city)
# contoh: menscraping data kota Boston tahun 2012 atau California 2013
# gunakan expand.grid() untuk menggabungkan beberapa parameter pada saat scraping
# misal menscrape dari tahun 2012 hingga 2016
seq(2012,2016)
cityYear<-expand.grid(city=allCities,yr=seq(2016,2019))
# ada kendala dimana kota ditulis dengan dua huruf seperti Los Angeles
# adanya spasi membuat kita harus melakukan encode "Los Angeles" menjadi "Los%Angeles"
cityYear #merupakan data frame dengan dua variabel/kolom berupa city dan year
# kita lakukan URL encode pada variabel city
cityYear$city <- urltools::url_encode(as.character(cityYear$city))
cityYear
colnames(cityYear)
#gunakan kembali fungsi paste0() untuk menggabungkan URL dan parameter city dan year
allUrls<-paste0('http://h1bdata.info/index.php?em=&job=&city=', cityYear[,1],'&year=', cityYear[,2])
allUrls
length(allUrls)
# note: dengan URL diatas akan menghabiskan waktu scraping berjam-jam
# silahkan parameternya dikustomisasi sesuai kebutuhan
# misal saya mengambil city berawalan huruf A saja untuk tahun 2015 hingga 2016
aCities<-paste0('http://h1bdata.info/cities.php?term=', letters[1])
aCities
aCities <- unlist(pblapply(json.cities,fromJSON))
aCityYear <-expand.grid(city=aCities,yr=seq(2019,2020))
aCityYear$city <- urltools::url_encode(as.character(aCityYear$city))
aCityYear
aUrls<-paste0('http://h1bdata.info/index.php?em=&job=&city=', aCityYear[,1],'&year=', aCityYear[,2])
aUrls
# total ada 100 url atau 100 halaman website yang akan discraping


# mulai menscraping halaman website
# Membuat suatu fungsi yang membaca halaman website
main<-function(urlX){
  x<- read_html(urlX)
  x<- html_table(x)
  x<- data.table(x[[1]])
  return(x)
  Sys.sleep(5)
}
aUrls[1:10]
allData <- pblapply(aUrls[1], main) #jika mau semua kota
#aData <- pblapply(aUrls, main)
#aData
allData
typeof(aData)

# menampilkan konten halaman pertama
allData[1]

# pada saat ini variabel aData merupakan list dari tabel
# satu tabel menyimpan konten satu page website
# untuk menggabungkan semua tabel (semua konten) menjadi satu tabel saja
# maka gunakan fungsi rbindlist (mirip seperti sintak do.call(rbind, aData))
aDataDF <- rbindlist(allData)
typeof(aDataDF)
head(aDataDF, 2)

# simpan ke dalam file CSV
write.csv(aDataDF, "C:/Users/sitimariyah/Documents/Workshop LIPI 14 September 2020/aData1920.csv", row.names = FALSE)

# Pembersihan data (Data Cleaning)
library(lubridate)
library(stringr)
options(scipen=999)
# mencoba membaca data yang sudah disimpan dalam bentuk csv
# kita gunakan fungsi fread() yang mirip seperti read.csv() hanya fread lebih efisin dalam membaca data yang besar
setwd()
h1bData <- fread("C:/Users/sitimariyah/Documents/Workshop LIPI 14 September 2020/aData1920.csv")
names(h1bData)
# pada dataset yang sdh dibaca, beberapa nama kolom terdapat spasi seperti kolom "BASE SALARY",
# kolom "SUBMIT DATA", "START DATE", dan "CASE STATUS"
# spasi pada nama kolom menyebabkan variabel/kolom tidak bisa dipanggil dengan tanda $
# seperti --> h1bData$EMPLOYER atau h1bData$BASE SALARY --> tidak bisa
# maka spasi diganti dengan tanda "_" dan menggunakan lowercase sehingga "BASE SALARY" menjadi "base_salary"
colnames(h1bData) <- tolower(names(h1bData))
names(h1bData)
colnames(h1bData) <- gsub(" ", "_", names(h1bData))
names(h1bData)
# menampilkan 5 data terakhir
tail(h1bData, 5)

# Data hasil scraping semua disimpan dalam bentuk teks
# meski seharusnya format datanya adalah tanggal dan nominal (dalam R dikenal istilah factor)
h1bData[case_status!="CERTIFIED"]
typeof(h1bData$case_status)
class(h1bData$case_status)
h1bData$case_status <- as.factor(h1bData$case_status)

# Nah dari data di atas terlihat bahwa seharusnya variabel case_status adalah nominal variabel
# namun hasil scraping membaca case status sebagai karakter
# cek variabel lain
typeof(h1bData$submit_date)
class(h1bData$submit_date)
typeof(h1bData$start_date)
class(h1bData$start_date)
# diketahui variabel submit date dan start date masih disimpan sebagai karakter
# seharusnya bertipe Date (tanggal)
h1bData$submit_date <- gsub("/", "-", h1bData$submit_date)
tail(h1bData$submit_date, 5)
# kita ubah ke Date
h1bData$submit_date <- mdy(h1bData$submit_date)
tail(h1bData$submit_date, 5)
typeof(h1bData$submit_date)
class(h1bData$submit_date)
# lakukan hal yang sama dengan start date
h1bData$start_date <- gsub("/", "-", h1bData$start_date)
h1bData$start_date <- mdy(h1bData$start_date)
class(h1bData$start_date)

names(h1bData)
# dengan library lubridate kita bisa meng-ekstrak tahun dengan year() atau bulan dengan month()
# pada submit date dan start date dan hasil ekstraknya dijadikan variabel baru
# membuat variabel submit year
h1bData$submit_year <- year(h1bData$submit_date)
tail(h1bData$submit_year)
class(h1bData$submit_year)
# membuat variabel start year
h1bData$start_year <- year(h1bData$start_date)
tail(h1bData$start_year)
class(h1bData$start_year)
tail(h1bData)
# mengubah variabel base salary agar bertipe numerik
head(h1bData$base_salary)
class(h1bData$base_salary)
# kita buang tanda koma
h1bData$base_salary <- gsub(",", "", h1bData$base_salary)
head(h1bData$base_salary)
# jadikan numerik
h1bData$base_salary <- as.numeric(h1bData$base_salary)
class(h1bData$base_salary)

# variabel location
head(h1bData$location)
# dari variabel location terlihat dua informasi yaitu kota dan state, dipisahkan dengan tanda koma
# kita bisa ekstrak informasi state pada variabel location dan menjadikannya variabel baru yaitu variabel state
# kita gunakan fungsi str_split_fixed untuk memisahkan dua string yang mengapit tanda koma m
state<-str_split_fixed(h1bData$location,', ', 3)
"ciracas-jakarta timur-dki JKT"
class(state)
state[0:3,1]
state[0:3,2]
h1bData$city <- state[,1]
h1bData$state <- state[,2]
head(h1bData)

# simpan clean data ke csv
write.csv(h1bData, "C:/Users/sitimariyah/Documents/Workshop LIPI 14 September 2020/aData_clean.csv", row.names = FALSE)
