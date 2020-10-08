#install package rvest which is useful for extracting the information you need from web pages
# also install xml2 and selectr
install.packages("xml2")
install.packages("selectr")
install.packages("rvest")
#fungsi fungsi dalam rvest:
#1. read_html(url): menscrape konten HTML dari alamat url yang diberikan
#2. html_nodes(): mengidentifikasi HTML wrappers
#3. html_nodes(".class"): memanggil node berdasarkan kelas CSS
#4. html_nodes("#id"): memanggil node berdasarkan <div> id
#5. html_nodes(xpath="xpath): memanggil node berdasarkan xpath
#6. html_attrs(): mengidentifikasi atribut-atribut
#7. html_table(): mengubah tabel HTML ke dalam dataframe
#8. html_text(): menguliti tag-tag HTML dan mengekstrak teksnya saja

#menginstal package stringr yang dipakai saat melakukan data cleaning and preparation
install.packages("stringr")

#menginstal jsonlite 
#jsonlite berfungsi untuk memparsing JSON yang biasa digunakan untuk mengoptimasi website
#jsonlite digunakan untuk memapping data dalam format JSON ke format tipe data R
install.packages("jsonlite")

# Tiga tahapan melakukan web scraping
# 1. Akses halaman website dari R
# 2. Instruksi R untuk melihat isi halaman
# 3. Mengkonversi data dalam bentuk format data R

# a. Loading packages yang dibutuhkan
library(xml2)
library(rvest)
library(stringr)

# b. membaca konten HTML dari website Amazon
# url disimpan di variabel scrappedurl
scrappedurl <- 'https://www.amazon.in/dp/B07DJLVHYC/ref=sspa_dk_detail_0?psc=1&pd_rd_i=B07DJLVHYC&pd_rd_w=g4XCx&pf_rd_p=1801b34c-8af9-42b5-8961-11f124edc99b&pd_rd_wg=FzCEc&pf_rd_r=6JF61DAF76QKV7Q57QV7&pd_rd_r=bc1432a2-7bc0-4f14-94fd-e554d1732a76&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUFIWldBSjZVWkpPVEQmZW5jcnlwdGVkSWQ9QTA3NDY5MTAyVFhCSTJRTkIyNSZlbmNyeXB0ZWRBZElkPUEwMTg2ODExMTI4VUlNU0w5RTc2WiZ3aWRnZXROYW1lPXNwX2RldGFpbCZhY3Rpb249Y2xpY2tSZWRpcmVjdCZkb05vdExvZ0NsaWNrPXRydWU='
amazonWebPage <- read_html(scrappedurl)

# c. men-scrape detail produk dari Amazon
# title: judul dari produk
# price: harga produk
# description: deskripsi produk
# rating: rating produk
# size: ukuran produk
# color: warna produk
htmlTitle <- html_nodes(amazonWebPage, 'h1#title')
titleProduct <- html_text(htmlTitle)
head(titleProduct)
# membersihkan teks dari "\n"
titleProduct <- str_replace_all(titleProduct, "[\r\n]", "")
titleProduct
# harga produk
htmlPrice <- html_nodes(amazonWebPage, 'span#priceblock_dealprice')
priceProduk <- html_text(htmlPrice)
head(priceProduk)
priceProduk <- str_replace_all(priceProduk, "[\r\u20b9]", "")
priceProduk
# deskripsi produk
htmlDeskripsi <- html_nodes(amazonWebPage, 'div#productDescription')
deskripsiProduk <- html_text(htmlDeskripsi)
head(deskripsiProduk)
deskripsiProduk <- str_replace_all(deskripsiProduk, "[\r\n]", "")
deskripsiProduk
# rating produk
htmlRating <- html_nodes(amazonWebPage, 'span#acrPopover')
ratingProduk <- html_attr(htmlRating, name = 'title')
head(ratingProduk)
# deskripsi lengkap produk
htmlDeskripsi2 <- html_nodes(amazonWebPage,'div#feature-bullets')
deskripsi2Produk <- html_text(htmlDeskripsi2)
deskripsi2Produk<- str_replace_all(deskripsi2Produk, "[\r\n]", "")
deskripsi2Produk
# warna produk
htmlWarna <- html_nodes(amazonWebPage, "div#variation_color_name")
warnaProduk <- html_text(htmlWarna)
head(warnaProduk)
warnaProduk <- str_replace_all(warnaProduk, "[\r\n]", "")

# kompilasi jadi satu dataframe
produk <- data.frame(Title = titleProduct, price = priceProduk, Description = deskripsi2Produk, Rating = ratingProduk[1], Color = warnaProduk)
produk
