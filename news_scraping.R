install.packages("newsanchor")
install.packages("robotstxt")
install.packages("httr")
install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidytext")
install.packages("textdata")
install.packages("stopwords")
install.packages("tm")


library(newsanchor) # download artikel berita 
library(robotstxt)  # mengambil file robots.txt (file robot.txt menjelaskan halaman2 apa saja yang boleh di donlot)
library(httr)       # http requests
library(rvest)      # package untuk menscraping website
library(dplyr)      # package untuk memanipulasi data
library(stringr)    # package untuk memanipulasi karakter
library(tidytext)   # tidy text analysis
library(textdata)   # memiliki the AFINN lexicon
library(stopwords)

??newsanchor
??rvest

# cara menarik headline berita dari website BBC News (bisa diganti dengan website lain)
# API key bisa diperoleh dengan registrasi ke https://newsapi.org/
# Misalkan disini kita mendownload headline berita dengan keyword Trump dari BBC News
# Mulai 10 September 2020 hingga 16 September 2020
# Mengenai waktu dan berapa banyak yang bisa didownload tergantung akun kita apakah akun biasa atau akun berbayar
response <- get_everything_all(query   = "Trump",
                                sources = "bbc-news",
                                from    = "2020-09-10",
                                to      = "2020-09-16",
                                api_key = "10e2c37740c445249f77d26bc415a3d6")
# Mengambil berita dengan keyword Indonesia
response2 <- get_everything_all(query   = "Indonesia",
                               sources = "bbc-news",
                               from    = "2020-09-10",
                               to      = "2020-09-16",
                               api_key = "10e2c37740c445249f77d26bc415a3d6")
class(response)               # Balikan dari API adalah list yang berisi dua list
length(response)

length(response$metadata)     # isi list pertama berupa metadata
length(response$results_df)   # isi list kedua berupa results_df
colnames(response$metadata)   # menampilkan atribut/variabel dari metadata
colnames(response$results_df) # menampilkan atribut/variabel dari results_df
head(response$results_df)

articles_set1 <- response$results_df  # simpan di kumpulan artikel pertama
articles_set2 <- response2$results_df # simpan di kumpulan artikel kedua
dim(articles_set1)                    # mengetahui dimensi dataframe kumpulan artikel pertama
dim(articles_set2)                    # mengetahui dimensi dataframe kumpulan artikel kedua
articles_all <- rbind(articles_set1, articles_set2) # jadikan satu dataframe dengan menggabungkan baris
dim(articles_all)
head(articles_all) # menampilkan 6 baris teratas

allowed <- paths_allowed(articles_all$url)  # untuk mengecek apakah pada website BBC News kita diizinkan untuk menscrape halaman URL berita
all(allowed) #jika return TRUE berarti kita diizinkan untuk melakukan scraping langsung ke URL nya

# fungsi untuk mengambil berita dari halaman url yang discrape
# dengan cara menentukan tag mana yang harus discrape
get_article_body <- function (url) {
  
  # mendownload halaman
  response <- GET(url)
  
  # mengecek apakah akses ke URL berhasil atau tidak. Jika berhasil akan mendapat balikan status 200
  if (response$status_code != 200) return(NA)
  
  # mengekstrak html
  html <- content(x        = response,
                  type     = "text",
                  encoding = "UTF-8")
  
  # memparsing html
  parsed_html <- read_html(html)
  
  # mendefinisikan paragraph DOM selector
  # untuk menentukan artikel berita diletakkan pada tag apa
  
  # selector <- "article#story div.StoryBodyCompanionColumn div p" 
  # selector di atas untuk website The New York Times hanya saja service ke website The New York Times tidak tersedia
  #selector <- "div.story-body__inner"     #tiap website mempunyai style masing-masing dan selector ini perlu disesuaikan
  selector <- "div.story-body__inner p"
  
  # parse content
  parsed_html %>%
    html_nodes(selector) %>%      # mengekstrak semua paragraphs dalam div story-body__inner
    html_text() %>%               # extract content of the <p> tags
    str_replace_all("\n", "") %>% # replace all line breaks
    str_replace_all('\"','')%>%
    paste(collapse = " ")         # join all paragraphs into one string
}

# lakukan test dengan satu URL 
test<- get_article_body("https://www.nytimes.com/2020/09/15/us/oregon-fires-california.html?action=click&module=Spotlight&pgtype=Homepage")
test<- get_article_body("https://www.bbc.com/news/world-us-canada-54171941")
test2 <- get_article_body(articles_all$url[3])

articles_all$url[3]

nrow(articles_all)

# membuat kolom text baru
articles_all$body <- NA

# inisialisasi progress bar untuk iterasi scraping ke URL langsung
pb <- txtProgressBar(min     = 1,
                     max     = nrow(articles_all),
                     initial = 1,
                     style   = 3)

# loop ke dalam semua artikel di dataframe articles_all dan terapkan fungsi "apply" 
for (i in 1:nrow(articles_all)) {
  
  # "apply" function to i url
  articles_all$body[i] <- get_article_body(articles_all$url[i])
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
  # sleep for 1 sec
  Sys.sleep(1)
}

#menampilkan hasil scraping
articles_all$body[3]

# mengelola content berita 
      # melakukan summary: rata-rata skor sentimen dan jumlah berita per tanggal


# mengelola content headline berita 
sentiment_from_headline <- articles_all %>%
  select(url, content) %>%                               # mengambil kolom yang dibutuhkan
  unnest_tokens(word, content) %>%                       # split setiap artikel menjadi kumpulan kata-kata
  anti_join(get_stopwords(), by = "word") %>%            # membuang stopwords
  inner_join(get_sentiments("afinn"), by = "word") %>%   # mendapatkan nilai sentimen dari fungsi "afinn"
  group_by(url) %>%                                      # mengelompokkan text berdasarkan URL
  summarise(sentiment = sum(value)) %>%                  # menjumlahkan skor sentimen
  left_join(articles_all, by = "url") %>%                # menambahkan kolom sentimen ke dataframe
  select(published_at, sentiment) %>%                    # mengambil kolom yang dibutuhkan
  group_by(date = as.Date(published_at)) %>%             # mengelompokkan berdasarkan tanggal
  summarise(sentiment = mean(sentiment), n = n())        # melakukan summary: rata-rata skor sentimen dan jumlah berita per tanggal


sentiment_by_day
sentiment_from_headline

library(tm)
articles_all$description
#content_text <- gsub("r?\n|\r", "", articles_all$description)
content_corpus <- Corpus(VectorSource(articles_all$description))
content_corpus
strwrap(content_corpus[[42]])
# lowercase
content_corpus <- tm_map(content_corpus, tolower)
# membuang tanda baca
content_corpus <- tm_map(content_corpus, removePunctuation)
# membuang space berlebih
content_corpus <- tm_map(content_corpus, stripWhitespace)
# membuang stopwords
content_corpus <- tm_map(content_corpus, removeWords, stopwords('english'))

strwrap(content_corpus[[1]])

# membuat Term Document Matrix
tdm <- TermDocumentMatrix(content_corpus)
inspect(tdm[1:20,])
inspect(tdm)

library(wordcloud)
matriks_kata <- as.matrix(tdm)
sorted_matriks_kata <- sort(rowSums(matriks_kata), decreasing = TRUE)
d <- data.frame(word = names(sorted_matriks_kata), freq = sorted_matriks_kata)
head(d, 3)
#membuat wordcloud
set.seed(12)
windows()
wordcloud(words=d$word, freq = d$freq, min.freq = 5,
          max.words = 50, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
