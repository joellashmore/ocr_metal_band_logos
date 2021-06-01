# Project to try and build a classifier that takes logos for metal bands, which are traditionally
# littered with semi-random distributions of serifs, fractals, and converts them to readable text
# This project was inspired by a contest that is hosted on metalsucks.com called "Completely Unreadable Band Logo of the Week"

# Script to build an AI model that can recognize metal band's names from their usually indescipherable logos.
library(tidyverse)
library(rvest)
library(magick)
library(magrittr)
library(polite)

# We're going to get 100 pages worth of images. Usually there are about 8 images to a page, so 800 images for the initial scrape.
#Using CSS selectors to scrape the rankings section

webpage <- 'https://www.angrymetalguy.com/category/reviews/page/2'

##
#Testing node targeting process
link.titles <- read_html(webpage) %>%
  html_nodes(".c_img") %>%
  html_nodes("img") %>%
  html_attr("src")

link.titles

##



#Specifying the base url for desired website to be scraped
url <-rep(('https://www.angrymetalguy.com/category/reviews/page/'),
      200) #repeat the base url 100 times for joining with the desired number

#Specifying which pages to be scraped
pagenum <-
  seq(2, 200, by = 1) # web page index starts at 0 and goes to 2

ToBeScraped <-
  paste0(url, pagenum) #combine the two (without spaces) to make an iterateable list of urls


# One of the most important but most skipped parts of web scraping is confirming existing rules for
# a particular website, and figuring out what the desired delay between requests is
# We do this with the "bow" function from the polite package
angryBow <- bow(
  url = "https://angrymetalguy.com/",  # base URL
  user_agent = "JL ",  # identify ourselves
  force = TRUE
)
angryBow

# Website returns only two rules, and a requested 5 second interval between requests. No problem!




BrvtalScraper <- function (x) {
  dir.create("//Users/joellashmore/Downloads/scraped metal logos")
  for (i in x[1:length(x)]) {
    pagephotos <- read_html(i) %>%
      html_nodes(".c_img")  %>%
      html_nodes("img") %>%
      html_attr("src") %>%
      noquote()
    
    artist_album <- read_html(i) %>%
      html_nodes("h4 a") %>%
      html_attr("title")
    
    print(i)
    print(artist_album)
    
    for (photo in 1:(min(length(pagephotos), length(artist_album)))) {
      filename <-
        str_extract(pagephotos[photo], '[^/]+(?=/$|$)') # Regexp to get everything after the last slash in the web address
      download.file(
        pagephotos[photo],
        destfile = paste0(
          "//Users/joellashmore/Downloads/scraped metal logos/",
          artist_album[photo],
          ".jpg"
        ),
        mode = "w"
      )
      Sys.sleep(5) # sleep 5 seconds as per request
    }
  }
}

# test
BrvtalScraper(ToBeScraped)


