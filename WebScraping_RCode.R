# R Code Web Scraping in R ---------------------------------------------
library(tidyverse)
library(rvest)
library(RSelenium)
#setwd("...")

# Examples ---------------------------------------------------------------

unibi <- read_html(x = "https://uni-bielefeld.de/")
# Mark month, right click, inspect element, copy
# without using Pipe (%>%) 
monat_node <- html_elements(x = unibi, css = ".ubf-eventsDetails__monthName") 
html_text(x = monat_node)

# using Pipe (%>%)
"https://uni-bielefeld.de/" %>% 
  read_html() %>% 
  html_elements(css=".ubf-eventsDetails__monthName") %>% 
  html_text()

# scrape Wikipedia table
biele <- read_html("https://en.wikipedia.org/wiki/Bielefeld")
# scrape all tables and select the fourth one
party_table <- biele %>% 
  html_table(fill=TRUE, header=TRUE, trim=TRUE) %>% 
  .[[6]] 
party_table

# or select particular table by its css selector
party_table <- biele %>% 
  html_elements(css="table.wikitable:nth-child(46)") %>% 
  html_table(fill=TRUE, header=TRUE, trim=TRUE)
party_table[[1]] 


# Uebung 1 ---------------------------------------------------------------
seite <- "uebung1.html"
html <- read_html(seite)

sportschlagzeilen <- html %>%  html_nodes("p.sports:nth-child(5)") %>% html_text(trim=TRUE)
sportschlagzeilen

##
html %>%  html_elements("#politics") %>% html_text() %>% 
  str_trim -> politikschlagzeilen
politikschlagzeilen

unibi %>% html_nodes(".ubf-picture__img") %>% 
  html_attrs() %>% .[[1]]

unibi %>% html_nodes(".ubf-picture__img") %>% 
  html_attr("alt")

## Sparrenburg example
# read in source code from website
biele <- read_html("https://de.wikipedia.org/wiki/Bielefeld")

# get all attributes from Sparrenburg picture
bild <- biele %>% 
  html_nodes("div.thumb:nth-child(3) > div:nth-child(1) > a:nth-child(1) > img:nth-child(1)") %>% 
  html_attrs() %>% 
  .[[1]]
bild

# return only the value/content if the attribute height
biele %>% 
  html_nodes("div.thumb:nth-child(3) > div:nth-child(1) > a:nth-child(1) > img:nth-child(1)") %>% 
  html_attr(name="height") 

### Uebung 2
html <- read_html("uebung2.html")
html %>% html_nodes("img") %>% html_attr("alt")
html %>% html_nodes("img") %>% html_attr("src")
html %>% html_table() %>% .[[1]] %>% dim

##
# immowelt.de ---------------------------------------------------------------
## Teil 1; I used a loop here, but this is not necessary
# dataset
df <- tribble(~kaltmiete, ~flaeche,  ~zimmer)
df
# choose two urls that are currently available on the webpage (check)!
# e.g. 
url <-c("https://www.immowelt.de/expose/24g5n5r","https://www.immowelt.de/expose/2422n5r")

# loop that does the same for i=1 and i=2
for (i in 1:length(url))
{
  html <- read_html(url[i])
  
  kaltmiete <- 
    html %>% 
    html_nodes("div.has-font-300 > strong:nth-child(1)") %>% 
    html_text() %>% 
    str_replace_all("\\.",",") %>% 
    str_replace_all("\\,",".") %>% 
    parse_number
  
  flaeche <- 
    html %>% 
    html_nodes("div.flex:nth-child(2) > div:nth-child(1) > span:nth-child(1)") %>% 
    html_text() %>% 
    str_replace_all("\\.",",") %>% 
    str_replace_all("\\,",".") %>% 
    parse_number
      
  zimmer <- 
    html %>% 
    html_nodes("div.hardfact:nth-child(2) > span:nth-child(1)") %>% 
    html_text() %>% 
    str_replace_all("\\.",",") %>% 
    str_replace_all("\\,",".") %>%  
    parse_number
  
   
  df <- add_row(df,kaltmiete, flaeche, zimmer)  
}

View(df)
mean(df$kaltmiete)

#### Selenium
rD <- rsDriver(browser=c("firefox"), port=4550L)
remDr <- rD$client

# relevant URL:
url <- paste0("https://www.immowelt.de/")
# navigate browser to URL:
remDr$navigate(url)

# type in Bielefeld and press enter
ortsfeld <- remDr$findElement(using = "css", "#tbLocationInput")
ortsfeld$sendKeysToElement(list("Bielefeld"))
ortsfeld$sendKeysToElement(list("", key = "enter"))

# Scroll down
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

# get current url
url <- remDr$getCurrentUrl() %>% unlist()

links <- url %>% read_html() %>%
  # select everything that has the tag </a> as these tags contain the links
  html_nodes("a") %>%
  # and select the attribute values of href as these are the links
  html_attr("href")

# There are still some duplicate links and some that directs to the owner of the flats instead to the ads.
# By checking manually, one can see that https://www.immowelt.de/expose/...
# directs to hte ads, thus we only want to have these links and remove the duplicates
links <- links %>%
  str_subset(pattern = "/expose/") %>% unique()
links




