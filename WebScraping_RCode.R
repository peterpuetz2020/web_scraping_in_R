# R Code Web Scraping in R ---------------------------------------------
library(tidyverse)
library(rvest)
library(RSelenium)
#setwd("...")

# Examples ---------------------------------------------------------------

unibi <- read_html(x="https://uni-bielefeld.de/")
# Mark month, right click, inspect element, copy
# without using Pipe (%>%) 
monat_node <- html_nodes(x=unibi, css=".ubf-eventsDetails__monthName") 
html_text(monat_node)

# using Pipe (%>%)
unibi %>% 
  html_nodes(css=".ubf-eventsDetails__monthName") %>% 
  html_text()

# scrape Wikipedia table
biele <- read_html("https://en.wikipedia.org/wiki/Bielefeld")
# scrape all tables and select the fourth one
biele %>% html_table(fill=TRUE, header=TRUE, trim=TRUE) %>% .[[4]] %>% View

# or select particular table by its css selector
pop_table <- biele %>% html_nodes(css=".toccolours") %>% 
  html_table(fill=TRUE, header=TRUE, trim=TRUE)
pop_table[[1]] %>% View()
pop_table_df <- pop_table[[1]]


# Uebung 1 ---------------------------------------------------------------
seite <- "uebung1.html"
html <- read_html(seite)

sportschlagzeilen <- html %>%  html_nodes(".sports") %>% html_text(trim=TRUE)
sportschlagzeilen

##
html %>%  html_nodes("#politics") %>% html_text() %>% 
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
biele %>% html_nodes("div.thumb:nth-child(3) > div:nth-child(1) > a:nth-child(1) > img:nth-child(1)") %>% 
  html_attrs() 

# return only the value/content if the attribute height
biele %>% html_nodes("div.thumb:nth-child(3) > div:nth-child(1) > a:nth-child(1) > img:nth-child(1)") %>% 
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
df <- tribble(~kaltmiete, ~flaeche,  ~zimmer,~ausstattung)

# choose two urls that are currently available on the webpage (check)!
# e.g. 
url <-c("https://www.immowelt.de/expose/2f54v4z?bc=13","https://www.immowelt.de/expose/2ym8v4t?bc=13")

# loop that does the same for i=1 and i=2
for (i in 1:length(url))
{
  html <- read_html(url[i])
  kaltmiete <- 
    html %>% 
    html_nodes(".hardfacts > div:nth-child(1) > strong:nth-child(1)") %>% 
    html_text() %>% 
    str_replace_all(",",".") %>% 
    parse_number
  
  flaeche <- 
    html %>% 
    html_nodes(".hardfacts > div:nth-child(2)") %>% 
    html_text() %>% 
    str_replace_all(",",".") %>% 
    parse_number
      
  zimmer <- 
    html %>% 
    html_nodes(".hardfacts > div:nth-child(3)") %>% 
    html_text() %>% 
    str_replace_all(",",".") %>% 
    parse_number
  
  ausstattung <- 
    html %>% 
    html_nodes("div.read:nth-child(3) > div:nth-child(1) > div:nth-child(2) > p:nth-child(1)") %>% 
    html_text()
  ausstattung %>% 
    str_replace_all(pattern="\r", replacement="") %>% 
    str_replace_all(pattern="\n", replacement="") %>% 
    trimws()
   
  df <- add_row(df,kaltmiete, flaeche, zimmer, ausstattung)  
}

View(df)


#### Selenium
rD <- rsDriver(browser=c("firefox"),port=4558L)
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

# select entire homepage
table.body <- remDr$findElements(using = "class", "modern_browser")

# extract all available links (href) and save them as strings
html <- unlist(sapply(table.body, function(x) {x$getElementAttribute("innerHTML")}))
# or:
html <- unlist(table.body[[1]]$getElementAttribute("innerHTML"))

links <- html %>% read_html() %>%
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
# Complete the URLs (add https://www.immowelt.de,
# as it is not writtten down in the html source codes
links <- paste("https://www.immowelt.de", links, sep="")
links



