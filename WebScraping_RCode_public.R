# R Code zum WebScraping Kurs ---------------------------------------------
library(tidyverse)
library(rvest)
library(RSelenium)
setwd("C:\\Users\\puetz5\\Desktop\\Statistische Beratung\\Web Scraping in R")


# Beispiele ---------------------------------------------------------------

unibi <- read_html(x="https://uni-bielefeld.de/")

# September markieren, Rechtsklick, Element untersuchen, Kopieren
unibi %>% html_nodes(css=".ubf-eventsDetails__monthName") %>% 
  html_text()
# ohne Pipe (%>%) sähe das so aus
monat_node <- html_nodes(x=unibi, css=".ubf-eventsDetails__monthName") 
html_text(monat_node)

# Wikipedia Tabelle scrapen
biele <- read_html("https://en.wikipedia.org/wiki/Bielefeld")
# Alle Tabellen scrapen und die zweite auswählen
biele %>% html_table(fill=TRUE, header=TRUE, trim=TRUE) %>% .[[2]] %>% View

# oder bestimmte Tabelle über css Selektor auswählen
klimatabelle <- biele %>% html_nodes(css=".toccolours") %>% 
  html_table(fill=TRUE, header=TRUE, trim=TRUE)
klimatabelle[[1]] %>% View()

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

# Sparrenburg
biele <- read_html("https://de.wikipedia.org/wiki/Bielefeld")

# alle Attribute
biele %>% html_nodes("div.thumb:nth-child(3) > div:nth-child(1) > a:nth-child(1) > img:nth-child(1)") %>% 
  html_attrs() 

# nur den Inhalt/den Wert des Attributes Höhe ausgeben
biele %>% html_nodes("div.thumb:nth-child(3) > div:nth-child(1) > a:nth-child(1) > img:nth-child(1)") %>% 
  html_attr("height") 

### Uebung 2
html <- read_html("uebung2.html")
html %>% html_nodes("img") %>% html_attr("alt")
html %>% html_nodes("img") %>% html_attr("src")
html %>% html_table() %>% .[[1]] %>% dim

##
# immowelt.de ---------------------------------------------------------------
## Teil 1; ich habe hier eine Schleife benutzt, aber das muss nicht sein
# Datensatz
df <- tribble(~kaltmiete, ~flaeche,  ~zimmer,~ausstattung)

# zwei URLs auswählen, ACHTUNG: Die müssen aktuell sein,
# also hier müssen wahrscheinlich andere angegeben werden
url <-c("https://www.immowelt.de/expose/2wapl4m?bc=13","https://www.immowelt.de/expose/2w5294m")

# Schleife die für i=1 und i=2 jeweils das gleiche macht
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
rD <- rsDriver(browser=c("firefox"),port=4570L)
remDr <- rD$client

# Relevante URL:
url <- paste0("https://www.immowelt.de/")
# Navigiere Browser zu URL:
remDr$navigate(url)

# gebe Bielefeld ein und drücke enter
ortsfeld <- remDr$findElement(using = "css", "#tbLocationInput")
ortsfeld$sendKeysToElement(list("Bielefeld"))
ortsfeld$sendKeysToElement(list("", key = "enter"))


# Runterscrollen
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

# Ganze Homepage auswaehlen
table.body <- remDr$findElements(using = "class", "modern_browser")

# Extrahiere alle enthaltenen links (href) und speichere sie als string ab
html <- unlist(sapply(table.body, function(x) {x$getElementAttribute("innerHTML")}))
# oder:
html <- unlist(table.body[[1]]$getElementAttribute("innerHTML"))


links <- html %>% read_html() %>%
  # suche alle Sachen mit tag </a> raus
  html_nodes("a") %>%
  # und davon alle href, denn das sind alle Links
  html_attr("href")


# Hier sind jedoch noch doppelte links Links dabei und welche, die zum Anbieter der Anzeigen fuehren
# und nicht zu den Anzeigen selbst. Per manuellem check sieht man, dass https://www.immowelt.de/expose/...
# zu den Anzeigen fuehrt, deshalb suchen wir nur diese Links und entfernen doppelte
links <- links %>%
  str_subset(pattern = "/expose/") %>% unique()
# Komplettieren der URLs (hier muss https://www.immowelt.de noch hinzugefuegt werden,
# weil es nicht im html-Quelltext steht, sieht man, wenn man links einmal ausgibt in R console)
links <- paste("https://www.immowelt.de", links, sep="")
links



