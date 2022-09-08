# title: "Food Banks Webscrape"
# author: "Lauren Truong"
# date: "12/24/2021"

# Citations:
# https://rworkshop-at-uf.github.io/
# Tek's Workshop: https://rworkshop-at-uf.github.io/session7_2/session7_2.html#6

# Install packages and load them into the library for use.
library(RSelenium) # Automates browsers. Scrap JavaScript enable pages.
library(rvest) # Webscraper.
library(dplyr)

# rsDriver() function creates a Selenium server.
driver <- rsDriver(browser = "chrome", chromever = "97.0.4692.71")

# Open and close client/browser.
# driver$client$open()
# driver$client$close()
# rm(driver) # Removing from the environment.

# Free port(s).
# If "Selenium server signals port = #### is already in use." Typically, use at the end!
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

###############################################

# BASIC NAVIGATION USING R SELENIUM:

# navigate() function
driver$client$navigate("https://www.feedingamerica.org/find-your-local-foodbank")

# goBack() and goForward() functions
driver$client$goBack()
driver$client$goForward()
driver$client$getCurrentUrl() # Check the current URLs.

###############################################

# getPageSource() function
# Write a loop to automatically change states.
# Narrow down results by finding elements.
driver$client$findElement(using="xpath", '//select [@id="find-fb-search-form-state"]//option [@value="FL"]')$clickElement()
driver$client$findElement(using="xpath", '//button [@class="red"]')$clickElement()
driver$client$getPageSource()[[1]] -> c_html

# Data content to extract.
paths=c(
  name='.//p [@class="name"]',
  address=".//p [not(@class)]",
  url='.//p [@class="url"]//a')

c_html %>%
  rvest::read_html() %>%
  rvest::html_nodes(xpath='//div [@class="results-box" and @data-orgid]') -> content

# Apply list and create data frame.
lapply(paths, function(x){rvest::html_nodes(content, xpath=x) %>%
    html_text()}) %>%
  do.call("cbind",.) %>%
  as.data.frame() -> FBdataframe

View(FBdataframe)

###############################################

# Cleanse address column.
FBdataframe$address <- gsub("([a-z])([A-Z])","\\1 \\2",FBdataframe$address) #space address and city
FBdataframe$address <- gsub('.{12}$', '', FBdataframe$address) #subtract 12 characters at end
FBdataframe$address <- gsub("([0-9])([A-Z])","\\1 \\2",FBdataframe$address)
FBdataframe$address <- gsub(".", ". ", FBdataframe$address, fixed=TRUE)
FBdataframe$address <- gsub("P. O.", "P.O.", FBdataframe$address, fixed=TRUE)
FBdataframe$address <- gsub("  ", " ", FBdataframe$address, fixed=TRUE)

# Cleanse url column.
FBdataframe$url <- gsub("org/", "org", FBdataframe$url, fixed=TRUE)

View(FBdataframe)