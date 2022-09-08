# title: "Foreclosure Webscrape"
# author: "Lauren Truong"
# date: "12/24/2021"

# Citations:
# https://rworkshop-at-uf.github.io/
# Tek's Workshop: https://rworkshop-at-uf.github.io/session7_2/session7_2.html#6

# Install packages and load them into the library for use.
install.packages("RSelenium")
install.packages("rvest")
install.packages("dplyr")

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
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

###############################################

# BASIC NAVIGATION USING R SELENIUM:

# navigate() function
driver$client$navigate("https://www.foreclosure.com/listing/search?q=Florida&lc=foreclosure&lc=preforeclosure&lc=foreclosure&pg=1&o=p&ob=desc&loc=Florida&view=list&#")

# goBack() and goForward() functions
# driver$client$goBack()
# driver$client$goForward()
# driver$client$getCurrentUrl() # Check the current URLs.

###############################################

# getPageSource() function
# NEED TO ASK TEK: Write a loop to automatically change pages to extract data.
# Narrow down results by finding elements.

# driver$client$findElement(using="xpath", '//select [@id="footerBarPagination"]//option [@class="fr"]')$clickElement()
# driver$client$findElement(using="xpath", '//button [@class="pagination"]')$clickElement()

driver$client$getPageSource()[[1]] -> c_html # You will always need this to run c_html.

###############################################

# Data content to extract.
paths=c(
  HomePrice='.//span [@class="tdprice"]//strong',
  Address='.//a [@class="address"]')

c_html %>%
  rvest::read_html() %>%
  rvest::html_nodes(xpath='//div [@class="fl listingInfo "]//div [@class="conListingInfo"]') -> content

# Apply list and create data frame.
lapply(paths, function(x){rvest::html_node(content, xpath=x) %>%
    html_text()}) %>%
  do.call("cbind",.) %>%
  as.data.frame() -> foreclosuredf

View(foreclosuredf)

################################################
# Loop Pages

full_foreclosure_list=vector("list", length=209)

# Make R Selenium click pages
urls="https://www.foreclosure.com/listing/search?q=Florida&lc=foreclosure&lc=preforeclosure&lc=foreclosure&pg=1&o=p&ob=desc&loc=Florida&view=list&"
for(i in 1:209){
  gsub("NUM", i, urls) %>%
    driver$client$navigate()
  Sys.sleep(3)
  
  driver$client$getPageSource()[[1]] -> c_html
  c_html %>%
    rvest::read_html() %>%
    rvest::html_nodes(xpath='//div [@class="fl listingInfo "]//div [@class="conListingInfo"]') -> content
  
  # Apply list and create data frame.
  lapply(paths, function(x){rvest::html_node(content, xpath=x) %>%
      html_text()}) %>%
    do.call("cbind",.) %>%
    as.data.frame() -> full_foreclosure_list[[i]]
  
  print(i)
}

full_foreclosure_list %>%
  do.call("rbind",.) -> full_foreclosure_df

# Clean price and address
gsub("^\n|\n$", "", full_foreclosure_df$Address) %>%
  gsub("\n", ", ", .)-> full_foreclosure_df$Address

gsub("[^[:digit:]]", "", full_foreclosure_df$HomePrice) %>%
  as.numeric()->full_foreclosure_df$HomePrice
# full_foreclosure_df$Address <- gsub("\n", " ", full_foreclosure_df$Address, fixed=TRUE)

full_foreclosure_df
View(full_foreclosure_df)
# lapply(full_foreclosure_list, ncol)

write.csv(full_foreclosure_df, "FLforeclosure2022.csv")
str(full_foreclosure_df)
