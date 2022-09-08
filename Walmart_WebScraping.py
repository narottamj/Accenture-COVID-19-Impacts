# -*- coding: utf-8 -*-
"""
Created on Sat Dec  4 09:40:05 2021

@author: jogoz
"""

#Hey UF team, I figured I'd throw you a bone and get you started on any 
#webscraping efforts you'll have to do. I prefer to use BeautifulSoup in 
#Python, and this is how I scraped all the walmart location data. It looks like 
#This process will be pretty similar for some other stores, so feel free to 
#build off the work here! 


#Walmart's website makes it so we have to do a couple steps
#Step 1: Get the link to every state's directory
#Step 2: Get the link to every city's directory within a state
#Step 3: Collect the data from all these city links
#Step 4: Perfrom minor cleaning

#Packages we'll need
from bs4 import BeautifulSoup
from urllib.request import Request, urlopen
import pandas as pd
import requests

#______________________Step 1: List to Every State Directory___________________
#If you want to learn more, go to this link: https://pythonprogramminglanguage.com/get-links-from-webpage/

#This chunk here is going to get all the text off the site
req = Request("https://www.walmart.com/store/directory")
html_page = urlopen(req)
soup = BeautifulSoup(html_page, "lxml")

#Select only the links we want on this site
links = [] #Create an empty list for our new links to go into
for link in soup.findAll('a'):
    links.append(link.get('href'))
#Filter out the non-relevant results
state_directories = links[23:74]

#Add the missing part of the hyperlink to each element in the list
for i in range(0, len(state_directories), 1):
    state_directories[i] = 'https://www.walmart.com' + state_directories[i]



#_____________________Step 2: List to Every City Directory_____________________
#This is basically the same process as before
#But now we are going to loop through each state's links

city_links = []
for i in range(len(state_directories)):
    req = Request(state_directories[i])
    html_page = urlopen(req)
    soup = BeautifulSoup(html_page, "lxml")
    for link in soup.findAll('a'):
        city_links.append(link.get('href'))


city_links = [i for i in city_links if i.startswith('/store/')]
city_links = [i for i in city_links if i != '/store/directory']

#Similar Cleaning Steps as we did in step 1
for i in range(0, len(city_links), 1):
    city_links[i] = 'https://www.walmart.com' + city_links[i]

#__________________Step 3: Scraping All The Sites______________________________

#Now we have all the links we need, but now the challenge is that 
#For cities with 2+ stores the website format is different

combs= []
combs1= []

for i in range(len(city_links)):
    URL = city_links[i]
    print(URL)
    page = requests.get(URL)
    soup = BeautifulSoup(page.content, 'html.parser')
    results = soup.find(class_='store-list-ul')
    if results == None:
        results = soup.find(class_='header-parts-container')
        if results == None: 
            print('Error',URL, 'is not a valid link')
        else: 
            walmart_elems = results.find_all('div', class_='header-store-type-address')
            for walmart_elems in walmart_elems:
                storeBanner = walmart_elems.find('span', class_='store-type-name')
                storeNumber = walmart_elems.find('span', class_='store-number')
                storeAddress = walmart_elems.find('span', class_='store-address-line-1')
                storeZip= walmart_elems.find('span', class_='store-address-postal')
                StoreCity= walmart_elems.find('span', class_='store-name-city')
                StoreState = walmart_elems.find('span', class_='store-address-state')
                Phone_Number = walmart_elems.find('div', class_='phone-cell')
                a1= StoreCity.text.strip()
                a2 =StoreState.text.strip()
                a3 = Phone_Number.text.strip()
                a4= storeBanner.text.strip()
                a5 = storeNumber.text.strip()
                a6= storeAddress.text.strip()
                a7= storeZip.text.strip()
                combs.append((a1, a2, a3,a4,a5,a6,a7))
    else: 
        job_elems = results.find_all('div', class_='storeDetails')
        for job_elem in job_elems:
            storeBanner = job_elem.find('a', class_='storeBanner')
            storeAddress = job_elem.find('div', class_='storeAddress')
            storeZip= job_elem.find('div', class_='storeZip')
            storePhoneNumber= job_elem.find('div', class_='storePhoneNumber')
            w= storeBanner.text.strip()
            x= storeAddress.text.strip()
            y= storeZip.text.strip()
            z = storePhoneNumber.text.strip()
            combs1.append((w,x, y,z))

#__________________________________Step 4: Cleaning the Data____________________

#In step 3 we created two different lists to store the different types of data
#Now we just need to clean it up a little so we can join our tables together

#This chunk will clean up the results from websites with only one store listing
df1 = pd.DataFrame(combs)
df1.columns = ['City', 'State_lil', 'Phone_Number', 'storeType','Store Number', 'Address', 'Zipcode']
df1['Store_Name'] = df1['storeType'] + ' ' + df1['Store Number']
df1['City_and_State'] = df1['City'] + ', ' +  df1['State_lil']
final_df1 = df1[['Store_Name', 'Address', 'City_and_State', 'Zipcode', 'Phone_Number']]

#This chunk cleans up the websites with 2+ listings
df2 = pd.DataFrame(combs1)
df2.columns = ['Store_Name', 'Address', 'State', 'Phone_Number']
df2['Zipcode'] = df2['State'].str[-5:]
df2['City_and_State'] = df2['State'].str[:-5]
final_df2= df2[['Store_Name', 'Address', 'City_and_State', 'Zipcode', 'Phone_Number']]

#Putting it in one complete dataframe
final_df = pd.concat([final_df1, final_df2])

#Export to csv and we are all done! 
final_df.to_csv('Walmart_Locations.csv', index= False)







