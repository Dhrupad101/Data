# -*- coding: utf-8 -*-
"""
Created on Fri Sep 13 15:35:02 2019

@author: Gopi
"""

import requests   # Importing requests to extract content from a url
from bs4 import BeautifulSoup as bs # Beautifulsoup is for web scrapping...used to scrap specific content 
import re 

import nltk
from nltk.corpus import stopwords

import matplotlib.pyplot as plt
from wordcloud import WordCloud

# creating empty reviews list 
Macbook_reviews=[]
#forest = ["the","king","of","jungle"]

for i in range(1,20):
  mb=[]  
  url="https://www.amazon.in/Apple-MacBook-Air-13-3-inch-Integrated/product-reviews/B073Q5R6VR/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber="+str(i)
  #url = "https://www.amazon.in/All-New-Kindle-reader-Glare-Free-Touchscreen/product-reviews/B0186FF45G/ref=cm_cr_getr_d_paging_btm_3?showViewpoints=1&pageNumber="
  response = requests.get(url)
  soup1 = bs(response.content,"html.parser")# creating soup object to iterate over the extracted content 
  reviews = soup1.findAll(class_='a-size-base review-text review-text-content')# Extracting the content under specific tags  
  for i in range(len(reviews)):
    mb.append(reviews[i].text)  
  Macbook_reviews=Macbook_reviews+mb  # adding the reviews of one page to empty list which in future contains all the reviews


# writng reviews in a text file 
with open("macbook.txt","w",encoding='utf8') as output:
    output.write(str(Macbook_reviews))
    
    
    
 # Joinining all the reviews into single paragraph 
rev_string = " ".join(Macbook_reviews)



# Removing unwanted symbols incase if exists
rev_string = re.sub("[^A-Za-z" "]+"," ",rev_string).lower()
rev_string = re.sub("[0-9" "]+"," ",rev_string)



# words that contained in iphone 7 reviews
reviews_words = rev_string.split(" ")

stop_words = stopwords.words('english')

with open("D:\\stop.txt","r") as sw:
    stopwords = sw.read()

stopwords = stopwords.split("\n")


temp = ["this","is","awsome","Data","Science"]
[i for i in temp if i not in "is"]

reviews_words = [w for w in reviews_words if not w in stopwords]



# Joinining all the reviews into single paragraph 
rev_string = " ".join(reviews_words)

# WordCloud can be performed on the string inputs. That is the reason we have combined 
# entire reviews into single paragraph
# Simple word cloud


wordcloud_ip = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(rev_string)

plt.imshow(wordcloud_ip)

# positive words # Choose the path for +ve words stored in system
with open("D:\\positive-words.txt","r") as pos:
  poswords = pos.read().split("\n")
  
poswords = poswords[36:]



# negative words  Choose path for -ve words stored in system
with open("D:\\negative-words.txt","r") as neg:
  negwords = neg.read().split("\n")

negwords = negwords[37:]

# negative word cloud
# Choosing the only words which are present in negwords
neg_in_neg = " ".join ([w for w in reviews_words if w in negwords])

wordcloud_neg_in_neg = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(neg_in_neg)

plt.imshow(wordcloud_neg_in_neg)

# Positive word cloud
# Choosing the only words which are present in positive words
pos_in_pos = " ".join ([w for w in reviews_words if w in poswords])
wordcloud_pos_in_pos = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(pos_in_pos)

plt.imshow(wordcloud_pos_in_pos)

nltk 

# Unique words 
unique_words = list(set(" ".join(Macbook_reviews).split(" ")))

unique_wc =WordCloud(collocations= False).generate(unique_words)

################# IMDB reviews extraction ######################## Time Taking process as this program is operating the web page while extracting 
############# the data we need to use time library in order sleep and make it to extract for that specific page 
#### We need to install selenium for python
#### pip install selenium
#### time library to sleep the program for few seconds 

from selenium import webdriver
browser = webdriver.Chrome()
from bs4 import BeautifulSoup as bs
#page = "http://www.imdb.com/title/tt0944947/reviews?ref_=tt_urv"
page = "http://www.imdb.com/title/tt6294822/reviews?ref_=tt_urv"
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import ElementNotVisibleException
browser.get(page)
import time
reviews = []
i=1
while (i>0):
    #i=i+25
    try:
        button = browser.find_element_by_xpath('//*[@id="load-more-trigger"]')
        button.click()
        time.sleep(5)
        ps = browser.page_source
        soup=bs(ps,"html.parser")
        rev = soup.findAll("div",attrs={"class","text"})
        reviews.extend(rev)
    except NoSuchElementException:
        break
    except ElementNotVisibleException:
        break
        

##### If we want only few recent reviews you can either press cntrl+c to break the operation in middle but the it will store 
##### Whatever data it has extracted so far #######
len(reviews)
len(list(set(reviews)))


import re 
cleaned_reviews= re.sub('[^A-Za-z0-9" "]+', '', reviews)

f = open("reviews.txt","w")
f.write(cleaned_reviews)
f.close()

with open("The_Post.text","w") as fp:
    fp.write(str(reviews))



len(soup.find_all("p"))