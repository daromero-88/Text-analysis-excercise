#####TEXT ANALYSIS IN R######

######WEBSITES######

#more information in the measles twitter scripts! 
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

######PACKAGES INSTALLATION#####
install.packages("tm")  #for text mining
install.packages("SnowballC") #for text stemming, which means retrieving only the roots of inflected words! 
install.packages("wordcloud") #word-cloud generator 
install.packages("RColorBrewer") #color palettes
install.packages ('qdapTools') #read docx files and transform them in plain text

#####LIBRARIES#####

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library ('qdapTools')

#####WORKING DIRECTORY#####

setwd ('/Users/daniel/Documents/GitHub/Text-analysis-excercise')

#####FROM DOCX TO PLAIN TXT FILES#####

#https://rdrr.io/cran/textreadr/man/read_docx.html
a = read_docx ('./UNIT1/3_std1_uni1.docx', skip = 0)
class(a) #character vector with four elements...
View (a[1])

#in order to write te txt file: 
b = read_docx ('./UNIT1/3_std2_uni1.docx', skip = 0)
writeLines(b, './UNIT1/3_std2_uni1.txt')

######READING TXT FILES#####

aa = readLines('./UNIT1/3_std1_uni1.txt') 
length (a)
length(aa) #it will read the last line as another element 

bb = readLines('./UNIT1/3_std2_uni1.txt')
length(a) == length(aa[1:4])
bb

#how to put together all the documents? we should count them here... 
cc = c(a, aa, b) #potentially this is a way...some of them are empty though (see line 9)
length(cc)
class(cc)

#####PUTTING A CORPUS TOGETHER#####
#at this point we don't longer have elements to see... 

docs = Corpus (VectorSource(cc)) 
inspect (docs)

######PLAYING WITH THE CORPUS#####

#replacing words or weird symbols: 
toSpace = content_transformer(function (x , pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")

#cleaning 
# Convert the text to lower case
docs = tm_map(docs, content_transformer(tolower))

# Remove numbers, NOT APPLIED
#docs = tm_map(docs, removeNumbers)

# Remove english common stopwords
docs = tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word, NOT APPLIED 
# specify your stopwords as a character vector
#This is done retrospectively after obtaining the document matrix
docs = tm_map(docs, removeWords, c("also", "will", "especially", "fat", "lab")) 

# Remove punctuations
docs = tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs = tm_map(docs, stripWhitespace)

# Text stemming: NOT APPLIED... THIS ONE CONVERTS TO THE ROOTS OF WORDS
# docs <- tm_map(docs, stemDocument)

####BUILDING A DOCUMENT MATRIX#####

dtm = TermDocumentMatrix(docs) #create a term document matrix
m = as.matrix(dtm) #shows the frequency of repeated terms 
v = sort(rowSums(m),decreasing=TRUE) #this order the terms by frequency! 
d = data.frame(word = names(v),freq=v)
head(d, 10)

write.csv (d, 'example1.csv', row.names = F)

#####CREATING THE WORDCLOUD#####

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#####FIND TERMS#####
findFreqTerms(dtm, lowfreq = 4)

#####WORD ASSOCIATION#####
findAssocs(dtm, terms = "accessory", corlimit = 0.3) #correlation with other terms!

#####HISTOGRAM FREQUENCIES#####

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#####READING A LIST OF ABSTRACTS######

#FOR LOOP READING ABSTRACTS: 
m = list.files (full.names = T, pattern = '.txt') #list of abstracts in an object

lt1 = list () #empty list to fill with the abstracts
for (i in m){
  rr = readLines(i) 
  enc = enc2utf8 (rr) #THIS LINE IS FIXING THE ENCODING PROBLEM...maybe hahaha
  lt1[[length(lt1)+1]]= enc #filling the list 
}




