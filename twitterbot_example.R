setwd("D:/Dropbox/Twitterbot")
library(twitteR)
library(ANLP)


consumer_key = "YOUR CONSUMER_KEY HERE"
consumer_secret = "YOUR CONSUMER_SECRET HERE"
token = "YOUR ACCESS TOKEN HERE"
secret = 'YOUR ACCESS_SECRET HERE'

## Step 1: Authenticate on Twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token=token, access_secret=secret)

## Step 2: Train your Markov Chain

dat = twitter.data
train.data <- sampleTextData(dat ,0.1)

train.data.cleaned <- cleanTextData(train.data) 
unigramModel <- generateTDM(train.data.cleaned,1)
bigramModel <- generateTDM(train.data.cleaned,2)
trigramModel <- generateTDM(train.data.cleaned,3)
nGramModelsList <- list(trigramModel,bigramModel,unigramModel)

## Step 3: Generate starting words

firstwords = regmatches(train.data, regexpr(" ", train.data), invert = TRUE)
firstwords = sapply(firstwords, FUN=function(x) x[1])

## Step 4: Generate 140 characters

generate_tweet = function(){
  init_string = sample(firstwords, 1)
  while(nchar(init_string) < 140) {
    new_word = predict_Backoff(init_string, nGramModelsList)
    init_string = paste(init_string, new_word, sep = " ")
  }
  return(init_string)
}

## Final Step: Post it!

tweet(generate_tweet())