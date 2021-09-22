  reqURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "http://api.twitter.com/oauth/access_token"
  authURL <- "http://api.twitter.com/oauth/authorize"
  api_key <- 	""
  api_secret <- ""
  access_token <- ""
  access_token_secret <- 	""
  install.packages('twitteR')
  install.packages('plyr') 
  install.packages('ROAuth') 
  install.packages('stringr')
  install.packages('ggplot2')
  install.packages('dplyr')
  install.packages('wordcloud')
  install.packages('tm')
  install.packages('syuzhet')
  library(twitteR) ### for fetching the tweets
  library(plyr) ## for breaking the data into manageable pieces
  library(dplyr)
  library(ROAuth) # for R authentication
  library(stringr) # for sting processing
  library(ggplot2) # for plotting the results
  library(wordcloud)
  library(tm)
  library(syuzhet)
  download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
  cred <- OAuthFactory$new(consumerKey='',
                           consumerSecret='',
                           requestURL='https://api.twitter.com/oauth/request_token',
                           accessURL='https://api.twitter.com/oauth/access_token',
                           authURL='https://api.twitter.com/oauth/authorize')
  cred$handshake(cainfo="cacert.pem")	
  #save for later use for Windows
  save(cred, file="twitter authentication.Rdata")
  load("twitter authentication.Rdata")
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  registerTwitterOAuth(cred)
  
  search.string <- "#sridevi"
  no.of.tweets <- 50
  tweets <- twitteR::searchTwitter(search.string,n =50,lang ="en")
  #strip retweets
  
  strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
  #tweets <- searchTwitter(search.string, n=no.of.tweets, cainfo="cacert.pem", lang="en")
  
  tweets
  length.tweets<- length(tweets)
  length.tweets
  df <- twListToDF(tweets)
  df
  write.csv(df,"tweets3.csv")
  df<-sapply(tweets, function(x) x$getText())
  pos = scan('/Users/hp laptop/Documents/dataset/positive-words.txt', what='character', comment.char=';')
  
  neg = scan('/Users/hp laptop/Documents/dataset/negative-words.txt', what='character', comment.char=';')
  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
    
  {
    
    require(plyr)
    
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    
    # or a vector as an "l" for us
    
    # we want a simple array ("a") of scores back, so we use
    
    # "l" + "a" + "ply" = "laply":
    
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      
      sentence = gsub('[[:punct:]]', '', sentence)
      
      sentence = gsub('[[:cntrl:]]', '', sentence)
      
      
      # and convert to lower case:
      
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      
      
      word.list = str_split(sentence, '\\+')
      
      # sometimes a list() is one level of hierarchy too much
      
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      
      pos.matches = match(words, pos.words)
      
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      
      # we just want a TRUE/FALSE:
      
      pos.matches = !is.na(pos.matches)
      
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
      
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    
    return(scores.df)
    
  }
  analysis = score.sentiment(df, pos, neg)
  table(analysis$score)
  
  
  
  df1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", df)
  df2 = gsub("http[^[:blank:]]+", " ", df1)
  df3 = gsub("@\\w+", " ", df2)
  df4 = gsub("[[:punct:]]", " ", df3)
  df5 = gsub("[^[:alnum:]]", " ", df4)
  
  
  
  
  write.csv(df5,"tweets5.csv")
  df6<- Corpus (VectorSource (df5))  # create a corpus
  
  #df6 <-gsub ("/", " ", "my/Text") # replaces all '/' with space " "
  
  df6<-tm_map (df6, removePunctuation) # remove punctuations
  
  df6<-tm_map (df6, content_transformer(tolower)) # to remove numbers
  
  df6<-tm_map (df6, removeWords, stopwords("english")) # to remove stop words(like 'as' 'the' etc..)
  df6<-tm_map(df6, stripWhitespace)
  pal<- brewer.pal(6,"Dark2")
  #write(txt_corpus,"tweets6.csv")
  
  #Matrix <- TermDocumentMatrix(txt_corpus) # terms in rows
  
  #DTM <- DocumentTermMatrix(txt_corpus) # document no's in rows
  #TM
  
  
  
  
  wordcloud (df6,min.freq = 3,max.words = Inf,width=1000,height=1000,random.color = FALSE ,color= pal,rot.per = .25,random.order = FALSE)
  
  
  #sentiment analysis
  
  mysentiment<-get_nrc_sentiment(df5)
  sentimentscores<-data.frame(colSums(mysentiment[,]))
  names(sentimentscores)<- "score"
  sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
  rownames(sentimentscores) <- NULL
  ggplot(data = sentimentscores, aes(x= sentiment, y=score))+
    geom_bar(aes(fill= sentiment), stat = "identity")+
    theme(legend.position = "none")+
    xlab("sentiment")+ ylab("score")+ ggtitle("sentiment analysis")
  
  
  
  