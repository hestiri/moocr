# The sentiment analysis uses two packages plyr and stringr to manipulate strings.
moocr_sentiment <- function() {
    
    pos.words <- read.csv("positive-words.csv")
    neg.words <- read.csv("negative-words.csv")
    
    pos.words <- scan("positive-words.csv", what = 'character')
    neg.words <- scan("negative-words.csv", what = 'character')
    
    pos.words <- c(pos.words, 'goood', 'nicee' ,'cool', 'wow')
    neg.words <- c(neg.words, 'wtf', 'shitty', 'bad', 'no', 'freaking', 'sucks', 'horrible', 'ghgh')
    
    score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
    {
        
        # we got a vector of sentences. plyr will handle a list
        # or a vector as an 'l' for us
        # we want a simple array ('a') of scores back, so we use 
        # 'l' + 'a' + 'ply' = 'laply':
        
        scores <- laply(sentences, function(sentence, pos.words, neg.words) {
            
            # clean up sentences with R's regex-driven global substitute, gsub():
            sentence <- gsub('[[:punct:]]', '', sentence)
            sentence <- gsub('[[:cntrl:]]', '', sentence)
            sentence <- gsub('\\d+', '', sentence)
            # and convert to lower case:
            sentence <- tolower(sentence)
            
            # split into words. str_split is in the stringr package
            word.list <- str_split(sentence, '\\s+')
            # sometimes a list() is one level of hierarchy too much
            words <- unlist(word.list)
            
            # compare our words to the dictionaries of positive & negative terms
            pos.matches <- match(words, pos.words)
            neg.matches <- match(words, neg.words)
            
            # match() returns the position of the matched term or NA
            # we just want a TRUE/FALSE:
            pos.matches <- !is.na(pos.matches)
            neg.matches <- !is.na(neg.matches)
            
            # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
            score <- sum(pos.matches) - sum(neg.matches)
            
            return(score)
        }, pos.words, neg.words, .progress=.progress )
        
        scores.df <- data.frame(score=scores, text=sentences)
        return(scores.df)
    }
    
}
