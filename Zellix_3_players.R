library(rayshader)
library(tidyverse)

deck_comb=data.frame(combinations(41, 3, c(0:40), repeats.allowed = TRUE))

df=data.frame()
set.seed(4)
mytriggs= c(1:4)

leng_deck_comb=nrow(deck_comb)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = leng_deck_comb, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for (i in 1:leng_deck_comb) {
  
  percentmill=c()
  meanhighscores=c()
  for (triggers in mytriggs) {
  
    highscores=c()
    for (num in 1:10000){
     
      num_creatures1=deck_comb[i, "X1"]
      non_creatures1=99-num_creatures1
      deck1=rep(1, each=num_creatures1)
      deckappend1=rep(0, each=non_creatures1)
      deck1=append(deck1,deckappend1)
      
      num_creatures2=deck_comb[i, "X2"]
      non_creatures2=99-num_creatures2
      deck2=rep(1, each=num_creatures2)
      deckappend2=rep(0, each=non_creatures2)
      deck2=append(deck2,deckappend2)
      
      num_creatures3=deck_comb[i, "X3"]
      non_creatures3=99-num_creatures3
      deck3=rep(1, each=num_creatures3)
      deckappend3=rep(0, each=non_creatures3)
      deck3=append(deck3,deckappend3)
      
      deck1=sample(deck1)
      deck2=sample(deck2)
      deck3=sample(deck3)
      
      #while loop for number of how far through the deck the chain reaction got
      #another while loop for first loop not accounting for multiplying how many hits
      firstloop<-TRUE
      milled<-0
      CRXN<-1
      
      while (firstloop==TRUE) {
        milled<-sum(deck1[1:triggers],deck2[1:triggers],deck3[1:triggers])
        CRXN<-CRXN+1
        firstloop<-FALSE
      }
      
      while (CRXN<=milled) {
        numb=milled
        if (milled>=99){
          break
        }
        milled1<-sum(deck1[1:numb],deck2[1:numb],deck3[1:numb])
        milled<-milled1*triggers
        CRXN<-CRXN+1
      }
      highscores=append(highscores,milled)
    }
    percentmill=append(percentmill,sum(highscores>=99)/length(highscores)*100)
    meanhighscores=append(meanhighscores,mean(highscores))
  }
  
  df1=data.frame(meanhighscores,percentmill,mytriggs,num_creatures1,num_creatures2,num_creatures3)
  df=rbind(df,df1)
  setTxtProgressBar(pb, i)
  
}
df$meanhighscores[df$meanhighscores>=99] = 99

  
df=df%>%
  mutate(players=paste(as.character(num_creatures1), as.character(num_creatures2),as.character(num_creatures3), sep=" "),
         creatureSum=num_creatures1+num_creatures2+num_creatures3)

View(df)

#write.csv()
close(pb)
