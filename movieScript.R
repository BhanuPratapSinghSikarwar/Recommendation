setwd("D://HPCC_Example//R//User-user collaborative filtering")
getwd()

## Read a file movie
moviesDs<-read.csv("MovieFile.csv",header = TRUE,sep = ',')

## we need header or not
head(moviesDs)

# Remove NA and replace it with 0
## is.na(moviesDs)
#moviesDs[is.na(moviesDs)] <- 0

#find the covariation between user 
corrMovies<-cor(moviesDs[sapply(moviesDs, is.numeric)], use="pairwise.complete.obs")

head(corrMovies)


#input 'X3867'
inputUser<-'X3867'

## function for finding top 5 recommendation for User 
TopFiveFunc<-function(inputcolomnname){

  #top five for 'X3867'
  TopFiveTemp<- corrMovies[order(corrMovies[,inputcolomnname],decreasing=T)[1:6],]
  #TopFiveTemp  ## not going to use directly
  #top five for 'X3867' names
  topFiveMatch<-names(TopFiveTemp[,inputcolomnname]  )  
  #topFiveMatch
  #top five for 'X3867' data
  topFivecov<-TopFiveTemp[,inputcolomnname][2:6]
  #topFivecov
  ###topFivecov<-topFivecov[2:6] ## removing corrv 1 for same user
  
  # Top five user have similar veiwing experience.
  TopFiveRatingWithName<-moviesDs[,c('X',topFiveMatch[topFiveMatch!=inputcolomnname])]
  TopFiveRating<-moviesDs[,c(topFiveMatch[topFiveMatch!=inputcolomnname])]
  TopFiveRating[is.na(TopFiveRating)] <- 0
  #head(TopFiveRating)
  
  #function for formula
  
  SumFunc<-function(val){
    num<-0
    den<-0
    
    for(i in 1:5){
      #if(is.numeric(val[i]){
        num<-num+topFivecov[i]*val[i]
        if(val[i]!=0){
          den<-den+topFivecov[i]
        }
     # }
    }
    num/den  
  }
  
  predictRating<-apply(TopFiveRating, 1, SumFunc)
  
  #typeof(predictRating)  str(predictRating)
  
  moviesDs[,c(topFiveMatch[topFiveMatch==inputcolomnname])]<-predictRating
  finalRating<-moviesDs[,c('X',inputcolomnname)]
  
  #finalRating
  
  #str(finalRating)
  # final rating
  
  finalRating[order(finalRating[,inputcolomnname],decreasing=T),]
}

## call function
TopFiveFunc(inputUser)



