############################################################################################################
############ This is a demo of the proposed application for a football coaching play-caller aid.
############ The intent of this demo is to demonstrate the architecture and main idea for this project.
############ Accuracy is not important here. Only defining the architecture and main idea. 
############ A description of the application is below.
############ To run application, run file "coaching aid - pilot version.run.R" .
############ Make this file is in the same directory as the one mentioned above before running.
############################################################################################################

#########################################################################################
# Libraries, Data and Models####
# Library 
library(nflscrapR)

# Bring in data. Only one game's worth of data is used for this demo.
# The final application will use a considerable more amount of observations. 
# The models below were made with a "play.types" as variable.
# This variable, along with others, are considered 'options' variables.
# By that i mean variables that are the options of football coaches. 
# There two other types of variables. Ones that are known before the play. And lastly, variables that the results of the play. 
# Three variable types = known-before-play, options-for-play, and results-of-play. 
# We will review all 61 variables to determine what type of character it is.
# For demonstration purposes, a handful of each type of variable were chosen to create the simple models below.
# Final models will be more complex.

game <- nflscrapR::game_play_by_play(2013091509)
game$PlayType.RP <- paste(game$PlayType,".",game$PassLength,".",game$PassLocation,".",game$RunLocation)
game$PlayType.RP <- gsub("NA", "", game$PlayType.RP, fixed = TRUE)
game$PlayType.RP <- gsub(" ", "", game$PlayType.RP, fixed = TRUE)
game$PlayType.RP <- gsub("...", ".", game$PlayType.RP, fixed = TRUE)
test <- subset(game, PlayType == "Run" | PlayType == "Pass" )
test$PlayType.RP <-  as.factor(test$PlayType.RP)
str(test$PlayType.RP)
play.types <- test$PlayType.RP
play.types <- levels(play.types)
test <- test[,c(3,4,5,8,12,62,20,15,22)]    

# Fit three models. One for yards, one for % first down and one for % touchdown
# Note that the options variables were used in the model along with the known-before-play variables. 
# The response variables are the results-of-play variables

# This are simple models. The final models will likely be created in python using keras
# Those models can be broguht into R as *.h5 files
fit.yds <- lm(Yards.Gained~., data = test[,-c(8,9)]) 
summary(fit.yds)
fit.fd <- lm(FirstDown~., data = test[,-c(7,9)]) 
summary(fit.fd)
fit.td <- lm(Touchdown~., data = test[,-c(7,8)]) 
summary(fit.td)

#########################################################################################

# Predict "present day" game situation#####
# Create object to be filled by prediction from models.
# The options variable used in the models are all factors.
# The idea is to run the model for each posible option a coach has. There are eleven.
# The model will be ran 11 times, once for each level of the factor option variable.
# We will create the blank table to store the predicted values to. 

PlayType.RP <- NA
Pred.Yds <- NA
prop.fd <- NA
prop.td <- NA
new.situation <- cbind(Drive,qtr,down,TimeSecs,ydstogo,PlayType.RP,Pred.Yds,prop.fd,prop.td)
new.situation <- rbind(new.situation,new.situation,new.situation,new.situation,
                       new.situation,new.situation,new.situation,new.situation,
                       new.situation,new.situation,new.situation)

# Fill in table with predict values
# There are 11 rows. One for every option a coach has. 

for(i in 1:length(play.types)){
  new.situation <- as.data.frame(new.situation)
  new.situation$PlayType.RP[i] <- play.types[i]
  new.situation$Pred.Yds[i] <- predict(fit.yds,new.situation[i,-c(7,8,9)])
  new.situation$prop.fd[i] <- round(predict(fit.fd,new.situation[i,-c(7,8,9)]),2)
  new.situation$prop.td[i] <- round(predict(fit.td,new.situation[i,-c(7,8,9)]),2)
}

# Predicted Yards, First Down Probability and Touchdown Probability  ####
results <- new.situation[-c(1,8),6:9]
