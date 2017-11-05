##################################################################################################################
############ This is a demo of the proposed application for a football coaching play-caller aid.
############ The intent of this demo is to demonstrate the architecture and main idea for this project
############ A description of the application is the file "coaching aid - pilot version.model.R".
############ Make sure the file metioned above is in the same directory as this file before running. 
##################################################################################################################
############ The purpose of this app is to aid football coach play callers. 
############ It uses play-by-play data to find insights.
############ It predicts various aspects for all possible play types.
############ Play caller can use this to make a better decision. 
############ It will print a table for each possible play type.
############ The table includes predicted yards, probability of first down and probability of touchdown
##################################################################################################################

# Current game situation / Hypothetical situation ####
# The final app uses 20 variables to create models.
Drive <- 2
qtr <- 1
down <- 2
TimeSecs <- 3300
ydstogo <- 6
source("coaching aid - pilot version.model.R")

# Current game situation / Hypothetical situation ####
print(results)