############################################################################################################
############ This file is to create a final dataset to create all models from. 
############ The first 20 variables are known-before-play variables.
############ Variables 21-25 variables are options variables.
############ Variables 26-36 variables are results variables.
############################################################################################################
# Bring in data
nfl <- read.csv("nfl0917.csv")
nfl <- nfl[,-1]

# Create vector of all features in the nfl play-by-ply dataset
features <- colnames(nfl)
features <- as.data.frame(features ) 

# Create vector of features known before the start of the play
pre.snap <- rbind(1,1,1,1,1,1,
                  1,1,1,1,1,1,
                  0,1,0,1,1,0,
                  0,0,0,0,0,0,
                  0,0,0,1,0,0,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,0,0,0,0,
                  0,0,0,1,1,1,
                  1)

# Create vector of options features
options.features <- rbind(0,0,0,0,0,0,
                          0,0,0,0,0,0,
                          0,0,0,0,0,0,
                          0,0,0,0,0,0,
                          0,0,1,0,0,0,
                          1,1,0,0,0,0,
                          1,1,0,0,0,0,
                          0,0,0,0,0,0,
                          0,0,0,0,0,0,
                          0,0,0,0,0,0,
                          0)

# Create vector of results features
resp.features <- rbind(0,0,0,0,0,0,
                       0,0,0,0,0,0,
                       1,0,1,0,0,0,
                       0,1,1,1,0,0,
                       0,0,0,0,1,1,
                       1,0,1,0,0,1,
                       0,0,0,1,0,0,
                       0,0,0,0,0,0,
                       0,0,0,0,0,0,
                       0,0,0,0,0,0,
                       0)

# Call the needed variables
pre.snap <- ifelse(pre.snap == 1, TRUE, FALSE)
options.features <- ifelse(options.features == 1, TRUE, FALSE)
resp.features <- ifelse(resp.features == 1, TRUE, FALSE)

# Create df of kbp features
ps.data <- nfl[,pre.snap]
ps.data <- ps.data[-1,]

# Create df of options features
opt.data <- nfl[,options.features]

opt.data$PlayType2 <- ifelse(nfl$PassAttempt == 1 & opt.data$PlayType != "No Play" , paste(opt.data$PlayType, opt.data$PassLength, opt.data$PassLocation),
                              ifelse(nfl$RushAttempt == 1 & opt.data$PlayType != "No Play" & opt.data$RunLocation == "middle", paste(opt.data$PlayType, opt.data$RunLocation),
                                     ifelse(nfl$RushAttempt == 1 & opt.data$PlayType != "No Play" & opt.data$RunLocation != "middle", paste(opt.data$PlayType, opt.data$RunLocation, opt.data$RunGap),"")))

opt.data <- opt.data[-1,]

# Create df of results features
resp.features <- nfl[,resp.features]
resp.features <- resp.features[-1,]

# Creat csv to work off
nfl.final <- cbind(ps.data,opt.data,resp.features)
write.csv(nfl.final, file = "nfl.final.csv")





