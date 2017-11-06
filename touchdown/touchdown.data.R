setwd("/Users/johnrivas/Desktop/UHD/SCHOOL/Fall_2017/Predictive/Project")
nfl <- read.csv("nfl.final.csv")
nfl <- nfl[,-1]

# Only columns that will be used in play type classification models
nfl.pt <- nfl[,c(4:8,10:15, 18:20 ,26, 31)]

# Only plays that are run/pass
nfl.pt <- nfl.pt[nfl.pt$PlayType2 %in% c("Pass Deep left", "Pass Deep middle", "Pass Deep right","Pass Short left","Pass Short middle", "Pass Short right",
                               "Run left end", "Run left tackle", "Run left guard", "Run middle", "Run right guard", "Run right tackle", "Run right end"), ]

# Change variables to correct type
nfl.pt$qtr <- as.factor(nfl.pt$qtr)
nfl.pt$down <- as.factor(nfl.pt$down)
nfl.pt$time <- as.integer(nfl.pt$time)
nfl.pt$PlayType2 <- droplevels(nfl.pt$PlayType2)

# 
nfl.pt$PlayType2 <-  make.names(nfl.pt$PlayType2)

# Only complete cases
x <- complete.cases(nfl.pt)
nfl.pt <- nfl.pt[x,]

# Trainign and Testing sets
set.seed(123)
intrain <- createDataPartition(y = nfl.pt$PlayType2, p = 0.7, list = FALSE) 
assign("training", nfl.pt[intrain,] , envir = .GlobalEnv)
assign("testing",  nfl.pt[-intrain,] , envir = .GlobalEnv)

write.csv(training,"touchdown.train.csv")
write.csv(testing,"touchdown.test.csv")
