########## Packages Loading ##########
library(recosystem)
library(rectools)
library(ggplot2)


########## Pre-process ##########
users <- read.table("u.user", header = FALSE, sep = "|", quote = "\"") # user information
colnames(users) = c("user", "age", "gender", "occupation", "zipcode")
users <- users[, -5]

dms1 <- dummy(users$gender); dms1 <- as.data.frame(dms1)
dms2 <- dummy(users$occupation); dms2 <- as.data.frame(dms2)

users$gender <- NULL; users$occupation <- NULL
users <- cbind(users, dms1, dms2)

items <- read.table("u.item", header = FALSE, sep = "|", quote = "\"") # item attribute
colnames(items) = c("item", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
items <- items[, -(2:5)]

rating <- read.table("u.data", col.names = c("user", "item", "rating", "timestamp")) # rating
rating <- rating[, -4]

rating <- merge(rating, users, by = "user", all.x = TRUE)
rating <- merge(rating, items, by = "item", all.x = TRUE)
rating <- cbind(rating[, 2], rating[, -2]); names(rating)[1]="user" # rating with covariates
set.seed(1); rating <- rating[sample(nrow(rating)), ]

n <- nrow(rating) # number of ratings
s <- 100 # simulation times

runningTime <- matrix(0, s, 9) # record running time
naNum <- matrix(0, s, 7) # recordr numbers of NA
MAE <- matrix(0, s, 6) # record MAE
RMSE <- matrix(0, s, 6) # record RMSE


########## Pre-experiment for Matrix Factorization ##########
set.seed(1)
in_train <- rep(TRUE, n)
    in_train[sample(1:n, size = round(0.1*n, 0))] <- FALSE # index of testing set
        ratingTrain <- rating[(in_train), ] # training set

write.table(ratingTrain[, 1:3], file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
    train_set <- data_file("trainset.txt", index1 = TRUE)

options <- NULL
    options[[1]] <- list(dim = 10L, costp_l1 = 0, costp_l2 = 0.01, costq_l1 = 0, costq_l2 = 0.01)
    options[[2]] <- list(dim = 10L, costp_l1 = 0, costp_l2 = 0.1, costq_l1 = 0, costq_l2 = 0.1)
    options[[3]] <- list(dim = 20L, costp_l1 = 0, costp_l2 = 0.01, costq_l1 = 0, costq_l2 = 0.01)
    options[[4]] <- list(dim = 20L, costp_l1 = 0, costp_l2 = 0.1, costq_l1 = 0, costq_l2 = 0.1)

lossRec_l1 <- function(x, y){
    lossFun <- NULL
    for(i in 1:100){
        r <- Reco()
        opts <- r$tune(train_set, opts = c(x, loss = "l1", lrate = y, niter = i, nthread = 4))
        lossFun[i] <- opts$res$loss_fun
    }
    lossFun
}    

lossRec_l2 <- function(x, y){
    lossFun <- NULL
    for(i in 1:100){
        r <- Reco()
        opts <- r$tune(train_set, opts = c(x, loss = "l2", lrate = y, niter = i, nthread = 4))
        lossFun[i] <- opts$res$loss_fun
    }
    lossFun
}    

loss_l11 <- sapply(options, function(x){lossRec_l1(x, 0.03)})
loss_l12 <- sapply(options, function(x){lossRec_l1(x, 0.01)})
loss_l21 <- sapply(options, function(x){lossRec_l2(x, 0.03)})
loss_l22 <- sapply(options, function(x){lossRec_l2(x, 0.01)})

matplot(matrix(21:100, 80, 4), loss_l11[21:100, ], type = "l", col = c(1, 2, 4, 3), xlab = "iterations", ylab = "loss function (MAE)", ylim = c(.72, .79))
    legend(20, .79, legend=c("10-0.01-0.01", "10 - 0.1 - 0.1", "20-0.01-0.01", "20 - 0.1 - 0.1"), lty=1:4, col = c(1, 2, 4, 3), bty = "n")

matplot(matrix(21:100, 80, 4), loss_l12[21:100, ], type = "l", col = c(1, 2, 4, 3), xlab = "iterations", ylab = "loss function (MAE)", ylim = c(.72, .79))
    legend(85, .79, legend=c("10-0.01-0.01", "10 - 0.1 - 0.1", "20-0.01-0.01", "20 - 0.1 - 0.1"), lty=1:4, col = c(1, 2, 4, 3), bty = "n")

matplot(matrix(21:100, 80, 4), loss_l21[21:100, ], type = "l", col = c(1, 2, 4, 3), xlab = "iterations", ylab = "loss function (RMSE)", ylim = c(.92, 1))
    legend(20, 1, legend=c("10-0.01-0.01", "10 - 0.1 - 0.1", "20-0.01-0.01", "20 - 0.1 - 0.1"), lty=1:4, col = c(1, 2, 4, 3), bty = "n")

matplot(matrix(21:100, 80, 4), loss_l22[21:100, ], type = "l", col = c(1, 2, 4, 3), xlab = "iterations", ylab = "loss function (RMSE)", ylim = c(.92, 1))
    legend(85, 1, legend=c("10-0.01-0.01", "10 - 0.1 - 0.1", "20-0.01-0.01", "20 - 0.1 - 0.1"), lty=1:4, col = c(1, 2, 4, 3), bty = "n")


########## Experiment ##########
for(i in 1:100){
    
    ##### Data Partitioning #####
    set.seed(i)
    in_train <- rep(TRUE, n)
    in_train[sample(1:n, size = round(0.1*n, 0))] <- FALSE # index of testing set
    ratingTrain <- rating[(in_train), ] # training set
    ratingTest <- rating[(!in_train), ] # testing set
    
    
    ##### Matrix Factorization, MF #####
    # default opts = list(dim = c(10L, 20L), costp_l1 = c(0, 0.1), costp_l2 = c(0.01, 0.1), costq_l1 = c(0, 0.1), costq_l2 = c(0.01, 0.1), 
    # loss = "l2", lrate = c(0.01, 0.1), niter = 20, nfold = 5, nthread = 1, nmf = FALSE, verbose = FALSE)
    write.table(ratingTrain[, 1:3], file = "trainset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
    write.table(ratingTest[, 1:3], file = "testset.txt", sep = " ", row.names = FALSE, col.names = FALSE)
    
    train_set <- data_file("trainset.txt", index1 = TRUE)
    test_set <- data_file("testset.txt", index1 = TRUE)
    
    ##### MF for l1 loss fuction #####
    timeStart <- proc.time()

        r <- Reco()
        opts_tune <- r$tune(train_set, opts = list(costp_l1 = 0, costq_l1 = 0, loss = "l1", lrate = 0.01, niter = 80, nthread = 4))

    timeMid <- proc.time()

        r$train(train_set, opts = c(opts_tune$min, loss = "l1", niter = 100, nthread = 4))
        pred <- r$predict(test_set, out_memory())

    runningTime[i, 1] <- (proc.time() - timeMid)[1] # recoed running time for MF(MAE)  without tuning
    runningTime[i, 2] <- (proc.time() - timeStart)[1] # recoed running time for MF(MAE)
    
    naNum[i, 1] <- sum(is.na(pred))
    MAE[i, 1] <- mean(abs(ratingTest[, 3] - pred), na.rm = TRUE) # recoed MAE for MF
    
    ##### MF for l2 loss fuction #####
    timeStart <- proc.time()
    
        r <- Reco()
        opts_tune <- r$tune(train_set, opts = list(costp_l1 = 0, costq_l1 = 0, lrate = 0.01, niter = 80, nthread = 4))
        
    timeMid <- proc.time()
    
        r$train(train_set, opts = c(opts_tune$min, niter = 100, nthread = 4))
        pred <- r$predict(test_set, out_memory())
    
    runningTime[i, 3] <- (proc.time() - timeMid)[1] # recoed running time for MF(RMSE) without tuning
    runningTime[i, 4] <- (proc.time() - timeStart)[1] # recoed running time for MF(RMSE)
    
    naNum[i, 2] <- sum(is.na(pred))
    RMSE[i, 1] <- sqrt(mean((ratingTest[, 3] - pred)^2, na.rm = TRUE)) # recoed RMSE for MF
    
    
    ##### Maximum Likelihood Estimate for Random Effects Model, MLE #####
    ##### MLE without covariates #####
    timeStart <- proc.time()
    
        ydotMLE <- findYdotsMLE(ratingTrain[, -c(4:44)])
        pred <- predict.ydotsMLE(ydotMLE, ratingTest[, -c(3:44)])
    
    runningTime[i, 5] <- (proc.time() - timeStart)[1]
    naNum[i, 3] <- sum(is.na(pred))
    
    MAE[i, 2] <- mean(abs(ratingTest[, 3] - pred), na.rm = TRUE)
    RMSE[i, 2] <- sqrt(mean((ratingTest[, 3] - pred)^2, na.rm = TRUE))
    
    ##### MLE with user information #####
    timeStart <- proc.time()
    
        ydotMLE <- findYdotsMLE(ratingTrain[, -c(26:44)])
        pred <- predict.ydotsMLE(ydotMLE, ratingTest[, -c(3, 26:44)])
    
    runningTime[i, 6] <- (proc.time() - timeStart)[1]
    naNum[i, 4] <- sum(is.na(pred))
    
    MAE[i, 3] <- mean(abs(ratingTest[, 3] - pred), na.rm = TRUE)
    RMSE[i, 3] <- sqrt(mean((ratingTest[, 3] - pred)^2, na.rm = TRUE))
    
    ##### MLE with item attribute #####
    timeStart <- proc.time()
    
        ydotMLE <- findYdotsMLE(ratingTrain[, -c(4:25)])
        pred <- predict.ydotsMLE(ydotMLE, ratingTest[, -c(3:25)])
    
    runningTime[i, 7] <- (proc.time() - timeStart)[1]
    naNum[i, 5] <- sum(is.na(pred))
    
    MAE[i, 4] <- mean(abs(ratingTest[, 3] - pred), na.rm = TRUE)
    RMSE[i, 4] <- sqrt(mean((ratingTest[, 3] - pred)^2, na.rm = TRUE))
    
    ##### MLE with user information & item attribute #####
    timeStart <- proc.time()
    
        ydotMLE <- findYdotsMLE(ratingTrain)
        pred <- predict.ydotsMLE(ydotMLE, ratingTest[, -3])
    
    runningTime[i, 8] <- (proc.time() - timeStart)[1]
    naNum[i, 6] <- sum(is.na(pred))
    
    MAE[i, 5] <- mean(abs(ratingTest[, 3] - pred), na.rm = TRUE)
    RMSE[i, 5] <- sqrt(mean((ratingTest[, 3] - pred)^2, na.rm = TRUE))
    
    ##### Method of Moments for Random Effects Model, MM #####
    timeStart <- proc.time()
    
        ydotMM <- findYdotsMM(ratingTrain[, -c(4:44)])
        pred <- predict.ydotsMM(ydotMM, ratingTest[, -c(3:44)])
    
    runningTime[i, 9] <- (proc.time() - timeStart)[1]
    naNum[i, 7] <- sum(is.na(pred))
    
    MAE[i, 6] <- mean(abs(ratingTest[, 3] - pred), na.rm = TRUE)
    RMSE[i, 6] <- sqrt(mean((ratingTest[, 3] - pred)^2, na.rm = TRUE))
}


########## Analysis ##########
    ##### Boxplot for MAE #####
    dat <- data.frame(Algorithm = factor(rep(c("MF", "MLE", "MLEU", "MLEI", "MLEUI", "MM"), each = 100), 
                                         levels = c("MF", "MLE", "MLEU", "MLEI", "MLEUI", "MM")), MAE = c(MAE[, 1], MAE[, 2], MAE[, 3], MAE[, 4], MAE[, 5], MAE[, 6]))
    bp <- ggplot(dat, aes(x = Algorithm, y = MAE)) + geom_boxplot(width = .5, lwd = 1)
    bp + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16))
    
    ##### Boxplot for RMSE #####
    dat <- data.frame(Algorithm = factor(rep(c("MF", "MLE", "MLEU", "MLEI", "MLEUI", "MM"), each = 100), 
                                         levels = c("MF", "MLE", "MLEU", "MLEI", "MLEUI", "MM")), RMSE = c(RMSE[, 1], RMSE[, 2], RMSE[, 3], RMSE[, 4], RMSE[, 5], RMSE[, 6]))
    bp <- ggplot(dat, aes(x = Algorithm, y = RMSE)) + geom_boxplot(width = .5, lwd = 1)
    bp + theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 16), axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 16))

    ##### statistics #####
    apply(MAE, 2, mean); apply(MAE, 2, var)
    apply(RMSE, 2, mean); apply(RMSE, 2, var)
    apply(runningTime, 2, mean)
    apply(naNum, 2, mean)