suppressMessages(library(dplyr, warn.conflicts = FALSE, quietly=TRUE))
suppressMessages(library(ggplot2, warn.conflicts = FALSE, quietly=TRUE))
suppressMessages(library(grid, warn.conflicts = FALSE, quietly=TRUE))
suppressMessages(library("data.table",warn.conflicts = FALSE, quietly=TRUE))
suppressMessages(library("recommenderlab",warn.conflicts = FALSE, quietly=TRUE))
# suppressMessages(library(caret,warn.conflicts = FALSE, quietly=TRUE))

n <- 1e6
numUsers <- 1e4
appCount <- 1e3
typeFactor <- as.factor(1:10)
set.seed(789)
payments <- 
      data.table(userID = sample(1:numUsers,size = n,replace = TRUE),
                 cat = sample(typeFactor,replace = TRUE, prob = c(0.2,0.15,0.1,0.05,0.25,0.1,0.05,0.02,0.05,0.03)),
                 appID = sample(1:appCount,replace = TRUE),
                 amount = round(sample(c(abs(rnorm(n = 0.1*n,mean = 3000,sd = 2000)),rep(0,0.9*n))),-2),
                 payDate = sample(seq(
                       as.Date('2013/01/01'), 
                       as.Date('2016/01/01'), 
                       by="day"), n,replace = TRUE))

# Ratings Matrix is Filled Randomly. 
ratings = matrix(sample(0:5,numUsers*appCount,replace = TRUE,prob = c(0.9,0.02,0.02,0.02,0.02,0.02)),nrow = numUsers)

RFM <- payments  %>% 
      group_by(userID)  %>% 
      summarise(FirstPurchaseDate = min(payDate),
                LastPurchaseDate = max(payDate),
                NumberPayments = sum(amount > 0),
                NumberofApps = length(unique(appID)),
                Breadth = length(unique(cat)),
                TotalAmount = sum(amount))


LastDate <- max(RFM$LastPurchaseDate)
RFM <- RFM %>% mutate(R = as.numeric(LastDate-LastPurchaseDate),
                      PayRatio = NumberPayments / NumberofApps)

#Analysis
##  Exploratory Data Analysis 

PlotRecency <- ggplot(RFM, aes(R %/% 7)) + geom_histogram(binwidth = 3) 
PlotRecency <- PlotRecency + labs(x = 'Recency(Weeks Ago)' , y = 'Customer Count', title = 'Recency')

PlotFreq <- ggplot(RFM, aes(NumberPayments)) + geom_histogram(binwidth = 1)
PlotFreq <- PlotFreq + labs(x = 'Number of payments' , y = 'Customer Count', title = 'Frequency')

PlotMonetary <- ggplot(RFM, aes(TotalAmount)) + geom_histogram(binwidth = 1000)
PlotMonetary <- PlotMonetary  + labs(x = 'Total Amount (Tomans)' , y = 'Customer Count', title = 'Total Payement(Monetary)')

PlotNumApp <- ggplot(RFM,aes(NumberofApps)) + geom_histogram(binwidth = 5)
PlotNumApp <- PlotNumApp + labs(x = 'Number of Installed Apps', y = 'count', title = 'Installed Apps')

PlotTenure <- ggplot(RFM, aes(as.numeric(LastDate - FirstPurchaseDate) %/% 7)) + geom_histogram(binwidth = 1)
PlotTenure <- PlotTenure + labs(x = 'First payment (Weeks Ago)' , y = 'Customer Count', title = 'Tenure')

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(PlotRecency, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(PlotTenure, vp = viewport(layout.pos.row = 1,layout.pos.col = 2))
print(PlotNumApp, vp = viewport(layout.pos.row = 2,layout.pos.col = 1))
print(PlotFreq, vp = viewport(layout.pos.row = 2,layout.pos.col = 2))
print(PlotMonetary, vp = viewport(layout.pos.row = 3,layout.pos.col = 1))

quantile(RFM$TotalAmount,seq(0.5,1,by = 0.1))

summary(RFM$PayRatio)
avgPayDF <- payments  %>% filter(amount > 0)  %>% group_by(userID)  %>% summarise(count = n(), totalAmount = sum(amount)) %>% 
      mutate(averageAmount = totalAmount %/% count)
quant90Vals <- quantile(avgPayDF$averageAmount, c(0.05,0.95))

avgPlot <- ggplot(avgPayDF, aes(x = averageAmount)) + geom_histogram(binwidth = 500)
avgPlot <- avgPlot + labs(x = 'Average payment (Tomans)', y = 'Customer Count',title = 'Average payments')
avgPlot <- avgPlot + geom_vline(xintercept=quant90Vals,color="red", linetype="dashed", size=1)
# print(avgPlot, vp = viewport(layout.pos.row = 3,layout.pos.col = 2))
print(avgPlot)

RFM_Segs <- data.table(Recency_Week = as.numeric(LastDate - RFM$LastPurchaseDate) %/% 7)
RFM_Segs$Recency <- ordered(ifelse(RFM_Segs$Recency_Week <= 1,"0-1",
                                   ifelse(RFM_Segs$Recency_Week <= 4,"1-4",
                                          ifelse(RFM_Segs$Recency_Week <= 8,"4-8",
                                                 ifelse(RFM_Segs$Recency_Week <= 10,"8-10","10+")))),
                            levels = c('0-1','1-4','4-8','8-10','10+'))

RFM_Segs$Frequency_count <- RFM$NumberofApps
RFM_Segs$Frequency <- ordered(ifelse(RFM_Segs$Frequency_count <= 10,"0-10",
                                     ifelse(RFM_Segs$Frequency_count <= 10-50,"10-50",
                                            ifelse(RFM_Segs$Frequency_count <= 80,"50-80",
                                                   ifelse(RFM_Segs$Frequency_count <= 100,"80-100",
                                                          ifelse(RFM_Segs$Frequency_count <= 110,"100-110","110+"))))),
                        levels = c("0-10","10-50","50-80","80-100","100-110","110+"))
RFM_Segs$Monetary_Value <- RFM$TotalAmount
RFM_Segs$Monetary <- ordered(ifelse(RFM_Segs$Monetary_Value <= 1000,"0-1K",
                                    ifelse(RFM_Segs$Monetary_Value <= 10000,"1K-10K",
                                           ifelse(RFM_Segs$Monetary_Value <= 30000,"10K-30K",
                                                  ifelse(RFM_Segs$Monetary_Value <= 50000,"30K-50K","50K+")))),
                             levels = c("0-1K","1K-10K","10K-50K","10K-30K","30K-50K","50K+"))


# RFM_Segs$Breadth_count <- RFM$TypeCount
# RFM_Segs$Breadth <-  ordered(ifelse(RFM_Segs$Breadth_count <= 1,"1","2"),
#                              levels = c("1","2"))
# 
RFM_Segs$Tenure_weeks <- as.numeric(LastDate - RFM$FirstPurchaseDate) %/% 7
RFM_Segs$Tenure <- ordered(ifelse(RFM_Segs$Monetary_Value <= 140,"0-140",
                                        ifelse(RFM_Segs$Monetary_Value <= 150,"140-150",
                                         ifelse(RFM_Segs$Monetary_Value <= 155,"150-155","155+"))),
                           levels = c("0-140","140-150","150-155","155+"))

summary(RFM_Segs)
ratings[ratings==0] <- NaN
# RFM Clustering
RFM_Values <- select(RFM_Segs,Recency_Week,Frequency_count,Monetary_Value)
# Normalize Data for Clustering
 normalVec = function(x){
      (x - min(x)) / (max(x)-min(x))
 }
 
normRFM <- sapply(RFM_Values, FUN = normalVec)
clustRes <- kmeans(normRFM,3)

P2 <- ggplot(RFM_Segs,aes(Recency_Week,Frequency_count))
P2 <- P2 +  geom_point(aes(size = Monetary_Value,colour = factor(clustRes$cluster)),alpha = 0.1)
print(P2)

goodUsers <- RFM$userID[which(clustRes$cluster == 2)]

appRates <- colMeans(ratings[goodUsers,],na.rm = TRUE)
top10 <- sort(appRates,decreasing = TRUE, index.return=TRUE)$ix[1:10]
print(top10)

# Get Best Apps in First Cluster ...

# Recommendation System ....


trainIndex <- which(clustRes$cluster == 2)
affinityMatrix<- as(ratings,"realRatingMatrix")
recModel<-Recommender(affinityMatrix[trainIndex], method = "UBCF")

testIndex <- sample(which(clustRes$cluster != 2),10)
# NMF Analysis
recom <- predict(recModel, affinityMatrix[testIndex,], type="ratings")
recomMat <- as(recom,"matrix")
get10 <- function(x){
      sort(x,decreasing = TRUE, index.return = TRUE)$ix[1:10]
}

res  <- apply(recomMat, MARGIN = 1, FUN = get10)
resTbl <- data.table(userID = testIndex,res)
print(resTbl)