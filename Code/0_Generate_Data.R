library(ggplot2)
library(dplyr)
library(data.table)
library(grid)
# Generate Data
n <- 3000
numUsers <- 500
typeFactor <- as.factor(c('App','Game'))
orders <- 
      data.table(CustID = sample(1:numUsers,size = n,replace = TRUE),
                 type = sample(typeFactor,n,replace = TRUE, prob = c(0.5,0.5)),
                 amount = round(sample(c(abs(rnorm(n = 0.1*n,mean = 3000,sd = 2000)),rep(0,0.9*n))),-2),
                 OrderDate = sample(seq(
                       as.Date('2013/01/01'), 
                       as.Date('2016/01/01'), 
                       by="day"), n,replace = TRUE))

# A Glance at Data
str(orders)
summary(orders)
# Building RFM Features
RFM <- orders  %>% 
      group_by(CustID)  %>% 
      summarise(FirstPurchaseDate = min(OrderDate),
                LastPurchaseDate = max(OrderDate),
                NumberOrders = n(),
                NumberSKUs = length(unique(type)) ,
                TotalAmount = sum(amount))

LastDate <- max(RFM$LastPurchaseDate)
RFM <- RFM %>% mutate(R = as.numeric(LastDate-LastPurchaseDate))

# ------------------ Exploratory Data Analysis ------------------------------
PlotRecency <- ggplot(RFM, aes(R %/% 7)) + geom_histogram() + geom_smooth()
PlotFreq <- ggplot(RFM, aes(NumberOrders)) + geom_histogram()
PlotMonetary <- ggplot(RFM, aes(TotalAmount)) + geom_histogram()
PlotBreadth <- ggplot(RFM, aes(NumberSKUs)) + geom_histogram()
PlotTenure <- ggplot(RFM, aes(as.numeric(LastDate - FirstPurchaseDate) %/% 7)) + geom_histogram()
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(PlotRecency, vp = viewport(layout.pos.row = 1,layout.pos.col = 1))
print(PlotFreq, vp = viewport(layout.pos.row = 1,layout.pos.col = 2))
print(PlotMonetary, vp = viewport(layout.pos.row = 2,layout.pos.col = 1))
print(PlotBreadth, vp = viewport(layout.pos.row = 2,layout.pos.col = 2))
print(PlotTenure, vp = viewport(layout.pos.row = 3,layout.pos.col = 1))
# ------------------------ Create Segments -----------------------------------
RFM_Segs <- data.table(Recency_Week = as.numeric(LastDate - RFM$LastPurchaseDate) %/% 7)
RFM_Segs$Recency <- ordered(ifelse(RFM_Segs$Recency_Week <= 30,"1-30",
                                   ifelse(RFM_Segs$Recency_Week <= 50,"30-50",
                                          ifelse(RFM_Segs$Recency_Week <= 60,"50-60",
                                                 ifelse(RFM_Segs$Recency_Week <= 90,"60-90","90+")))),
                            levels = c("1-30","30-50","50-60","60-90","90+"))

RFM_Segs$Frequency_count <- RFM$NumberOrders
RFM_Segs$Frequency <- ordered(ifelse(RFM_Segs$Frequency <= 1,"0-1",
                                     ifelse(RFM_Segs$Frequency <= 4,"2-4",
                                            ifelse(RFM_Segs$Frequency <= 8,"5-8",
                                                   ifelse(RFM_Segs$Frequency <= 10,"9-10","10+")))),
                              levels = c("0-1","2-4","5-8","9-10","10+"))
RFM_Segs$Monetary_Value <- RFM$TotalAmount
RFM_Segs$Monetary <- ordered(ifelse(RFM_Segs$Monetary_Value <= 0,"0",
                                     ifelse(RFM_Segs$Monetary_Value <= 1500,"0-1.5K",
                                            ifelse(RFM_Segs$Monetary_Value <= 5000,"1.5K-5K",
                                                   ifelse(RFM_Segs$Monetary_Value <= 10000,"5K-10K","10K+")))),
                              levels = c("0","0-1.5K","1.5K-5K","5K-10K","10K+"))


RFM_Segs$Breadth_count <- RFM$NumberSKUs
RFM_Segs$Breadth <-  ordered(ifelse(RFM_Segs$Breadth_count <= 1,"1","2"),
                             levels = c("1","2"))

RFM_Segs$Tenure_weeks <- as.numeric(LastDate - RFM$FirstPurchaseDate) %/% 7
RFM_Segs$Tenure <- ordered(ifelse(RFM_Segs$Monetary_Value <= 2,"0-2",
                                  ifelse(RFM_Segs$Monetary_Value <= 10,"2-10",
                                         ifelse(RFM_Segs$Monetary_Value <= 75,"10-75",
                                                ifelse(RFM_Segs$Monetary_Value <= 110,"75-110","110+")))),
                           levels = c("0-2","2-10","10-75","75-110","110+"))

#------------------------ More Plots -------------------------------------------
P1 <- ggplot(RFM_Segs, aes(Recency,Frequency))
P1 <- P1 +  geom_point(aes(size = Monetary_Value,colour = 'pink'))
print(P1)
