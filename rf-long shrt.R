library(class)
library(lubridate)
library(caret)
library(quantstrat)
library(randomForest)
library(ROSE)
library(plyr)
library(quantmod)

getSymbols.FI(Symbols='ESM7',
              dir='/Users/josephaultman/Strategy/Strategies/RData',
              from='2017-04-10', 
              to='2017-04-13')
TAQD <- ESM7

TAQD$d <- day(index(TAQD))
TAQD$h <- hour(index(TAQD))
TAQD$m <- minute(index(TAQD))
TAQD$s <- second(index(TAQD))

# moving average lengths
.fast = 5
.slow = 18

#TAQD$VOI = TAQD$bidVol - TAQD$askVol
#TAQD$VOIa = round(EMA(TAQD$VOI, n=5),0) - round(EMA(TAQD$VOI, n=18),0)
#TAQD$VOIr = round(EMA(ifelse(TAQD$bidVol==0, 0, TAQD$bidVol / (TAQD$bidVol + TAQD$askVol)),n=30),2)
TAQD$BidEMA = round(EMA(TAQD$AvgBidSz,n=.fast) - EMA(TAQD$AvgBidSz,n=.slow),0)
TAQD$SellEMA = round(EMA(TAQD$askVol,n=.fast) - EMA(TAQD$askVol,n=.slow),0)
TAQD$AskEMA = round(EMA(TAQD$AvgAskSz,n=.fast) - EMA(TAQD$AvgAskSz,n=.slow),0)
TAQD$BuyEMA = round(EMA(TAQD$bidVol,n=.fast) - EMA(TAQD$bidVol,n=.slow),0)
TAQD$VOIb = TAQD$BuyEMA - (TAQD$SellEMA + TAQD$AskEMA)
TAQD$VOIs = TAQD$SellEMA - (TAQD$BuyEMA + TAQD$BidEMA)
#TAQD$R20 = stats::lag(Hi(TAQD),-18) - stats::lag(Op(TAQD),-1)
#TAQD$R10 = stats::lag(Hi(TAQD),-5) - stats::lag(Op(TAQD),-1)
#TAQD$R5 = stats::lag(Hi(TAQD),-3) - stats::lag(Op(TAQD),-1)
TAQD$R = stats::lag(Cl(TAQD),-.slow) - stats::lag(Op(TAQD),-1)

TAQD$MFE_Long = rollapply(Hi(TAQD), FUN=max, width=.slow, align="left") - stats::lag(Op(TAQD),-1)
TAQD$MAE_Long = rollapply(Lo(TAQD), FUN=min, width=.slow, align="left") - stats::lag(Op(TAQD),-1) - 0.25
TAQD$MFR_Long = TAQD$MFE_Long/abs(TAQD$MAE_Long)

TAQD$MFE_Shrt = rollapply(Lo(TAQD), FUN=min, width=.slow, align="left") - stats::lag(Op(TAQD),-1)
TAQD$MFA_Shrt = rollapply(Hi(TAQD), FUN=max, width=.slow, align="left") - stats::lag(Op(TAQD),-1) + 0.25
TAQD$MFR_Shrt = TAQD$MFE_Shrt/abs(TAQD$MFA_Shrt)

TAQD <- TAQD['T08:45/T15:45']

d <- coredata(TAQD$d)
h <- coredata(TAQD$h)
m <- coredata(TAQD$m)
s <- coredata(TAQD$s)

voib <-  coredata(TAQD$VOIb)
voib1 <- Lag(voib,1)
voib2 <- Lag(voib,2)
voib3 <- Lag(voib,3)
voib4 <- Lag(voib,4)

vois <-  coredata(TAQD$VOIs)
vois1 <- Lag(vois,1)
vois2 <- Lag(vois,2)
vois3 <- Lag(vois,3)
vois4 <- Lag(vois,4)

bid <-  coredata(TAQD$BidEMA)
bid1 <- Lag(bid,1)
bid2 <- Lag(bid,2)
bid3 <- Lag(bid,3)
bid4 <- Lag(bid,4)

ask <-  coredata(TAQD$AskEMA)
ask1 <- Lag(ask,1)
ask2 <- Lag(ask,2)
ask3 <- Lag(ask,3)
ask4 <- Lag(ask,4)

bvol <-  coredata(TAQD$BuyEMA)
bvol1 <- Lag(bvol,1)
bvol2 <- Lag(bvol,2)
bvol3 <- Lag(bvol,3)
bvol4 <- Lag(bvol,4)

svol <-  coredata(TAQD$SellEMA)
svol1 <- Lag(svol,1)
svol2 <- Lag(svol,2)
svol3 <- Lag(svol,3)
svol4 <- Lag(svol,4)

avgbsz <- coredata(TAQD$AvgBSz)
avgbsz1 <- Lag(avgbsz,1)
avgbsz2 <- Lag(avgbsz,2)
avgbsz3 <- Lag(avgbsz,3)
avgbsz4 <- Lag(avgbsz,4)

avgasz <- coredata(TAQD$AvgASz)
avgasz1 <- Lag(avgasz,1)
avgasz2 <- Lag(avgasz,2)
avgasz3 <- Lag(avgasz,3)
avgasz4 <- Lag(avgasz,4)

r1 <- coredata(TAQD$MFR_Long)
r2 <- coredata(TAQD$MFR_Shrt)
r <- coredata(TAQD$R)

# y1 <- as.factor(ifelse(r1 > 0.00, "B","S"))
# y2 <- as.factor(ifelse(r2 > 0.25, "B","S"))
# y3 <- as.factor(ifelse(r3 > 0.50, "B","S"))

y1 <- as.factor(ifelse(r1 > 2.50, "B","H"))
y2 <- as.factor(ifelse(r2 < -2.50, "S","H"))

pxCat <- function(x) {
  zero <- ifelse(x == 0, 4, 0)
  postive <- ifelse(x > 0, ifelse(x > 0.50, 1, ifelse(x == 0.50, 2, 3)), 0)
  negative <- ifelse(x < 0, ifelse(x < -0.50, 7, ifelse(x == -0.50, 6, 5)), 0)
  return(as.factor(zero + postive + negative))
}

y3 <- pxCat(r2)
table(y1)
table(y2)
table(y3)

df <- data.frame(y1,y2,y3,
                 #ophi,ophi1,ophi2,
                 #oplo,oplo1,oplo2,
                 voib,voib1,voib2,voib3,voib4,
                 vois,vois1,vois2,vois3,vois4,
                 #voia,voia1,voia2,
                 #voir,voir1,voir2,
                 bid,bid1,bid2,bid3,bid4,
                 ask,ask1,ask2,ask3,ask4,
                 bvol,bvol1,bvol2,bvol3,bvol4,
                 svol,svol1,svol2,svol3,svol4,
                 avgbsz,avgbsz1,avgbsz2,avgbsz3,avgbsz4,
                 avgasz,avgasz1,avgasz2,avgasz3,avgasz4,
                 h,m,s,d,r)

df <- na.omit(df)

colnames(df) <- c("y1","y2","y3",
                  #"ophi","ophi1","ophi2",
                  #"oplo","oplo1","oplo2",
                  "voib","voib1","voib2","voib3","voib4",
                  "vois","vois1","vois2","vois3","vois4",
                  #"voia","voia1","voia2",
                  #"voir","voir1","voir2",
                  "bid","bid1","bid2","bid3","bid4",
                  "ask","ask1","ask2","ask3","ask4",
                  "bvol","bvol1","bvol2","bvol3","bvol4",
                  "svol","svol1","svol2","svol3","svol4",
                  "avgbsz","avgbsz1","avgbsz2","avgbsz3","avgbsz4",
                  "avgasz","avgasz1","avgasz2","avgasz3","avgasz4",
                  "h","m","s","d","r")

bound <- floor((nrow(df)/5)*3)          #define % of training and test set

df.train <- df[1:bound,c(-1,-2,-3)]     #get training set
cl.train1 <- df[1:bound,1]
cl.train2 <- df[1:bound,2]
cl.train3 <- df[1:bound,3]

df.test <- df[(bound+1):nrow(df),c(-1,-2,-3)]  #get test set
cl.test1 <- df[(bound+1):nrow(df),1] 
cl.test2 <- df[(bound+1):nrow(df),2]
cl.test3 <- df[(bound+1):nrow(df),3]

table(cl.train1)
df_balanced_over1 <- ovun.sample(cl.train1~.,data=data.frame(cl.train1,df.train),method="both",seed=1)$data
table(df_balanced_over1$cl.train1)
df.train1 <- df_balanced_over1[,-1]
cl.train1 <- df_balanced_over1[,1]

table(cl.train2)
df_balanced_over2 <- ovun.sample(cl.train2~.,data=data.frame(cl.train2,df.train),method="both",seed=1)$data
table(df_balanced_over2$cl.train2)
df.train2 <- df_balanced_over2[,-1]
cl.train2 <- df_balanced_over2[,1]

set.seed(1)
# FeatureNumber <- tuneRF(x = df.train1[1:33], y = cl.train1,
#                         ntreeTry = 100,stepFactor = 1.5, improve = 0.01,
#                         trace = TRUE, plot = TRUE, dobest = FALSE)


t1 <- Sys.time()
rf1 <- randomForest(x = df.train1[1:43], y = cl.train1, mtry=5, ntree=1501, keep.forest=TRUE, importance=TRUE) 
rf2 <- randomForest(x = df.train2[1:43], y = cl.train2, mtry=5, ntree=1501, keep.forest=TRUE, importance=TRUE)
#rf3 <- randomForest(x = df.train[1:33], y = cl.train3, mtry=5, ntree=1501, keep.forest=TRUE, importance=TRUE)

varImpPlot(rf1)
varImpPlot(rf2)
#varImpPlot(rf3)
#plot(varImp(rf1), main = "Top 15 most influencial Predictors", top = 15)
#varImp(rf1)

results1 <- predict(rf1, df.test)
results2 <- predict(rf2, df.test)
#results3 <- predict(rf3, df.test)

t2 <- Sys.time()
##########################################################
print(t2-t1)
##########################################################

results <- data.frame(obs=cl.test1, pred=results1, 
                      obs=cl.test2, pred=results2,
                      #obs=cl.test3, pred=results3,
                      d=df.test$d, h=df.test$h, m=df.test$m, s=df.test$s, r=df.test$r)

roc.curve(cl.test1, results1, plotit = F)
roc.curve(cl.test2, results1, plotit = F)

colnames(results) <- c("obs1","pred1",
                       "obs2","pred2",
                       #"obs3","pred3",
                       "d","h","m","s","r")

trades_long <- subset(results, pred1 == "B" & pred2 == "H")
trades_shrt <- subset(results, pred2 == "S" & pred1 == "H")

ret_long <- as.double(sum(trades_long$r)) 
ret_shrt <- as.double(sum(trades_shrt$r)) * -1

t_long <- as.double(nrow(trades_long))
t_shrt <- as.double(nrow(trades_shrt))

p_long <- sum(trades_long$r > 0)
p.pct_long <- as.double(p_long/t_long)
p_shrt <- sum(trades_shrt$r < 0)
p.pct_shrt <- as.double(p_shrt/t_shrt)

n_long <- sum(trades_long$r < 0)
n.pct_long <- as.double(n_long/t_long)
n_shrt <- sum(trades_shrt$r > 0)
n.pct_shrt <- as.double(n_shrt/t_shrt)

z_long <- sum(trades_long$r == 0)
z.pct_long <- as.double(z_long/t_long)
z_shrt <- sum(trades_shrt$r == 0)
z.pct_shrt <- as.double(z_shrt/t_shrt)

cat("# Long Trades: ",t_long,"\n",
    "Return: ",ret_long,"\n",
    "Positive: ",p.pct_long,"\n",
    "Negative: ",n.pct_long,"\n",
    "Zero: ",z.pct_long,"\n",
    "Model Accuracy: ",mean(trades_long$pred1==trades_long$obs1),"\n",
    "OoS Hit Rate: ",mean(cl.test1=="B"))

cat("# Short Trades: ",t_shrt,"\n",
    "Return: ",ret_shrt,"\n",
    "Positive: ",p.pct_shrt,"\n",
    "Negative: ",n.pct_shrt,"\n",
    "Zero: ",z.pct_shrt,"\n",
    "Model Accuracy: ",mean(trades_shrt$pred2==trades_shrt$obs2),"\n",
    "OoS Hit Rate: ",mean(cl.test2=="S"))



mean(results$pred1==results$obs1)
mean(results$pred2==results$obs2)
mean(results$pred3==results$obs3)

mean(tradeB$pred1==tradeB$obs1)
mean(tradeS$pred2==tradeS$obs2)
mean(trades$pred3==trades$obs3)
