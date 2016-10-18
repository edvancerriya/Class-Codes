## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
df <- data.frame(
  x = c(1,2,3,2,3,4.5,5.5,6.5, 5.5,6.5),
  y=c(1,1,1,3,3,5,5,5,7,7),
  color=as.factor(c(1,1,1,2,2,2,2,2,1,1))
)
p=ggplot(df,aes(x,y,color=color))
 q= theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
 p+geom_point(size=10)+q

## ----echo=FALSE----------------------------------------------------------
p+
geom_rect(aes(xmin=0.50,xmax=3.50,ymin=0.50,ymax=3.5),color="blue") + 
geom_rect(aes(xmin=4,xmax=7,ymin=4.50,ymax=7.5),color="blue") +geom_point(size=10)+
  q

## ----echo=FALSE----------------------------------------------------------
p+
geom_rect(aes(xmin=0.50,xmax=3.50,ymin=0.50,ymax=1.75),color="blue") + 
   geom_rect(aes(xmin=0.50,xmax=3.50,ymin=2.25,ymax=3.5),color="blue")+
geom_rect(aes(xmin=4,xmax=7,ymin=4.50,ymax=5.5),color="blue") +
   geom_rect(aes(xmin=4,xmax=7,ymin=6.25,ymax=7.5),color="blue")+geom_point(size=10)+
  q

## ----message=FALSE,warning=FALSE-----------------------------------------
library(tree)
library(ISLR)
library(dplyr)

## ------------------------------------------------------------------------
mysales=Carseats %>%
   mutate(High=as.factor(ifelse(Sales<=8,"No","Yes"))) %>%
   select(-Sales)

## ------------------------------------------------------------------------
tree.carseats = tree(High~.,data=mysales)

summary(tree.carseats)



## ------------------------------------------------------------------------
plot(tree.carseats)
text(tree.carseats,pretty=0)

## ------------------------------------------------------------------------
tree.carseats

## ------------------------------------------------------------------------
set.seed(2)
train=sample(1:nrow(mysales), 200)
mysales_train=mysales[train,]
mysales_test=mysales[-train,]

## ------------------------------------------------------------------------
tree.carseats=tree(High~.,data=mysales_train)

tree.pred.test=predict(tree.carseats,newdata=mysales_test,type="class")
tree.pred.train=predict(tree.carseats,newdata=mysales_train,type="class")

table(tree.pred.train,mysales_train$High)
18/200
table(tree.pred.test,mysales_test$High)

54/200

plot(tree.carseats)
text(tree.carseats,pretty=0)

## ------------------------------------------------------------------------
set.seed(3)
cv.carseats= cv.tree(tree.carseats,FUN=prune.misclass)

plot(cv.carseats$size,cv.carseats$dev,type="b")

## ------------------------------------------------------------------------
prune.carseats=prune.misclass(tree.carseats,best=9)

plot(prune.carseats)
text(prune.carseats,pretty=0)

## ------------------------------------------------------------------------
prune.carseats

## ------------------------------------------------------------------------
tree.pred=predict(prune.carseats,mysales_test,type="class")
table(tree.pred,mysales_test$High)


46/200

## ------------------------------------------------------------------------
set.seed(3)
train=sample(1:nrow(Carseats),200)
carseats_train=Carseats[train,]
carseats_test=Carseats[-train,]

## ------------------------------------------------------------------------
rt.carseats=tree(Sales~.,data=carseats_train)

plot(rt.carseats)
text(rt.carseats,pretty=0)

## ------------------------------------------------------------------------
rt.carseats

## ------------------------------------------------------------------------
sales_pred_train=predict(rt.carseats,newdata =carseats_train )
sales_pred_test=predict(rt.carseats,newdata=carseats_test)

rmse_train =sqrt(mean((sales_pred_train-carseats_train$Sales)^2))
rmse_test =sqrt(mean((sales_pred_test-carseats_test$Sales)^2))

rmse_train
rmse_test

## ------------------------------------------------------------------------
cv.rt.carseats=cv.tree(rt.carseats)
plot(cv.rt.carseats$size,cv.rt.carseats$dev,type='b')

## ------------------------------------------------------------------------
prune.rt.carseats=prune.tree(rt.carseats,best=6)

plot(prune.rt.carseats)
text(prune.rt.carseats,pretty=0)
prune.rt.carseats

## ------------------------------------------------------------------------
sales_pred_train=predict(prune.rt.carseats,newdata =carseats_train )
sales_pred_test=predict(prune.rt.carseats,newdata=carseats_test)

rmse_train =sqrt(mean((sales_pred_train-carseats_train$Sales)^2))
rmse_test =sqrt(mean((sales_pred_test-carseats_test$Sales)^2))

rmse_train
rmse_test

## ----warning=FALSE,message=FALSE-----------------------------------------

library(dplyr)
ld=read.csv("loans data.csv",stringsAsFactors = FALSE)
ld=ld %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Amount.Funded.By.Investors=as.numeric(Amount.Funded.By.Investors),
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
         )
ld= ld %>%
  mutate(f1=as.numeric(substr(FICO.Range,1,3)),
         f2=as.numeric(substr(FICO.Range,5,7)),
         fico=0.5*(f1+f2)
         ) %>%
  select(-FICO.Range,-f1,-f2)

ld = ld %>%
  select(-Amount.Funded.By.Investors) %>%
  na.omit()

## ------------------------------------------------------------------------
for(i in 1:ncol(ld)){
  if(class(ld[,i])=="character"){
    ld[,i]=as.factor(ld[,i])
  }
}

glimpse(ld)
set.seed(2)
train=sample(1:nrow(ld),0.6*nrow(ld))
ld_train=ld[train,]
ld_test=ld[-train,]


## ----error=TRUE----------------------------------------------------------
ld.tree=tree(Interest.Rate~.-ID,data=ld_train)


## ----error=TRUE----------------------------------------------------------
ld.tree=tree(Interest.Rate~.-ID-State,data=ld_train)
ld.tree

## ------------------------------------------------------------------------
cv.ld.tree=cv.tree(ld.tree)
plot(cv.ld.tree$size,cv.ld.tree$dev,type='b')

## ------------------------------------------------------------------------
sum((ld_test$Interest.Rate-predict(ld.tree,newdata=ld_test))**2) %>%
  sqrt()

### Home Assignment :apply classification tree on the revenue grid case study 


## ----message=FALSE,warning=FALSE-----------------------------------------
library(randomForest)

class_rf=randomForest(High~.,data=mysales_train,do.trace=T)

class_rf


## ------------------------------------------------------------------------
forest.pred=predict(class_rf,newdata=mysales_test)
table(mysales_test$High,forest.pred)

(20+17)/200

### Home Assignment :see if you can imporve this with different values of mtry and ntree

## ------------------------------------------------------------------------
rf_ld=randomForest(Interest.Rate~.-ID-State,data=ld_train)
rf_ld

## ------------------------------------------------------------------------
sum((ld_test$Interest.Rate-predict(rf_ld,newdata=ld_test))**2) %>%
  sqrt()

## ------------------------------------------------------------------------


importance(class_rf)
varImpPlot(class_rf)


## ------------------------------------------------------------------------
importance(rf_ld)
varImpPlot(rf_ld)

### find out what is OOB error rate in context of random forest