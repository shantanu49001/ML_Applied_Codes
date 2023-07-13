dataset=read.csv('Social_Network_Ads.csv')
#add ke outcomes hai csv file me
#train  odel by ony;a ge and dlary
#depedent 1 and indepe 2 
dataset=dataset[,3:5]

#to make the dep var observable as class and not as contineuous piint
#change the dep var data to 1,0 category
dataset$Purchased=factor(dataset$Purchased,levels = c(0,1))

library(caTools)
set.seed(123)
split=sample.split(dataset$Purchased,SplitRatio = 0.75)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

#for classicifation fs krlo
#dep var categrial hai ony scale rest
#dataest me 3 hi col hai ek dep 2 indep ka
training_set[,1:2]=scale(training_set[,1:2])
test_set[,1:2]=scale(test_set[,1:2])
#genrlsed linear modle #binomial=logistic

#classifier




#reponse means rsults ina vector
#now compare the results from origional
#prob value with th purchas status
prob_pred=predict(classifier,newdata=test_set[-3]) #only dep var cilumns
#y_pred=ifelse(prob_pred>0.5,1,0) #prob pred is also a vector

#confusion matrix
#real and precisted vectors
#correct and incorrct predictions
# 57+26->coorect
#only logistic regre returns prob
cm=table(test_set[,3],y_pred>0.5)
#elemstatlearn pakage istalled 
#warnign folder from the course 
#install.packages('ElemStatLearn')
library('ElemStatLearn')
#plot coe
#set me 2 col 2 dep var 
set=training_set  #chage it to tes set

#age clumn ko expanded 
X1=seq(min(set[,1])-1,max(set[,1])+1,by =0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by =0.01)
#grid build
grid_set=expand.grid(X1,X2)

#grid ke column name set
colnames(grid_set)=c('Age','ESTIMATEDSaalry')
#model
prob_set=predict(classifier,type = 'response',newdata = grid_set)

#edit outpput probabilty->0,1
y_grid=ifelse(prob_set>0.5,1,0)
#plot pijts of input -->leaving the thord col as it is dep var
plot(set[,-3],
     main='Log Regression',
     xlab='Age',
     ylab='Estimated Slalry',
     xlim=range(X1),
     ylim=range(X2))
contour(X1,X2,matrix(as.numeric(y_grid),length(X2)),add=TRUE)
#line
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
#points
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))
#Stright line is the prediction boundary\
#liner classifier 
#if users were also linerly distrubuted it woruld have been perfect 
#reason why we are having some wrong preuctions at the line

#classification templte