# Final Project car price prediction
#install.packages('dummies_1.5.6.tar.gz')
#install.packages('corrplot')

#install.packages('gbm')
#install.packages('neuralnet')
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(forcats)
library('dummies')
library(fastDummies)
library(corrplot)
library(neuralnet)
library(randomForest)

library(gbm)

#clear varaibles
rm(list=ls())
car.df=read.csv('archive/Car details v3.csv')
head(car.df)


#Data exploration and Data cleaning 
car.df$name=sapply(strsplit(car.df$name," "),'[',1)
#View(car.df)

#count of car brands
palette <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(car.df$name))) #color

#bar plot
ggplot(data=car.df, aes(x=fct_infreq(name),fill = name))+
  geom_bar(color='black',size=0.3) + 
  #scale_fill_manual(values=palette)+
  labs(x='Car_Brand',
       y='Count',
       title= 'Car Brands - Bar Graph') +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
      #  panel.grid.major = element_line(color = "gray", linetype = "dashed"))



#coverting car name into ordinal encoding 
brands=unique(car.df$name)
numbers <- 0:(length(brands) - 1)

for (i in seq_along(brands)){
  car.df$name=str_replace(car.df$name,brands[i],as.character(numbers[i]))
}
car.df$name=as.numeric(car.df$name)
#View(car.df)



#Data cleaning - resolving blank values with average values 

#identify blank or na columns 
blank_or_na=car.df==""| is.na(car.df)
colSums(blank_or_na)
car.df$mileage[car.df$mileage=='']=NA
car.df$engine[car.df$engine=='']=NA
car.df$max_power[car.df$max_power=='']=NA
car.df$torque[car.df$torque=='']=NA
blank_or_na=is.na(car.df)
colSums(blank_or_na) #final is.na values

#data transformation
# car mileage
car.df$mileage=str_replace_all(car.df$mileage,'kmpl|km/kg','')
car.df$mileage=as.numeric(car.df$mileage)
car.df$mileage[is.na(car.df$mileage)]=mean(car.df$mileage,na.rm = TRUE)
#View(car.df)

#car engine
car.df$engine=str_replace(car.df$engine,'CC','')
car.df$engine=as.numeric(car.df$engine)
car.df$engine[is.na(car.df$engine)]=mean(car.df$engine, na.rm = TRUE)
#View(car.df)

#max_power
car.df$max_power=str_replace(car.df$max_power,'bhp','')
car.df$max_power=as.numeric(car.df$max_power)
car.df$max_power[is.na(car.df$max_power)]=mean(car.df$max_power, na.rm = TRUE)
#View(car.df)

#seats 
car.df$seats=as.numeric(car.df$seats)
car.df$seats[is.na(car.df$seats)]=median(car.df$seats,na.rm = TRUE)
car.df=subset(car.df,select = -torque)



#plotting categorical values - check

#fuel 
ggplot(data = car.df, aes(x=fct_infreq(fuel),fill = fuel))+
          scale_fill_manual(values=c('orange','blue','red','green'))+
         geom_bar() +labs(x='Fuel', title = 'Fuel - Bar Graph')+
          theme(axis.text.x = element_text(angle = 90,hjust=1))

#seller type
ggplot(data=car.df, aes(x=fct_infreq(seller_type), fill= seller_type))+
  scale_fill_manual(values=c('green','blue','red'))+
  geom_bar()+labs(x='Seller Type', title='Seller Type - Bar Graph')+
  theme(axis.text.x = element_text(angle = 90,hjust=1))

#owner
ggplot(data = car.df, aes(x=fct_infreq(owner),fill=owner))+
  scale_fill_manual(values=c('green','blue','red','orange','lightblue'))+
  geom_bar() +labs(x='Owner', title = 'Owner - Bar Graph')+
  theme(axis.text.x = element_text(angle = 90,hjust=1))

#seats
palette <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique(car.df$seats))) #color
ggplot(data=car.df, aes(x=fct_infreq(as.factor(seats)),fill = seats))+
  geom_bar()+labs(x='Seats', title = 'Seats - Bar Graph') + 
  theme(axis.text.x = element_text(angle = 90,hjust=1))


#convert to dummies 
#View(car.df)
car.df.cat=dummy_cols(car.df,select_columns = c('owner','transmission','fuel','seller_type'), remove_selected_columns = TRUE)
#View(car.df.cat)

#check for selling price distribution
ggplot(data=car.df.cat,aes(x=selling_price))+
  geom_histogram(bins = 50,fill='blue',color='black')+
  geom_density(alpha=0.2,fill='red')+
  labs(
    title = 'Selling Price - Histogram',
    x='Selling Price'
  )
  #scale_x_continuous(trans = 'log10')

#check the distribution for KM driven 
ggplot(car.df.cat,aes(x=km_driven))+
  geom_histogram(color='black',fill='blue',bins=100)+
  labs(x='KM_driven',title = 'KM_Driven Histogram Graph')
  #scale_x_continuous(trans = 'log10')



#box plots
#KM driven
box_stats=boxplot(car.df.cat$km_driven,
        ylim=c(0,300000),
        main='KM driven- Boxplot',
        ylab='KM Driven',
        col='lightblue'
        )
q1 <- box_stats$stats[2]
median <- box_stats$stats[3]
q3 <- box_stats$stats[4]
text(x = 1.2, y = q1, labels = paste("Q1:", round(q1, 2)), col = "blue", pos = 4)
text(x = 1.2, y = median, labels = paste("Median:", round(median, 2)), col = "red", pos = 3.5)
text(x = 1.2, y = q3, labels = paste("Q3:", round(q3, 2)), col = "green", pos = 4)
#selling price
box_stats=boxplot(car.df.cat$selling_price,
                  ylim=c(0,2000000),
                  main='selling price- Boxplot',
                  ylab='selling price',
                  col='lightblue'
)
q1 <- box_stats$stats[2]
median <- box_stats$stats[3]
q3 <- box_stats$stats[4]
text(x = 1.2, y = q1, labels = paste("Q1:", round(q1, 2)), col = "blue", pos = 4)
text(x = 1.2, y = median, labels = paste("Median:", round(median, 2)), col = "red", pos = 3.5)
text(x = 1.2, y = q3, labels = paste("Q3:", round(q3, 2)), col = "green", pos = 4)

#mileage
box_stats=boxplot(car.df.cat$mileage,
                  main='Mileage- Boxplot',
                  ylab='mileage',
                  col='lightblue'
)
q1 <- box_stats$stats[2]
median <- box_stats$stats[3]
q3 <- box_stats$stats[4]
text(x = 1.2, y = q1, labels = paste("Q1:", round(q1, 2)), col = "blue", pos = 4)
text(x = 1.2, y = median, labels = paste("Median:", round(median, 2)), col = "red", pos = 3.5)
text(x = 1.2, y = q3, labels = paste("Q3:", round(q3, 2)), col = "green", pos = 4)

#engine
box_stats=boxplot(car.df.cat$engine,
                  main='engine CC- Boxplot',
                  ylab='engine',
                  col='lightblue'
)
q1 <- box_stats$stats[2]
median <- box_stats$stats[3]
q3 <- box_stats$stats[4]
text(x = 1.2, y = q1, labels = paste("Q1:", round(q1, 2)), col = "blue", pos = 4)
text(x = 1.2, y = median, labels = paste("Median:", round(median, 2)), col = "red", pos = 3.5)
text(x = 1.2, y = q3, labels = paste("Q3:", round(q3, 2)), col = "green", pos = 4)

#Power
box_stats=boxplot(car.df.cat$max_power,
                  main='Power BHP- Boxplot',
                  ylim=c(0,200),
                  ylab='power',
                  col='lightblue'
)
q1 <- box_stats$stats[2]
median <- box_stats$stats[3]
q3 <- box_stats$stats[4]
text(x = 1.2, y = q1, labels = paste("Q1:", round(q1, 2)), col = "blue", pos = 4)
text(x = 1.2, y = median, labels = paste("Median:", round(median, 2)), col = "red", pos = 3.5)
text(x = 1.2, y = q3, labels = paste("Q3:", round(q3, 2)), col = "green", pos = 4)





#correlation 
corrplot(cor(car.df.cat), type="full", 
         method ="color", title = "Correlation Plot", 
         mar=c(0,0,1,0), tl.cex= 0.8, outline= T, tl.col="black")
#needs to check if we categorical values or nominal values are better
# remove seat 14 as it has no correlation with any other variables
#car.df.cat=subset(car.df.cat,select = -seats_14)

#write to csv for further use 
#write.csv(car.df.cat,file='cleaned_car_data.csv',row.names = FALSE)





#Model building Linear Regression 
# without normalization  
set.seed(5)
train_index=sample(1:nrow(car.df.cat),size=0.6*nrow(car.df.cat))
car.df.train=car.df.cat[train_index,]
car.df.valid=car.df.cat[-train_index,]
dim(car.df.train)


#Linear Regression
start_time=Sys.time()
m1_lr=lm(selling_price~., data=car.df.train)
end_time=Sys.time()
summary(m1_lr)
#plot(m1_lr)

#predict selling price on test data
pred_lr_valid=predict(m1_lr,newdata = car.df.valid)
pred_lr_train=predict(m1_lr,newdata = car.df.train)
error_lr_valid=car.df.valid$selling_price -pred_lr_valid
error_lr_train=car.df.train$selling_price -pred_lr_train

#RMSE
RMSE_train=sqrt((mean(error_lr_train^2)))
RMSE_valid=sqrt((mean(error_lr_valid^2)))
#MAE
mae_lr_valid=mean(abs(error_lr_valid))
mae_lr_train=mean(abs(error_lr_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_lr_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_lr_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)


qqnorm(pred_lr_valid, main = 'Q-QPlot - linear regression valid')
qqline(pred_lr_valid,col='red')
qqnorm(pred_lr_train, main = 'Q-QPlot - linear regression train')
qqline(pred_lr_train,col='red')

rmse_df = data.frame(
  Algorithm = c("Linear Regression"),
  Train_RMSE = c(RMSE_train),
  Valid_RMSE = c(RMSE_valid),
  Train_MAE = c(mae_lr_train),
  Valid_MAE = c(mae_lr_valid),
  'R2-Train' = c(r_squared_lr_train),
  'R2-Valid' = c(r_squared_lr_valid),
  Run_Time=end_time-start_time
)
print(rmse_df)

#Plotting predicted vs actual values 
options(scipen = 999)
plot(car.df.valid$selling_price,pred_lr_valid, main="Scatterplot Linear Regression", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")
#View(car.df.valid)



#with normalization 
car.df.norm=as.data.frame(sapply(car.df.cat,scale))
write.csv(car.df.norm,file='car_cleaned_data_normalized.csv',row.names = FALSE)
car.df.train.norm=car.df.norm[train_index,]
car.df.valid.norm=car.df.norm[-train_index,]

#View(car.df.train.norm)
start_time=Sys.time()
m1_lr=lm(selling_price~., data=car.df.train.norm)
end_time=Sys.time()
summary(m1_lr)
#plot(m1_lr)

#upscaling the Price valuewhich was normalized
mean_price = mean(car.df.cat$selling_price)
sd_price = sd(car.df.cat$selling_price)

unscale.price <- function(scaled.price) {
  unscaled = scaled.price * sd_price + mean_price
  return(unscaled)
}

#predict selling price on test data
pred_lr1_valid=predict(m1_lr,newdata = car.df.valid.norm)
pred_lr1_train=predict(m1_lr,newdata = car.df.train.norm)

unscaled_valid <- unscale.price(pred_lr1_valid)
unscaled_train <- unscale.price(pred_lr1_train)

error_lr_valid=car.df.valid$selling_price -unscaled_valid
error_lr_train=car.df.train$selling_price -unscaled_train
RMSE_train=sqrt((mean(error_lr_train^2)))
RMSE_valid=sqrt((mean(error_lr_valid^2)))
#MAE
mae_lr_valid=mean(abs(error_lr_valid))
mae_lr_train=mean(abs(error_lr_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_lr_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_lr_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)

qqnorm(pred_lr_valid, main = 'Q-QPlot - linear regression normalized valid')
qqline(pred_lr_valid,col='red')
qqnorm(pred_lr_train, main = 'Q-QPlot - linear regression normalized train')
qqline(pred_lr_train,col='red')

rmse_df = rbind(rmse_df,
                data.frame(
  Algorithm = c("Linear Regression normalized"),
  Train_RMSE = c(RMSE_train),
  Valid_RMSE = c(RMSE_valid),
  Train_MAE = c(mae_lr_train),
  Valid_MAE = c(mae_lr_valid),
  'R2-Train' = c(r_squared_lr_train),
  'R2-Valid' = c(r_squared_lr_valid),
  Run_Time=end_time-start_time
))
print(rmse_df)

#Plotting predicted vs actual values 
options(scipen = 999)
plot(car.df.valid$selling_price,unscaled_valid, main="Scatterplot Normalized Linear Regression", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")















#model 2 neural network 3 hidden layer nodes 
colnames(car.df.train.norm) <- gsub(" |_&_", "_", colnames(car.df.train.norm))
colnames(car.df.valid.norm) <- gsub(" |_&_", "_", colnames(car.df.valid.norm))
colnames(car.df.train.norm) <- gsub(" |_&_", "_", colnames(car.df.train.norm))
colnames(car.df.valid.norm) <- gsub(" |_&_", "_", colnames(car.df.valid.norm))
colnames(car.df.train) <- gsub(" |_&_", "_", colnames(car.df.train))
colnames(car.df.valid) <- gsub(" |_&_", "_", colnames(car.df.valid))
colnames(car.df.train) <- gsub(" |_&_", "_", colnames(car.df.train))
colnames(car.df.valid) <- gsub(" |_&_", "_", colnames(car.df.valid))
summary(car.df.train.norm)
start_time=Sys.time()
nn = neuralnet(selling_price ~ 
                 name + year + km_driven + mileage + engine + max_power +
                 seats + 
                 owner_First_Owner + owner_Fourth_Above_Owner + owner_Second_Owner + 
                 owner_Test_Drive_Car + owner_Third_Owner + 
                 transmission_Automatic + transmission_Manual + 
                 fuel_CNG + fuel_Diesel + fuel_LPG + fuel_Petrol + 
                 seller_type_Dealer + seller_type_Individual + seller_type_Trustmark_Dealer, 
               data = car.df.train.norm, 
               hidden = 3,
               stepmax = 1e+06)
end_time=Sys.time()

#print(end_time-start_time)
plot(nn,rep = 'best')

#upscaling the Price valuewhich was normalized
mean_price = mean(car.df.cat$selling_price)
sd_price = sd(car.df.cat$selling_price)

unscale.price <- function(scaled.price) {
  unscaled = scaled.price * sd_price + mean_price
  return(unscaled)
}
#predict selling price on test data
train.predict=neuralnet::compute(nn,car.df.train.norm)
unscaled_train=unscale.price(train.predict$net.result[,1])
valid.predict=neuralnet::compute(nn,car.df.valid.norm)
unscaled_valid=unscale.price(valid.predict$net.result[,1])


error_lr_valid=car.df.valid$selling_price -unscaled_valid
error_lr_train=car.df.train$selling_price -unscaled_train
RMSE_train=sqrt((mean(error_lr_train^2)))
RMSE_valid=sqrt((mean(error_lr_valid^2)))
#MAE
mae_lr_valid=mean(abs(error_lr_valid))
mae_lr_train=mean(abs(error_lr_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_lr_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_lr_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)


rmse_df = rbind(rmse_df,
                data.frame(
                  Algorithm = c("Neural network - 3 hidden layers"),
                  Train_RMSE = c(RMSE_train),
                  Valid_RMSE = c(RMSE_valid),
                  Train_MAE = c(mae_lr_train),
                  Valid_MAE = c(mae_lr_valid),
                  'R2-Train' = c(r_squared_lr_train),
                  'R2-Valid' = c(r_squared_lr_valid),
                  Run_Time=end_time-start_time
                ))
print(rmse_df)

#Plotting predicted vs actual values 
options(scipen = 999)
plot(car.df.valid$selling_price,unscaled_valid, main="Scatterplot neural network with 3 hidden layers", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")




#model 2 neural network 5 hidden layer nodes 
colnames(car.df.train.norm) <- gsub(" |_&_", "_", colnames(car.df.train.norm))
colnames(car.df.valid.norm) <- gsub(" |_&_", "_", colnames(car.df.valid.norm))
colnames(car.df.train.norm) <- gsub(" |_&_", "_", colnames(car.df.train.norm))
colnames(car.df.valid.norm) <- gsub(" |_&_", "_", colnames(car.df.valid.norm))
colnames(car.df.train) <- gsub(" |_&_", "_", colnames(car.df.train))
colnames(car.df.valid) <- gsub(" |_&_", "_", colnames(car.df.valid))
colnames(car.df.train) <- gsub(" |_&_", "_", colnames(car.df.train))
colnames(car.df.valid) <- gsub(" |_&_", "_", colnames(car.df.valid))
summary(car.df.train.norm)
start_time=Sys.time()
nn = neuralnet(selling_price ~ 
                 name + year + km_driven + mileage + engine + max_power +
                 seats + 
                 owner_First_Owner + owner_Fourth_Above_Owner + owner_Second_Owner + 
                 owner_Test_Drive_Car + owner_Third_Owner + 
                 transmission_Automatic + transmission_Manual + 
                 fuel_CNG + fuel_Diesel + fuel_LPG + fuel_Petrol + 
                 seller_type_Dealer + seller_type_Individual + seller_type_Trustmark_Dealer, 
               data = car.df.train.norm, 
               hidden = 5,
               stepmax = 1e+06)
end_time=Sys.time()

#print(end_time-start_time)
plot(nn,rep = 'best')

#upscaling the Price valuewhich was normalized
mean_price = mean(car.df.cat$selling_price)
sd_price = sd(car.df.cat$selling_price)

unscale.price <- function(scaled.price) {
  unscaled = scaled.price * sd_price + mean_price
  return(unscaled)
}
#predict selling price on test data
train.predict=neuralnet::compute(nn,car.df.train.norm)
unscaled_train=unscale.price(train.predict$net.result[,1])
valid.predict=neuralnet::compute(nn,car.df.valid.norm)
unscaled_valid=unscale.price(valid.predict$net.result[,1])


error_lr_valid=car.df.valid$selling_price -unscaled_valid
error_lr_train=car.df.train$selling_price -unscaled_train
RMSE_train=sqrt((mean(error_lr_train^2)))
RMSE_valid=sqrt((mean(error_lr_valid^2)))
#MAE
mae_lr_valid=mean(abs(error_lr_valid))
mae_lr_train=mean(abs(error_lr_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_lr_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_lr_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)


rmse_df = rbind(rmse_df,
                data.frame(
                  Algorithm = c("Neural network- 5 hidden layers"),
                  Train_RMSE = c(RMSE_train),
                  Valid_RMSE = c(RMSE_valid),
                  Train_MAE = c(mae_lr_train),
                  Valid_MAE = c(mae_lr_valid),
                  'R2-Train' = c(r_squared_lr_train),
                  'R2-Valid' = c(r_squared_lr_valid),
                  Run_Time=end_time-start_time
                ))
print(rmse_df)

#Plotting predicted vs actual values 
options(scipen = 999)
plot(car.df.valid$selling_price,unscaled_valid, main="Scatterplot neural network with 3 hidden layers", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")














#model 3 Random Forest
start_time=Sys.time()
m3_random_forest=randomForest(selling_price~., data=car.df.train)
end_time=Sys.time()
plot(m3_random_forest)
varImpPlot(m3_random_forest, main='Feature Importance')

#upscale the values


#prediction for test and train 
pred_rf_valid=predict(m3_random_forest,car.df.valid)
error_rf_valid=car.df.valid$selling_price - pred_rf_valid
RMSE_valid=sqrt(mean(error_rf_valid^2))

pred_rf_train=predict(m3_random_forest,car.df.train)
error_rf_train=car.df.train$selling_price - pred_rf_train
RMSE_train=sqrt(mean(error_rf_train^2))

#MAE
mae_lr_valid=mean(abs(error_rf_valid))
mae_lr_train=mean(abs(error_rf_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_rf_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_rf_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)


rmse_df = rbind(rmse_df,
                data.frame(
                  Algorithm = c("Random forest"),
                  Train_RMSE = c(RMSE_train),
                  Valid_RMSE = c(RMSE_valid),
                  Train_MAE = c(mae_lr_train),
                  Valid_MAE = c(mae_lr_valid),
                  'R2-Train' = c(r_squared_lr_train),
                  'R2-Valid' = c(r_squared_lr_valid),
                  Run_Time=end_time-start_time
                ))
print(rmse_df)



#for normalized values 
start_time=Sys.time()
m3_random_forest=randomForest(selling_price~., data=car.df.train.norm)
end_time=Sys.time()
plot(m3_random_forest)
varImpPlot(m3_random_forest, main='Feature Importance normalized')


#prediction for test and train 
pred_rf_valid=predict(m3_random_forest,car.df.valid.norm)
unscaled_valid=unscale.price(pred_rf_valid)
error_rf_valid=car.df.valid$selling_price - unscaled_valid
RMSE_valid=sqrt(mean(error_rf_valid^2))

pred_rf_train=predict(m3_random_forest,car.df.train.norm)
unscaled_train=unscale.price(pred_rf_train)
error_rf_train=car.df.train$selling_price - unscaled_train
RMSE_train=sqrt(mean(error_rf_train^2))

#MAE
mae_lr_valid=mean(abs(error_rf_valid))
mae_lr_train=mean(abs(error_rf_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_rf_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_rf_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)

rmse_df = rbind(rmse_df,
                data.frame(
                  Algorithm = c("Random forest normalized"),
                  Train_RMSE = c(RMSE_train),
                  Valid_RMSE = c(RMSE_valid),
                  Train_MAE = c(mae_lr_train),
                  Valid_MAE = c(mae_lr_valid),
                  'R2-Train' = c(r_squared_lr_train),
                  'R2-Valid' = c(r_squared_lr_valid),
                  Run_Time=end_time-start_time
                ))
print(rmse_df)

#scatter plot
plot(car.df.valid$selling_price,unscaled_valid, main='Scatterplot random forest',col=c('red','blue'), xlab = 'Actual Selling Price', ylab='Predicted selling Price')




#View(car.df.cat.train)
#model gradient boosting 
set.seed(123)
start_time=Sys.time()
m4_gbm=gbm(
  formula=selling_price ~.,
  distribution = 'gaussian',
  data = car.df.train,
  n.trees = 6000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL,
  verbose = FALSE
)
end_time=Sys.time()
m4_gbm

#plot loss function as a result of n trees 
gbm.perf(m4_gbm,method = 'cv')

#variance importance 
summary(
  m4_gbm,
  cBars = 15,
  method = relative.influence,
  las=2
)

#prediction 


#metrics
#prediction for test and train 
pred_rf_valid=predict(m4_gbm,car.df.valid)
#unscaled_valid=unscale.price(pred_rf_valid)
error_rf_valid=car.df.valid$selling_price - pred_rf_valid
RMSE_valid=sqrt(mean(error_rf_valid^2))

pred_rf_train=predict(m4_gbm,car.df.train)
#unscaled_train=unscale.price(pred_rf_train)
error_rf_train=car.df.train$selling_price - pred_rf_train
RMSE_train=sqrt(mean(error_rf_train^2))

#MAE
mae_lr_valid=mean(abs(error_rf_valid))
mae_lr_train=mean(abs(error_rf_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_rf_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_rf_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)

rmse_df = rbind(rmse_df,
                data.frame(
                  Algorithm = c("Gradient Boosting"),
                  Train_RMSE = c(RMSE_train),
                  Valid_RMSE = c(RMSE_valid),
                  Train_MAE = c(mae_lr_train),
                  Valid_MAE = c(mae_lr_valid),
                  'R2-Train' = c(r_squared_lr_train),
                  'R2-Valid' = c(r_squared_lr_valid),
                  Run_Time=end_time-start_time
                ))
print(rmse_df)

#plotting predicted vs actual 
plot(car.df.valid$selling_price,unscaled_valid,main='scatterplot',col=c('red','blue'),xlab='Actual selling price',ylab='Predicted selling price')



# gradient boosting normalized
set.seed(123)
start_time=Sys.time()
m4_gbm=gbm(
  formula=selling_price ~.,
  distribution = 'gaussian',
  data = car.df.train.norm,
  n.trees = 6000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL,
  verbose = FALSE
)
end_time=Sys.time()
m4_gbm

#plot loss function as a result of n trees 
gbm.perf(m4_gbm,method = 'cv')

#variance importance 
summary(
  m4_gbm,
  cBars = 15,
  method = relative.influence,
  las=2
)

#prediction 


#metrics
#prediction for test and train 
pred_rf_valid=predict(m4_gbm,car.df.valid.norm)
unscaled_valid=unscale.price(pred_rf_valid)
error_rf_valid=car.df.valid$selling_price - unscaled_valid
RMSE_valid=sqrt(mean(error_rf_valid^2))

pred_rf_train=predict(m4_gbm,car.df.train.norm)
unscaled_train=unscale.price(pred_rf_train)
error_rf_train=car.df.train$selling_price - unscaled_train
RMSE_train=sqrt(mean(error_rf_train^2))

#MAE
mae_lr_valid=mean(abs(error_rf_valid))
mae_lr_train=mean(abs(error_rf_train))
#R-squared 
sst_lr=sum((car.df.valid$selling_price-mean(car.df.valid$selling_price))^2)
sse_lr=sum(error_rf_valid^2)
r_squared_lr_valid=1-(sse_lr/sst_lr)
sst_lr=sum((car.df.train$selling_price-mean(car.df.train$selling_price))^2)
sse_lr=sum(error_rf_train^2)
r_squared_lr_train=1-(sse_lr/sst_lr)

rmse_df = rbind(rmse_df,
                data.frame(
                  Algorithm = c("Gradient Boosting normalized"),
                  Train_RMSE = c(RMSE_train),
                  Valid_RMSE = c(RMSE_valid),
                  Train_MAE = c(mae_lr_train),
                  Valid_MAE = c(mae_lr_valid),
                  'R2-Train' = c(r_squared_lr_train),
                  'R2-Valid' = c(r_squared_lr_valid),
                  Run_Time=end_time-start_time
                ))
print(rmse_df)


