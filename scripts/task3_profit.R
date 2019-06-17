if (!require("pacman")) install.packages("pacman")
library(rpart)
library(rpart.plot)

pacman::p_load(readr,corrplot, ggplot2,caret, lattice,party,GGally,
               plotly, reshape)

#Read files
ExistProd <- read.csv("C:/Users/KEYLA/Desktop/Rcsv/existingproductattributes2017.csv")
NewProd <- read.csv("C:/Users/KEYLA/Desktop/Rcsv/newproductattributes2017.csv")
str(ExistProd)

summary(ExistProd)

#En BestSellersRank teneos 15 NA, lo eliminamos porque no podremos imputar los valores 
sum(is.na(ExistProd$BestSellersRank))
ExistProd$BestSellersRank <- NULL
ExistProd$ProductNum <- NULL
# duplicated(ExistProd)


#OUTLIERS ####
names(ExistProd)
name_outlier <- names(ExistProd)
box_plot <- boxplot(ExistProd[, c(name_outlier)])

# exset<- ExistProd[,c("Volume","x4StarReviews", "PositiveServiceReview")]
# exset <- subset(exset, Volume < 7000)
# boxplot(exset)
#
# outliers

boxplot(ExistProd$Volum)
boxplot(ExistProd$Volume)$out
ExistProd <- ExistProd[!ExistProd$Volume > 6000,]
# ExistProd <- ExistProd[which(ExistProd$Volume < 5999),]
# Salen 2 productos con outliers en volumen, que son el 150 y el 198
volume_plot <- plot_ly(ExistProd, x = ~ProductType, y= ~Volume)
volume_plot

# Warranty ####
ExistProd[34:41, "Price"] <- mean(ExistProd[34:41, "Price"])
ExistProd <- ExistProd[-c(35:41),]



#Dummy ####
newdataFrame <- dummyVars("~ .", data=ExistProd)
readyData <- data.frame(predict(newdataFrame, newdata = ExistProd)) 
str(ExistProd)

#Correlation ####
corr_analysis <- cor(readyData)
corrplot(corr_analysis, method = "number",  tl.cex= 0.55, number.cex = 0.53,  type = "lower")
ggcorr(readyData,label = TRUE,label_alpha = TRUE, legend.size = 7 )

# findCorrelation(corr_analysis, cutoff = 0.9, verbose = FALSE, names = TRUE,
#                 exact = TRUE)

Excorr <- subset(ExistProd, select = -c(ProductType,x1StarReviews,x3StarReviews,NegativeServiceReview))
corrData <- cor(Excorr)
corrplot(corrData,method = "number", type = "lower")


ExistProd$x5StarReviews <- NULL
tree <-ctree(Volume~., ExistProd, control = ctree_control(maxdepth = 6))
plot(tree)

#### decision tree ####
exdt <- subset(ExistProd, select = -c(x1StarReviews,x3StarReviews,NegativeServiceReview))
set.seed(100)

decisionTree <- rpart(Volume~ .,
                      data = exdt,
                      control = list(maxdepth = 6))
decisionTree
rpart.plot(decisionTree)

varImp(decisionTree)


#OUTLIER ejemplo####

# OutlierRecognicer <- function(x, y){
# sd <- sd(x)
# med <- median(x)
# out_neg <- c()
# out_pos <- c()
# for (i in 1:length(x)){
#   value <- x[i]
#   if (value - (y * sd) > med){
#     out_pos <- c(out_pos, i)
#   }
#   if(value + (y * sd) < med){
#     out_neg <- c(out_neg, i)
#   }
# }
# print(paste("You have", length(out_neg), "numbers of outliers below the distribution"))
# return(c(out_neg, out_pos))
# }
# 
# outlier <- apply(readyData, 2, function(x){OutlierRecognicer(x,2)})



#set seed####
set.seed(252)

control<-trainControl(method = "repeatedcv", 
                      number = 10,
                      repeats = 2)

#Create data partition ####
in_training <- (createDataPartition(ExistProd$Volume, p=.75 , list=FALSE))
training <- ExistProd[in_training, ]
testing <- ExistProd[-in_training, ]

#Compare models ####
models <- list("knn","svmLinear","rf")
compareModel<- c()
# trainModel1<- list()

for (i in models) {
  fit <- train (Volume~ x4StarReviews + PositiveServiceReview,
                data = training,
                method = i,
                preProc = c("center", "scale"),
                tuneLength = 2,
                trControl =control)
  prediction <- predict(fit, testing)
  predictResample <- postResample(testing$Volume, prediction)
  compareModel <- cbind(predictResample, compareModel)
}

colnames(compareModel) <- models
compareModel

# model_melt <- melt(compareModel)
# model_melt
# 
# ggplot(model_melt, aes(x=X2, y=value))+
#   geom_bar(stat = "identity",aes(fill = X2))+
#   facet_grid(X1~., scales = "free", ) +
#   scale_fill_discrete()

model_melt <- melt(compareModel, varnames = c("metric", "model"))

ggplot(model_melt, aes(x=model, y=value))+
  geom_bar(stat = "identity",aes(fill = model))+
  facet_grid(metric~., scales = "free",) + scale_fill_discrete()

#### final prediction ####
modelknn<- train(Volume~.,data = training, method = "knn", trControl = control, tuneLength = 2, preProcess = c("center", "scale"))
predictionknn <- predict(modelknn, NewProd)

summary(predictionknn)

#### create columns and csv ####
NewProd$Volume <- predictionknn
NewProd$Sales <- NewProd$Price*NewProd$ProfitMargin*NewProd$Volume
str(NewProd)
summary(NewProd)
#Visualizations ####
finalpredictions <- ggplot(NewProd[NewProd$ProductType == "PC" | 
                                     NewProd$ProductType == "Laptop" | 
                                     NewProd$ProductType == "Netbook" | 
                                     NewProd$ProductType == "Smartphone",],
                                 aes(x = ProductType, y = Volume, fill= as.character(ProductNum)))+ 
  geom_bar(stat = "identity") + 
  ggtitle("Sales by each Product Category") +
  ylab("Sales Volume")+
  xlab("Product Type")+
  guides(fill=guide_legend(title="Product Number"))
  # scale_fill_brewer(palette = "Paired")

finalpredictions


#Ejemplo ####

modelKnn<- train(Volume~.,data = training, method = "knn", trControl = control, 
                 tuneLength = 2, preProcess = c("center", "scale"))
finalpredictionKnn<- predict(modelKnn, newdata = testing)

testing$predictionknn <- finalpredictionKnn
testing$absolute.errorKnn <- abs(testing$Volume - testing$predictionknn )
testing$relative.errorKnn <- testing$absolute.errorKnn/testing$Volume

errors <- ggplot( data=testing, aes(x=Volume, y=absolute.errorKnn))+
  geom_smooth()+ geom_point()+ggtitle("Abs errors in Knn")

MetricKnn <- postResample(pred = testing$predictionknn, obs = testing$Volume)

