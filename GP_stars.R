require(kernlab)
require(caret)
require(ggthemes)

# Read data
spec_jplus <- read.csv("/Users/Rafael/Downloads/DR10MP_Params_spec.csv")

spec_cut <- spec_jplus[,c("SPEC_ELO","gLup","rLup","iLup","zLup","F395Lup","F410Lup","F430Lup",
                          "F515Lup","F660Lup","F861Lup")]

spec_complete<-spec_cut[complete.cases(spec_cut),]

# Binning of the classes
library(plyr)
spec_complete$SPEC_ELO<-revalue(spec_complete$SPEC_ELO, c("A0"="A", "A0p"="A","B6"="B","B9"="B","CAR" = "C","Carbo"="C",
                                  "CV"="C","CWD"="C","F2"="F","F5"="F","F9"="F",
                                  "G0"="G","G2"="G","G5"="G","K1"="K","K3"="K",
                                  "K5"="K","K7"="K","L4"="L","L5"="L","L5.5"="L","L9"="L",
                                  "M0"="M", "M0V"="M", "M1"="M", "M2"="M", "M2V"="M", "M3"="M",
                                  "M4"="M", "M5"="M", "M6"="M", "M7"="M", "M8"="M", "M9"="M", "OB"="O",
                                  "T2"="T","WD"="W","WDmag"= "W"))
spec_complete_new <- droplevels(spec_complete[-which(spec_complete$SPEC_ELO %in% c("STARF","00")),])


inTrain <- createDataPartition(y = spec_complete_new$SPEC_ELO,
                               ## the outcome data are needed
 p = .01,
## The percentage of data in the
 ## training set
list = FALSE)

train <- spec_complete_new[inTrain,]

train_scaled<-as.data.frame(scale(train[,-1]))


pc<- princomp(train_scaled[,-1],cor=TRUE, scores=TRUE) 



out <- data.frame(x= pc$scores[,1], y = pc$scores[,2], class = train[,1])

ggplot(out,aes(x=x,y=y,colour=class))+geom_point()+
  xlab("PC1")+ylab("PC2")+theme_hc()+scale_color_stata()


library(rgl)
plot3d(pc$scores[,1:3], col=as.numeric(train[,1]),xlim=c(-5,5),ylim=c(-2,3.5),
       zlim=c(-0.3,0.5))





test <- gausspr(SPEC_ELO~.,data=train,var=2)
test
alpha(test)
# predict on the training set
predict(test,iris[,-5])
# class probabilities
predict(test, iris[,-5], type="probabilities")