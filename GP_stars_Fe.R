#The MIT License (MIT)

#Copyright (c) 2017 Rafael S. de Souza

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:
  
#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#THE SOFTWARE.

# Generalized Linear Models applied to J-PLUS dataset
# Goal--Compare SDDS-like vs J-PLUS bands for physical parameters estimation

# required libraries
require(ggplot2);require(scales);library(caret);library(reshape);library(e1071);library(scales);library(MASS);
library(mgcv);require(ggthemes);require(visreg)

###### Set seed 42 the answer to Life, Universe and Everything Else
set.seed(42)
###### Read, clean and format data
spec_jplus <- read.csv("/Users/Rafael/Downloads/DR10MP_Params_spec.csv")

spec_cut <- spec_jplus[,c("FEHB","gLup","rLup","iLup","zLup","F395Lup","F410Lup","F430Lup",
                          "F515Lup","F660Lup","F861Lup")]
spec_cut <- spec_cut[which(spec_cut$FEHB!=-9999.000),]
spec_complete<-spec_cut[complete.cases(spec_cut),]

###### Sumsampling  for test 
inTrain <- sample(seq_len(nrow(spec_complete)),replace=F, size = 5000)
train <- spec_complete[inTrain,]


r <- 30 # smooth parameter for GAM
###### Fit the SDSS-like bands

fit_SDSS        <- gam(FEHB ~ s(gLup,bs="cr",k=r)         + s(rLup,bs="cr",k=r)      + s(iLup,bs="cr",k=r)+
                          s(zLup,bs="cr",k=r),
                        data=train) 

###### Fit the J-PLUS like bands
fit_SPLUS       <- gam(FEHB  ~ s(gLup,bs="cr",k=r)         + s(rLup,bs="cr",k=r)      + s(iLup,bs="cr",k=r)+
                         s(zLup,bs="cr",k=r) + s(F395Lup,bs="cr",k=r)+s(F410Lup,bs="cr",k=r)+
                         s(F430Lup,bs="cr",k=r)+s(F515Lup,bs="cr",k=r)+s(F660Lup,bs="cr",k=r)+s(F861Lup,bs="cr",k=r),
                       data=train) 

visreg(fit_SPLUS)
plot(fit_SPLUS ,pages=1,residuals=F,scheme=1,rug=FALSE,lwd=3,shade=TRUE,seWithMean=TRUE) 
gam.check(fit_SPLUS) # Residual analysis
gam.check(fit_SDSS)
BIC(fit_SDSS)
BIC(fit_SPLUS)
anova(fit_SDSS,fit_SPLUS,test="Chi")


# Compare to original data
pred_SDSS <- predict(fit_SDSS,type="response",se.fit = TRUE)
pred_SPLUS <- predict(fit_SPLUS,type="response",se.fit = TRUE)

# Combining data

g_comp <- data.frame(train$FEHB,SDSS=pred_SDSS$fit,SPLUS=pred_SPLUS$fit)
g_melt<-melt(g_comp,id="train.FEHB")
colnames(g_melt) <- c("Fe","telescope","value")


p1 <- ggplot(g_melt,aes(x=Fe,y=value,color=telescope,fill=telescope))


p1+stat_density2d(geom="polygon",aes(fill =telescope,alpha=..level..),na.rm = TRUE,n = 250,contour = TRUE) +
  xlab(expression(paste("[",Fe/H,beta,"]",sep="")["Obs"]))+ylab(expression(paste("[",Fe/H,beta,"]",sep="")["Pred"]))+
  geom_abline(intercept = 0,linetype="dashed",color="gray20")+theme(legend.text = element_text(colour="gray40"),
 legend.title=element_blank(),text = element_text(size=20),
  legend.position=c(0.1,0.75),axis.line = element_line(color = 'black'))+
  theme_bw()+
  scale_alpha(guide="none",range=c(0,0.45))+scale_color_fivethirtyeight()+
  scale_fill_fivethirtyeight()




