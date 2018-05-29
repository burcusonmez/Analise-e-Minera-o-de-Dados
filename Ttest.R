
library(ggplot2)
library(magrittr)
library(ggpubr)


glibc_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/glibc_data_balanced.csv", stringsAsFactors = FALSE)

httpd_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/httpd_data_balanced.csv", stringsAsFactors = FALSE)

kernel_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/kernel_data_balanced.csv",stringsAsFactors = FALSE)

mozillal_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/mozilla_data_balanced.csv", stringsAsFactors = FALSE)

xen_balanced <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/random_undersampling/xen_data_balanced.csv",stringsAsFactors = FALSE)


glibc <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/glibc_data.csv",stringsAsFactors = FALSE)

httpd <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/httpd_data.csv", stringsAsFactors = FALSE)

kernel <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/kernel_data.csv",stringsAsFactors = FALSE)

mozillal <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/mozilla_data.csv", stringsAsFactors = FALSE)

xen <- read.csv("/home/hyuri/Dropbox/graduação/mineracao/vunelability/unbalanced/xen_data.csv",stringsAsFactors = FALSE)


feature_cols <- c('AltCountLineCode', 'CountInput', 'CountLineBlank', 'CountLineCodeDecl', 'CountLineComment',
                  'CountLinePreprocessor', 'CountPath', 'CountStmt', 'CountStmtEmpty', 'Cyclomatic', 'CyclomaticStrict',
                  'Knots', 'MinEssentialKnots', 'RatioCommentToCode', 'AltCountLineComment', 'CountLine', 'CountLineCode',
                  'CountLineCodeExe', 'CountLineInactive', 'CountOutput', 'CountSemicolon', 'CountStmtDecl',
                  'CountStmtExe', 'CyclomaticModified', 'Essential', 'MaxEssentialKnots', 'MaxNesting')


#===========================================glibc=========================================================================
for (i in feature_cols){
  balanced <- glibc_balanced[,i]
  
  unbalanced <-glibc[,i]
  
  my_data <- data.frame( 
    group = c(rep("balanced",each=length(balanced)), rep("unbalanced",each=length(unbalanced))),
    weight = c(balanced,  unbalanced)
  )
  print(t.test(unbalanced, balanced))
  ttest <- t.test(unbalanced, balanced)
  ggboxplot(my_data, x = "group", y = "weight", 
            color = "group", palette = c("#8B008B", "#40E0D0"),
            order = c("balanced", "unbalanced"),
            ylab = "Weight", xlab = "Groups", title = paste(i ," p-value=", ttest$p.value))
  
  ggsave(paste(i,".png"))
}

#====================httpd==========================================================================

for (i in feature_cols){
  balanced <- httpd_balanced[,i]
  
  unbalanced <- httpd[,i]
  
  my_data <- data.frame( 
    group = c(rep("balanced",each=length(balanced)), rep("unbalanced",each=length(unbalanced))),
    weight = c(balanced,  unbalanced)
  )
  print(t.test(unbalanced, balanced))
  ttest <- t.test(unbalanced, balanced)
  ggboxplot(my_data, x = "group", y = "weight", 
            color = "group", palette = c("#8B008B", "#40E0D0"),
            order = c("balanced", "unbalanced"),
            ylab = "Weight", xlab = "Groups", title = paste(i ," p-value=", ttest$p.value))
  
  ggsave(paste(i,".png"))
}

#===============================KERNEL-==============================================================

for (i in feature_cols){
  balanced <- kernel_balanced[,i]
  
  unbalanced <- kernel[,i]
  
  my_data <- data.frame( 
    group = c(rep("balanced",each=length(balanced)), rep("unbalanced",each=length(unbalanced))),
    weight = c(balanced,  unbalanced)
  )
  print(t.test(unbalanced, balanced))
  ttest <- t.test(unbalanced, balanced)
  ggboxplot(my_data, x = "group", y = "weight", 
            color = "group", palette = c("#8B008B", "#40E0D0"),
            order = c("balanced", "unbalanced"),
            ylab = "Weight", xlab = "Groups", title = paste(i ," p-value=", ttest$p.value))
  
  ggsave(paste(i,".png"))
}

#============================mozilla==================================================================

for (i in feature_cols){
  balanced <- mozillal_balanced[,i]
  
  unbalanced <- mozillal[,i]
  
  my_data <- data.frame( 
    group = c(rep("balanced",each=length(balanced)), rep("unbalanced",each=length(unbalanced))),
    weight = c(balanced,  unbalanced)
  )
  print(t.test(unbalanced, balanced))
  ttest <- t.test(unbalanced, balanced)
  ggboxplot(my_data, x = "group", y = "weight", 
            color = "group", palette = c("#8B008B", "#40E0D0"),
            order = c("balanced", "unbalanced"),
            ylab = "Weight", xlab = "Groups", title = paste(i ," p-value=", ttest$p.value))
  
  ggsave(paste(i,".png"))
}

#====================================xen=========================================================
for (i in feature_cols){
  balanced <- xen_balanced[,i]
  
  unbalanced <-xen[,i]
  
  my_data <- data.frame( 
    group = c(rep("balanced",each=length(balanced)), rep("unbalanced",each=length(unbalanced))),
    weight = c(balanced,  unbalanced)
  )
  print(t.test(unbalanced, balanced))
  ttest <- t.test(unbalanced, balanced)
  ggboxplot(my_data, x = "group", y = "weight", 
            color = "group", palette = c("#8B008B", "#40E0D0"),
            order = c("balanced", "unbalanced"),
            ylab = "Weight", xlab = "Groups", title = paste(i ," p-value=", ttest$p.value))
  
  ggsave(paste(i,".png"))
}


#============================================================================
# Regression Tree Example
library(rpart)


str(rpart)
# grow tree 
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree for Mileage ")

