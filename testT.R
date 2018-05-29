
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


for (i in feature_cols){
  balanced <- mozilla_balanced[,i]
  
  unbalanced <- mozilla[,i]
  
  my_data <- data.frame( 
    group = c(rep("balanced",each=length(balanced)), rep("unbalanced",each=length(unbalanced))),
    weight = c(balanced,  unbalanced)
  )
  print(t.test(unbalanced, balanced))
  ttest <- t.test(unbalanced, balanced)
  
  
  
  ggboxplot(my_data, x = "group", y = "weight", 
            color = "group", palette = c("#00AFBB", "#E7B800"),
            order = c("balanced", "unbalanced"),
            ylab = "Weight", xlab = "Groups", title = paste(i ," p-value=", ttest$p.value))
  
  ggsave(paste(i,".png"))
}
