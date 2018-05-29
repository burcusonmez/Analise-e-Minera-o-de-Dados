library(corrgram)



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


head(glibc_balanced)
str(glibc_balanced)
head(httpd_balanced)
str(httpd_balanced)
head(kernel_balanced)
str(kernel_balanced)
head(mozillal_balanced)
str(mozillal_balanced)
head(xen_balanced)
str(xen_balanced)

head(glibc)
str(glibc)
head(httpd)
str(httpd)
head(kernel)
str(kernel)
head(mozillal)
str(mozillal)
head(xen)
str(xen)



round(cor(glibc_balanced[,-28,-29], use = "pair"),2)
round(cor(httpd_balanced[,-28,-29], use = "pair"),2)
round(cor(kernel_balanced[,-28,-29], use = "pair"),2)
round(cor(mozillal_balanced[,-28,-29], use = "pair"),2)
round(cor(xen_balanced[,-28,-29], use = "pair"),2)

round(cor(glibc[,-28,-29], use = "pair"),2)
round(cor(httpd[,-28,-29], use = "pair"),2)
round(cor(kernel[,-28,-29], use = "pair"),2)
round(cor(mozillal[,-28,-29], use = "pair"),2)
round(cor(xen[,-28,-29], use = "pair"),2)

feature_glibc <- c("AltCountLineCode","CountInput", "CountLineBlank","CountLineCodeDec", "CountLineComment","CountLinePreprocessor",
                     "CountPath","CountStmt","CountStmtEmpty","Cyclomatic","CyclomaticStrict","Knots", "MinEssentialKnots","RatioCommentToCode",
                     "AltCountLineComment","CountLine","CountLineCode", "CountLineCodeExe","CountLineInactive","CountOutput","CountSemicolon",
                     "CountStmtDecl","CountStmtExe","CyclomaticModified","Essential","MaxEssentialKnots","MaxNesting")

feature_httpd <- c("AltCountLineCode","CountInput", "CountLineBlank","CountLineCodeDec", "CountLineComment","CountLinePreprocessor",
                     "CountPath","CountStmt","CountStmtEmpty","Cyclomatic","CyclomaticStrict","Knots", "MinEssentialKnots","RatioCommentToCode",
                     "AltCountLineComment","CountLine","CountLineCode", "CountLineCodeExe","CountLineInactive","CountOutput","CountSemicolon",
                     "CountStmtDecl","CountStmtExe","CyclomaticModified","Essential","MaxEssentialKnots","MaxNesting")

feature_kernel <- c("AltCountLineCode","CountInput", "CountLineBlank","CountLineCodeDec", "CountLineComment","CountLinePreprocessor",
                      "CountPath","CountStmt","CountStmtEmpty","Cyclomatic","CyclomaticStrict","Knots", "MinEssentialKnots","RatioCommentToCode",
                      "AltCountLineComment","CountLine","CountLineCode", "CountLineCodeExe","CountLineInactive","CountOutput","CountSemicolon",
                      "CountStmtDecl","CountStmtExe","CyclomaticModified","Essential","MaxEssentialKnots","MaxNesting")

feature_mozillal <- c("AltCountLineCode","CountInput", "CountLineBlank","CountLineCodeDec", "CountLineComment","CountLinePreprocessor",
                        "CountPath","CountStmt","CountStmtEmpty","Cyclomatic","CyclomaticStrict","Knots", "MinEssentialKnots","RatioCommentToCode",
                        "AltCountLineComment","CountLine","CountLineCode", "CountLineCodeExe","CountLineInactive","CountOutput","CountSemicolon",
                        "CountStmtDecl","CountStmtExe","CyclomaticModified","Essential","MaxEssentialKnots","MaxNesting")

feature_xen <- c("AltCountLineCode","CountInput", "CountLineBlank","CountLineCodeDec", "CountLineComment","CountLinePreprocessor",
                   "CountPath","CountStmt","CountStmtEmpty","Cyclomatic","CyclomaticStrict","Knots", "MinEssentialKnots","RatioCommentToCode",
                   "AltCountLineComment","CountLine","CountLineCode", "CountLineCodeExe","CountLineInactive","CountOutput","CountSemicolon",
                   "CountStmtDecl","CountStmtExe","CyclomaticModified","Essential","MaxEssentialKnots","MaxNesting")


corrgram( glibc_balanced[,-28,-29], order = TRUE, main = "glibc data balanced",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)

corrgram( httpd_balanced[,-28,-29], order = TRUE, main = "httpd data balanced",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)

corrgram( kernel_balanced[,-28], order = TRUE, main = "kernel data balanced",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)


corrgram( mozillal_balanced[,-28], order = TRUE, main = "mozillal data balanced",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)

corrgram( xen_balanced[,-28], order = TRUE, main = "xen data balanced",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)


corrgram( glibc[,-28,-29], order = TRUE, main = "glibc data ",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)

corrgram( httpd[,-28,-29], order = TRUE, main = "httpd data",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)

corrgram( kernel[,-28,-29], order = TRUE, main = "kernel data ",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)


corrgram( mozillal[,-28,-29], order = TRUE, main = "mozillal data ",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)

corrgram( xen[,-28,-29], order = TRUE, main = "xen data ",lower.panel=panel.shade, upper.panel=panel.pie,
          diag.panel=panel.minmax, text.panel=panel.txt)



