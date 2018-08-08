library(muvis)
library(heatmaply)
df <- read.csv("data.csv")
df1 <- readxl::read_xlsx("D Mining Sample NA.xlsx")




test.assoc <- function(data, vars, levels){
  test.pair <- function(var1, var2){
    is.cat <- function(var) {
      !length(unique(var)) > levels
    }
    var1.is.cat <- is.cat(var1)
    var2.is.cat <- is.cat(var2)
    if(!var1.is.cat & !var2.is.cat){
      ct <- cor.test(var1, var2)
      ct <- ct$p.value
    }
    if(var1.is.cat & var2.is.cat){
      ct <- chisq.test(var1, var2)
      ct <- ct$p.value
    }
    if(var1.is.cat & !var2.is.cat){
      ct <- aov(var2 ~ var1, data=data.frame(var1 = var1 , var2 = var2))
      ct <- summary(ct)[[1]][["Pr(>F)"]][1]
    }
    if(!var1.is.cat & var2.is.cat){
      ct <- aov(var1 ~ var2, data=data.frame(var1 = var1 , var2 = var2))
      ct <- summary(ct)[[1]][["Pr(>F)"]][1]
    }
    ct
  }
  to.ret <- matrix(NA, ncol = length(vars), nrow = length(vars))
  for(i in 1:length(vars)){
    for(j in 1:length(vars)){
      if(i == j)
        next 
      to.ret[i,j] <- test.pair(data[,vars[i]], data[,vars[j]])
    }
  }
  to.ret <- p.adjust(to.ret, method = "BH")
  to.ret <- matrix(to.ret, ncol = length(vars), nrow = length(vars))
  rownames(to.ret) <- colnames(to.ret) <- vars
  heat.map <- heatmaply(to.ret) %>% layout(margin = list(l = 130, b = 40))
  list(matrix = to.ret, heatmap = heat.map)
}




test.assoc(df, colnames(df)[1:50], 10) -> corr


