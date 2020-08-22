links_group = read.csv("coreimages_network_crossspots.csv",header = TRUE)
library(ggplot2)
head(links_group)
pdf(file = "sub_coreimages/typepairs_linkstrength.pdf", width = 10, height = 6)
typepairs_linktimes_plot <- ggplot(links_group, aes(x = type.pairs,y = linktimes)) + 
  scale_x_discrete(limits=c("OS-OS","OS-LR","OS-TS","OS-AF","LR-LR","LR-TS","LR-AF","TS-TS","TS-AF","AF-AF")) + geom_boxplot()+ labs(x="",y="linktimes(log-scale)")
typepairs_linktimes_plot + scale_y_log10()
dev.off()





res <- kruskal.test(linktimes ~ type.pairs, data = links_group)
res

source("http://www.statmethods.net/RiA/wmc.txt")
wmc(linktimes ~ type.pairs, data = links_group, method = "holm")




library(effsize)
res <- cliff.delta(linktimes ~ type.pairs, data = links_group)
res








cliff <- function(formula, data, exact=FALSE, sort=TRUE){
  
  # setup
  df <- model.frame(formula, data)
  y <- df[[1]]
  x <- as.factor(df[[2]])
  
  
  # reorder levels of x by median y
  if(sort){
    medians <- aggregate(y, by=list(x), FUN=median)[2]
    index <- order(medians)
    x <- factor(x, levels(x)[index])
  }
  
  groups <- levels(x)
  k <- length(groups)
  
  # summary statistics
  stats <- function(z)(c(N = length(z), Median = median(z), MAD = mad(z)))
  sumstats <- t(aggregate(y, by=list(x), FUN=stats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  cat("Descriptive Statistics\n\n")
  print(sumstats)
  
  # multiple comparisons
  mc <- data.frame(Group.1=character(0), 
                   Group.2=character(0), 
                   W=numeric(0),
                   p.unadj=numeric(0), 
                   p=numeric(0),
                   stars=character(0),
                   stringsAsFactors=FALSE)
  
  # perform Wilcoxon test
  row <- 0
  for(i in 1:k){
    for(j in 1:k){
      if (j > i){
        row <- row + 1
        y1 <- y[x==groups[i]]
        y2 <- y[x==groups[j]] 
        test <- cliff.delta(y1, y2)
        print(test)
        mc[row,1] <- groups[i]
        mc[row,2] <- groups[j]
        #mc[row,3] <- test$chi-squared
        #mc[row,4] <- test$p$
        mc[row,3] <- test
      }
    }
  }

  print(mc[-4], right=TRUE)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  return(invisible(NULL))
  
}











wt_linktimes <- kruskal.test(linktimes ~ type, data = links_group)
wt_linktimes







wmc2(linktimes ~ type.pairs, data = links_group)
wmc2




test = read.csv("test.csv",header = TRUE)
library(ggplot2)
head(test)


wt_rate22 <-cliff.delta(rate ~ type, data = test)
wt_rate22



wmc2(rate ~ type, data = image_group_noother)



