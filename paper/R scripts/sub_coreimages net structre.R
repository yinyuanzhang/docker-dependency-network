image_group = read.csv("sub_coreimages net structre.csv",header = TRUE)
library(ggplot2)
head(image_group)




pdf(file = "sub_coreimages/sub_coreimages_netstructre.pdf", width = 15, height = 5)
node_plot <- ggplot(image_group, aes(x = type,y = nodenumber)) + 
  scale_x_discrete(limits=c("OS","LR","T/S","AI/F","Other"),
                   labels=c("OS","LR","T/S","AI/F","Other")) + geom_boxplot()+ labs(x="",y="# Node(log-scale)")
p3 <- node_plot + scale_y_log10()



edge_plot <- ggplot(image_group, aes(x = type,y = edgenumber)) + 
  scale_x_discrete(limits=c("OS","LR","T/S","AI/F","Other"),
                   labels=c("OS","LR","T/S","AI/F","Other")) + geom_boxplot()+ labs(x="",y="# Edge(log-scale)")
p4 <- edge_plot + scale_y_log10()



edges_all_plot <-  ggplot(image_group, aes(x = type,y = all_edgesnumber)) + 
  scale_x_discrete(limits=c("OS","LR","T/S","AI/F","Other"),
                   labels=c("OS","LR","T/S","AI/F","Other")) + geom_boxplot()+ labs(x="",y="# Link(log-scale)")
p5 <- edges_all_plot + scale_y_log10()



p6 <- ggplot(image_group, aes(x = type,y = rate)) + 
  scale_x_discrete(limits=c("OS","LR","T/S","AI/F","Other"),
                   labels=c("OS","LR","T/S","AI/F","Other")) + geom_boxplot()+ labs(x="",y="# Link/# Lode")


library(gridExtra)
grid.arrange(p3,p4,p5,p6,ncol=4)
dev.off()


















wilcox <- function(formula, data, exact=FALSE, sort=TRUE){
  
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
        test <- wilcox.test(y1, y2)
        print(test)
        mc[row,1] <- groups[i]
        mc[row,2] <- groups[j]
        #mc[row,3] <- test$chi-squared
        #mc[row,4] <- test$p$
        mc[row,3] <- test$p.value
      }
    }
  }
  
  print(mc[-4], right=TRUE)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  return(invisible(NULL))
  
}





image_group_noother = read.csv("sub_coreimages net structre noother.csv",header = TRUE)
library(ggplot2)
head(image_group_noother)

            
# wt_nodenumber <- kruskal.test(nodenumber ~ type, data = image_group_noother)
# wt_nodenumber
# source("http://www.statmethods.net/RiA/wmc.txt")
# wmc(nodenumber ~ type, data = image_group_noother, method = "holm")



wt_nodenumber <- kruskal.test(nodenumber ~ type, data = image_group_noother)
wt_nodenumber
wilcox(nodenumber ~ type, data = image_group_noother)
cliff(nodenumber ~ type, data = image_group_noother)


wt_edgenumber <- kruskal.test(edgenumber ~ type, data = image_group_noother)
wt_edgenumber
wilcox(edgenumber ~ type, data = image_group_noother)
cliff(edgenumber ~ type, data = image_group_noother)


wt_all_edgesnumber <- kruskal.test(all_edgesnumber ~ type, data = image_group_noother)
wt_all_edgesnumber
wilcox(all_edgesnumber ~ type, data = image_group_noother)
cliff(all_edgesnumber ~ type, data = image_group_noother)


wt_rate <- kruskal.test(rate ~ type, data = image_group_noother)
wt_rate
wilcox(rate ~ type, data = image_group_noother)
cliff(rate ~ type, data = image_group_noother)










#  ²âÊÔ
test = read.csv("test.csv",header = TRUE)
library(ggplot2)
head(test)

wt_rate22 <-wilcox.test(nodenumber ~ type, data = test)
wt_rate22




