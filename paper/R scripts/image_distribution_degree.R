list.files()
data_pagerank_degree = read.csv(file = "pagerank_degree.csv",header = TRUE)
#data_degree= data_pagerank_degree[,c(1,5)] 
#data_indegree= data_pagerank_degree[,c(1,3)] 
#data_outdegree= data_pagerank_degree[,c(1,4)] 

data_degree <- with(data_pagerank_degree,table(degree))
data_indegree <- with(data_pagerank_degree,table(indegree))
data_outdegree <- with(data_pagerank_degree,table(outdegree))

pdf(file = "image_distribution_degree.pdf")
opar <- par(no.readonly = TRUE) 
par(mfrow = c(1,3))   #,mar=c(1,3.8,1,3.8),oma=c(2,2,2,2)   ,pin=c(1.33,2)

#par(pin=c(1,1))
par(mar=c(4,3.8,4,1.2),oma=c(12,1,12,1))

plot(as.numeric(names(data_indegree)),as.numeric(data_indegree),xlab="Indegree(log-scale)",ylab="Frequency(log-scale)",type="l",log="xy",lwd=2,col="dark green")

plot(as.numeric(names(data_outdegree)),as.numeric(data_outdegree),xlab="Outegree(log-scale)",ylab="Frequency(log-scale)",type="l",log="xy",lwd=2,col="dark green")

plot(as.numeric(names(data_degree)),as.numeric(data_degree),xlab="Degree(log-scale)",ylab="Frequency(log-scale)",type="l",log="xy",lwd=2,col="dark green")

par(opar)
dev.off()
