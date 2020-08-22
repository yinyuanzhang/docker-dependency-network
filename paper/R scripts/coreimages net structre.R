image_group = read.csv("coreimages net structre star.csv",header = TRUE)
library(ggplot2)
head(image_group)
pdf(file = "coreimages_net_structre_avg.length.pdf")
avg.length_plot <- ggplot(image_group, aes(x = type,y = avg.length)) + 
  scale_x_discrete(breaks=c("Operating Systems","Language Runtime","Tools/Services","Application Infrastructure/Frameworks","Other"),
                   labels=c("OS","Language Runtime","Tools/Services","Application I/F","Other")) + geom_boxplot()+ labs(x="",y="avg.length(log-scale)")
avg.length_plot + scale_y_log10()
dev.off()



pdf(file = "coreimages_net_structre_diameter.pdf")
ggplot(image_group, aes(x = type,y = diameter)) + 
  scale_x_discrete(breaks=c("Operating Systems","Language Runtime","Tools/Services","Application Infrastructure/Frameworks","Other"),
                    labels=c("OS","Language Runtime","Tools/Services","Application I/F","Other")) + geom_boxplot()+ labs(x="")
dev.off()



pdf(file = "coreimages_net_structre_avg.edge.pdf")
ggplot(image_group, aes(x = type,y = avg.edge)) + 
  scale_x_discrete(breaks=c("Operating Systems","Language Runtime","Tools/Services","Application Infrastructure/Frameworks","Other"),
                    labels=c("OS","Language Runtime","Tools/Services","Application I/F","Other")) + geom_boxplot()+ labs(x="")
dev.off()




pdf(file = "coreimages_net_structre_node.pdf")
node_plot <- ggplot(image_group, aes(x = type,y = node)) + 
  scale_x_discrete(breaks=c("Operating Systems","Language Runtime","Tools/Services","Application Infrastructure/Frameworks","Other"),
                    labels=c("OS","Language Runtime","Tools/Services","Application I/F","Other")) + geom_boxplot()+ labs(x="",y="edges_all(log-scale)")
node_plot + scale_y_log10()
dev.off()




pdf(file = "coreimages_net_structre_edges_all.pdf")
edges_all_plot <-  ggplot(image_group, aes(x = type,y = edges_all)) + 
  scale_x_discrete(breaks=c("Operating Systems","Language Runtime","Tools/Services","Application Infrastructure/Frameworks","Other"),
                    labels=c("OS","Language Runtime","Tools/Services","Application I/F","Other")) + geom_boxplot()+ labs(x="",y="edges_all(log-scale)")
edges_all_plot + scale_y_log10()
dev.off()



pdf(file = "coreimages_net_structre_edge.pdf")
edge_plot <- ggplot(image_group, aes(x = type,y = edge)) + 
  scale_x_discrete(breaks=c("Operating Systems","Language Runtime","Tools/Services","Application Infrastructure/Frameworks","Other"),
                   labels=c("OS","Language Runtime","Tools/Services","Application I/F","Other")) + geom_boxplot()+ labs(x="",y="edge(log-scale)")
edge_plot + scale_y_log10()
dev.off()





