image_group = read.csv("subimages net structre.csv",header = TRUE)
library(ggplot2)
head(image_group)
pdf(file = "subimages net structre.pdf")
ggplot(image_group, aes(x = type, y = pathlength)) + 
scale_x_discrete(breaks=c("Operating Systems","Programming Languages","Other","DevOps Tools","Databases",
"Cross-Images","Base Images","Application Services","Application Infrastructure","Application Frameworks","Analytics"),
labels=c("OS","Programming\nlanguage","Other","DevOps\nTools","Databases","Cross-\nImages","Base \nImages","AS","AI","AF","Analytics")) + geom_boxplot()
dev.off()
