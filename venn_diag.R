


## The code that generates Venn diagram
library(RColorBrewer)
myCol <- brewer.pal(4, "Pastel2")

library(VennDiagram)
par(mar = c(2, 8, 2, 8))
venn.diagram(
  x = list(svm.math.cat.var.imp[1:10,1], svm.math.reg.var.imp[1:10,1], svm.por.cat.var.imp[1:10,1], svm.por.reg.var.imp[1:10,1]),
  category.names = c("math class" , "math reg" , "por class", "por reg"),
  filename = '#14_venn_diagramm.png',
  output=T,
  
  # Output features
  imagetype="png" ,
  height = 1200 , 
  width = 1200, 
  resolution = 600,
  compression = "lzw",
  
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = myCol,
  
  # Numbers
  cex = .6,
  fontface = "bold",
  fontfamily = "sans",
  margin = 0.2,
  cat.cex = 0.6
)
