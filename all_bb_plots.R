setwd("~/Desktop/Data+/Models/bb_models")

load("bb_names.RData")

pdf("t.pdf")

par(mfrow = c(8,1))

for( i in 1:length(names)){
  print(i)
  mod <- names[i]
  do <- paste("load('", mod, ".RData')" , sep="")
  eval(parse(text=do))
  do <- paste( "print(plot_model(", mod, ", title = '",mod,  "' ))", sep = "")
  eval(parse(text=do))
  
  do <- paste("rm(",mod,")",sep="")
  eval(parse(text=do))
}

dev.off()



f <- function(x) {((x-1)^2) * exp(-(x^3/3-2*x^2/2+x))}
F <- function(x) {integrate(f,0,x)$value}
F <- Vectorize(F)


quants <- quantile(f(0:5),probs = seq(0,1,0.05))

round(quants,4)
sample(quants, 4)

hist(quants,breaks=20,col="green",prob=T)

