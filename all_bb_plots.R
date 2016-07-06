setwd("C:/Users/Nathaniel Brown/workspace/BECR/bb_models")


load("bb_1_resid_fat.RData")
plot_model(bb_1_resid_fat)




pdf("taking_by_person_bb.pdf", height = 100 )
par(mfrow = c(8, 2))

for( i in 1:length(names)){
  print(i)
  mod <- names[i]
  do <- paste("load('", mod, ".RData')" , sep="")
  eval(parse(text=do))
  do <- paste( "plot_model(", mod, ")", sep = "")
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

