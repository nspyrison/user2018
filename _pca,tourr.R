#NS 30/3/2018.
#examples for higher dim vis.

library(tourr)
f <- flea[, 1:6]
f.col <- rainbow(length(unique(flea$species)))[as.numeric(as.factor(flea$species))]
f.pch <- as.numeric(flea$species)+14
head(f, 3)
##str(iris)#[150, 5] numeric: [,1:4]. factor: [,5]
##str(flea)#[74, 7]  numeric: [,1:6]. factor: [,7]

### Tourr
f.holes <- save_history(f, guided_tour(index = holes), max_bases = 25)
#cmass <- save_history(flea[, 1:6],guided_tour(index = cmass), max_bases = 25)
#lda_pp <- save_history(flea[, 1:6],guided_tour(index = lda_pp(flea[,7])), max_bases = 25)

f.holes_end <- matrix(as.numeric(f.holes[,,dim(f.holes)[3]]),ncol=2) #holes r_space
#matrix(as.numeric(cmass[,,dim(cmass)[3]]),ncol=2) #cmass r_space
#matrix(as.numeric(lda_pp[,,dim(lda_pp)[3]]),ncol=2) #lda_pp r_space



###PCA
#browseURL("https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/")
f.pca <- prcomp(f, center = TRUE, scale. = TRUE)
f.v = round(sum(f.pca[[1]][1:2]) / sum(f.pca[[1]]),3) 
f.main = paste(100*f.v,"% of var~ PC1,2")

#f.pca[[2]] #[6,6] r_space for flea.
#qr.Q(qr(f.pca[[2]])) # validated orthonormal basis.
plot(f.pca, type = "l")
sum(f.pca[[1]][5:6]) / sum(f.pca[[1]]) #last 2 removes 14.83% of the variation.
sum(f.pca[[1]][1:2]) / sum(f.pca[[1]]) #first 2 contains: 59.25% of the variation.
#pareto chart
#plot(0:6,c(0,cumsum(f.pca[[1]][1:6]) / sum(f.pca[[1]])), type = "l",)

?segments
##
par(mfrow=c(1,2))
plot(rescale(f.pca$x), col=f.col, pch=f.pch,
     main=f.main, line=1, xaxt='n', yaxt='n', xlab='')
segments(.5, .5, f.pca$rotation[,1], f.pca$rotation[,2], col="grey50")
text(f.pca$rotation[,1], f.pca$rotation[,2], label = names(f), col = "grey50")
animate(f, start=f.holes_end, max_frames=0,
        display = display_xy(pch=f.pch, col=f.col))
title(main="holes tour", line=1)
par(mfrow=c(1,1))
##


###t-SNE
#browseURL("https://www.analyticsvidhya.com/blog/2017/01/t-sne-implementation-r-python/")
#install.packages("Rtsne")
library(Rtsne)

#perplexity between 5:20. different results every time, even with seed set.
f.tsne <- Rtsne(f, dims = 6, perplexity=15, verbose=TRUE, max_iter = 500)
colnames(f.tsne$Y) <- paste0("tS",1:6)
plot(f.tsne$Y, col=f.col, pch=f.pch,
     main="t-SNE", line=1, xaxt='n', yaxt='n', xlab='')
#Y is the projected t-SNE data.

#We can tour t_sne-space, but have lost interpebility to the original dimensions!
#animate_xy(tsne$Y, col=f.col, pch=f.pch)

##Great, this looks amazing! yet we have lost interprebility to the real dimension space! It looks nice, and can use it for GUIs and illustration, yet more of a graphical display of categorical classification. It can litterally bend and twist dimensions till we have a pretty picture.


##
par(mfrow=c(1,3))
animate(f.pca$x, max_frames=0, 
        display = display_xy(pch=f.pch, col=f.col))
title(main="PCA1,2", line=1)
animate(f.tsne$Y, max_frames=0, 
        display = display_xy(pch=f.pch, col=f.col))
title(main="t-SNE1,2", line=1)
animate(f, start=f.holes_end, max_frames=0,
        display = display_xy(pch=f.pch, col=f.col))
title(main="holes tour", line=1)
par(mfrow=c(1,1))
##


### RANDOM NOISE:
mvn <- matrix(rnorm(900),nrow=100,ncol=9)
colnames(mvn) <- paste0("n",1:9)

#pca
mvn.pca <- prcomp(mvn, center = TRUE, scale. = TRUE)
mvn.v = round(sum(mvn.pca[[1]][1:2]) / sum(mvn.pca[[1]]),3) 
mvn.main = paste(100*mvn.v,"% of var~ PC1,2")
#first 2 pca: .2555 #modest compared to 2/9=.222

#tour
mvn.holes <- save_history(mvn, guided_tour(index = holes), max_bases = 25)
mvn.holes_proj <- matrix(as.numeric(mvn.holes[,,dim(mvn.holes)[3]]),ncol=2)

#t-sne
mvn.tsne <- Rtsne(mvn, dims = 9, perplexity=15, verbose=TRUE, max_iter = 500)
colnames(mvn.tsne$Y) <- paste0("tS",1:9)

##
par(mfrow=c(1,3))
animate(mvn.pca$x, max_frames=0, 
        display = display_xy(pch=20, col=rainbow(9)))
title(main="PCA1,2", line=1)
animate(mvn.tsne$Y, max_frames=0, 
        display = display_xy(pch=20, col=rainbow(9)))
title(main="t-SNE1,2", line=1)
animate(mvn, start=mvn.holes_proj, max_frames=0,
        display = display_xy(pch=20, col=rainbow(9)))
title(main="holes tour", line=1)
par(mfrow=c(1,1))


#Holes tour of PCA
mvn.pca <- prcomp(mvn, center = TRUE, scale. = TRUE)
mvn.pca.holes <- save_history(mvn.pca$x, guided_tour(index = holes), max_bases = 25)
mvn.pca.holes_end <- matrix(as.numeric(mvn.pca.holes[,,dim(mvn.pca.holes)[3]]),ncol=2)

#not 100% sure what this tells us.
par(mfrow=c(1,2))
animate(mvn.pca$x, start=mvn.pca.holes_end, max_frames=0,
        display = display_xy(pch=20, col=rainbow(9)))
title(main="PCA holes tour")
animate(mvn, start=mvn.holes_end, max_frames=0,
        display = display_xy(pch=20, col=rainbow(9)))
title(main="holes tour", line=1)
par(mfrow=c(1,1))

stop()
stop()
###OUTPUT
#Rstudio, snap of the end projection (of holes). cannot pass col, pch into animate. 
animate(f, start=holes_end, max_frames=0,
        display = display_xy(pch=f.pch, col=f.col))
#is this or render to png better?
render(f, start=holes_end, grand_tour(), frames = 1,
       display_xy(pch=f.pch, col=f.col), "png", "test_holes-%03d.png")

#par(mfrow=c(1,1))
#par(mfrow=c(1,2))

