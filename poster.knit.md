---
title: "High Dim Vis _draft"
author: "Nicholas Spyrison"
date: "3 April 2018"
output: html_document
---


[tour](https://en.wikipedia.org/wiki/Grand_Tour_(data_visualisation))
[tour documentation](https://cran.r-project.org/web/packages/tourr/tourr.pdf)
[PCA](https://en.wikipedia.org/wiki/Principal_component_analysis)
[PCA documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/prcomp.html)
[t-SNE](https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding)
[t-SNE documentation](https://cran.r-project.org/web/packages/Rtsne/Rtsne.pdf)

# tourr; visualizing higher dimensions vs alternatives

## Absract
Visualizing in higher (greater than p=3 numeric dimensions) can be messy and unintuitive. Here we explain the methodology and explore the functionality of tourr. We offer a vignette for use and contrast with other higher dimensional visualization methods. 

The R package, tourr (2011, Wickham, H., D. Cook), gives us the means to animate the projection as we rotate though p-dimensions. This is achieved by varying the contributions from each dimension, via random walk, predefined path, or optimizing an index.


### sources
Wickham, H., D. Cook, and H. Hofmann (2015). Visualising statistical models: Removing the blindfold (withdiscussion). Statistical Analysis and Data Mining 8(4), 203–225.

Wickham, H., D. Cook, H. Hofmann, and A. Buja (2011). tourr: An r package for exploring multivariate data withprojections. Journal of Statistical Software 40(2), http://www.jstatsoft.org/v40.

Asimov D (1985). “The Grand Tour: A Tool for Viewing Multidimensional Data.” SIAM
Journal of Scientific and Statistical Computing, 6(1), 128–143.

## High Dimensional Data

Let 'high dimensional' refer to any data set with more than 3 numeric variables. Dimensions in this definition are rarely space or time. Such multivariate data are still interpretable in mathematics and numbers, but their visualization is not trivial. 



### flea Data Set
6 measurements taken between 3 different species of flea-beetles. 74 observations arcoss 6 dimensions and species. Below species controls the color and point type, but is unknown to the methods. Available with the `tourr` package.


<!-- ```{r init} -->
<!-- library(tourr) -->
<!-- f <- flea[, 1:6] -->
<!-- f.col <- rainbow(length(unique(flea$species)))[as.numeric(as.factor(flea$species))] -->
<!-- f.pch <- as.numeric(flea$species)+14 -->


<!-- ### Tourr -->
<!-- f.holes <- save_history(f, guided_tour(index = holes), max_bases = 25) -->
<!-- f.holes_end <- matrix(as.numeric(f.holes[,,dim(f.holes)[3]]),ncol=2) -->

<!-- ### PCA -->
<!-- f.pca <- prcomp(f, center = TRUE, scale. = TRUE) -->
<!-- f.v = round(sum(f.pca[[1]][1:2]) / sum(f.pca[[1]]),3)  -->
<!-- f.main = paste(100*f.v,"% of var~ PC1,2") -->

<!-- plot(f.pca, type = "l") -->
<!-- sum(f.pca[[1]][5:6]) / sum(f.pca[[1]]) #last 2 removes 14.83% of the variation. -->
<!-- sum(f.pca[[1]][1:2]) / sum(f.pca[[1]]) #first 2 contains: 59.25% of the variation. -->
<!-- ``` -->


<!-- ### Comparing PCA with holes tour -->

<!-- ```{r PCA and tour} -->
<!-- par(mfrow=c(1,2)) -->
<!-- plot(f.pca$x, col=f.col, pch=f.pch, xaxt='n',  -->
<!--      yaxt='n', ann=F, space=0) -->
<!-- title(main=f.main, line=1) -->
<!-- segments(0, 0, f.pca$rotation[,1], f.pca$rotation[,2], col="grey50") -->
<!-- text(f.pca$rotation[,1], f.pca$rotation[,2], label = names(f), col = "grey50") -->
<!-- animate(f, start=f.holes_end, max_frames=0, -->
<!--         display = display_xy(pch=f.pch, col=f.col)) -->
<!-- title(main="holes tour", line=1) -->
<!-- par(mfrow=c(1,1)) -->
<!-- ``` -->

<!-- For the most part, PCA 1 and 2 is close to the holes guided tour. We can see from the data and the original axis that the main difference is location of the green and blue species. if we rotate across the x=y diagonal the results are similar. -->


<!-- ### t-SNE -->

<!-- ```{r t-SNE} -->
<!-- library(Rtsne) -->
<!-- #perplexity is k neighbors. -->
<!-- f.tsne <- Rtsne(f, dims = 6, perplexity=15, verbose=TRUE, max_iter = 500)  -->
<!-- colnames(f.tsne$Y) <- paste0("tS",1:6) -->
<!-- plot(f.tsne$Y, col=f.col, pch=f.pch, -->
<!--      main="t-SNE", line=1, xaxt='n', yaxt='n', xlab='', ylab='') -->
<!-- ``` -->

<!-- Can look outstanding and spread groups the furthest! At what cost?  -->

<!-- - Loss of repreducability even after `set.seed(n)`, t-SNE can look completle different every time.  -->
<!-- - Loss of interprebility to the real dimension space! The non-linear manipulation has left us with no map to real space!  -->

<!-- **Non-starter for analysis** -->

<!-- It looks nice, and can use it for GUIs and illustration, more of a graphical display of categorical classification.  -->

<!-- ```{r t-SNE differs} -->
<!-- set.seed(5) -->
<!-- par(mfrow=c(3,3)) -->
<!-- for (i in 1:9) { -->
<!--   f.tsne <- Rtsne(f, dims = 6, perplexity=15, verbose=TRUE, max_iter = 500)  -->
<!--   colnames(f.tsne$Y) <- paste0("tS",1:6) -->
<!--   plot(f.tsne$Y, col=f.col, pch=f.pch, xaxt='n', yaxt='n', ann=FALSE) -->
<!--   title(main=paste("t-SNE ",i)) -->
<!-- } -->
<!-- set.seed(Sys.time()) #random seed -->
<!-- par(mfrow=c(1,1)) -->
<!-- ``` -->

<!-- ### Across method -->

<!-- ```{r across method} -->
<!-- par(mfrow=c(1,3)) -->
<!-- animate(f.pca$x, max_frames=0,  -->
<!--         display = display_xy(pch=f.pch, col=f.col)) -->
<!-- title(main="PCA1,2", line=1) -->
<!-- animate(f.tsne$Y, max_frames=0,  -->
<!--         display = display_xy(pch=f.pch, col=f.col)) -->
<!-- title(main="t-SNE1,2", line=1) -->
<!-- animate(f, start=f.holes_end, max_frames=0, -->
<!--         display = display_xy(pch=f.pch, col=f.col)) -->
<!-- title(main="holes tour", line=1) -->
<!-- par(mfrow=c(1,1)) -->
<!-- ``` -->

<!-- Side-by-side representation across methodology. Results from a single frame from `tourr::animate()` for comparison. Also note PCA and t-SNE have 6 dim of data. PCA has implicit order; descending variation. Viewing `pairs()` or `tourr::animate()` quickly shows us that t-SNE1,2 is not the optimal 2D projection. We could put t_SNE through PCA or the holes tour to reorient. -->




