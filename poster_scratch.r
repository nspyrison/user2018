plot(f.pca, type = "l")
library(ggplot2)
library(gridExtra)
library(magrittr)
dat <- as.data.frame(cbind("PCA"=1:6,
                     "Var"=f.pca[[1]], 
                     "Prop_Var"= f.pca[[1]]/sum(f.pca[[1]]),
                     "cumsum_Prop_Var"= cumsum(f.pca[[1]])/sum(f.pca[[1]])
                     ) )

##
gg1 <- ggplot(dat,aes(x=PCA,y=Prop_Var,label=round(Prop_Var,3))) + 
  geom_point() + geom_line() + ggtitle("~ pmf, mass = Var") + 
  geom_text(nudge_x = 0.3, nudge_y = .01) +
  geom_ribbon(data=dat[-1, ], aes(x=PCA, ymin=0, ymax=Prop_Var), 
              fill = "red", alpha = .2)
gg2 <- ggplot(dat,aes(x=PCA,y=cumsum_Prop_Var,label=round(cumsum_Prop_Var,3))) +
  geom_point() + geom_line() + ggtitle("~ cmf, mass = Var") + 
  geom_text(nudge_x = 0.3) +
  geom_ribbon(data=dat[-1, ], aes(x=PCA, ymin=cumsum_Prop_Var, ymax=1), 
              fill = "red", alpha = .2)
gridExtra::grid.arrange(gg1, gg2, ncol = 2)

## PCA
dat <- as.data.frame(f.pca[[5]]) %>% cbind("Species" = flea[,7])
(gg3 <- ggplot(dat, aes(x = PC1, y = PC2, shape = Species, col = Species)) +
    geom_point() + ggtitle("PC1 by PC2 across Species"))

## t-SNE
library(Rtsne)
#perplexity is knn
f.tsne <- Rtsne(f, dims = 6, perplexity=15, verbose=TRUE, max_iter = 500) 
#~.9 sec for [74x6]
colnames(f.tsne$Y) <- paste0("tS",1:6)
#plot(f.tsne$Y, col=f.col, pch=f.pch,
#     main="t-SNE", line=1, xaxt='n', yaxt='n', xlab='', ylab='')
### t-SNE is not inherently ordered, so let's do PCA on it.

f.tsne.pca <- stats::prcomp(f.tsne$Y, center = TRUE, scale. = TRUE)
colnames(f.tsne.pca$x) <- paste0("tSNE_PC",1:6)
dat <- as.data.frame(f.tsne.pca$x) %>% cbind("Species" = flea[,7])
(gg4 <- ggplot(dat, aes(x = tSNE_PC1, y = tSNE_PC2, 
                        shape = Species, col = Species)) +
    geom_point() + ggtitle("t-SNE PC1 by PC2 across Species"))

## tourr
library(tourr)
f.col <- rainbow(length(unique(flea$species)))[as.numeric(as.factor(flea$species))]
f.pch <- as.numeric(flea$species)+14

f.holes <- save_history(f, guided_tour(index = holes), max_bases = 25)
f.holes_end <- matrix(as.numeric(f.holes[,,dim(f.holes)[3]]),ncol=2)

dat <- as.data.frame(scale(as.matrix(f), center=T, scale=T) %*% f.holes_end)
colnames(dat) <- c("x", "y")
dat %<>% cbind("Species" = flea[,7])
(gg5 <- ggplot(dat, aes(x = x, y = y, 
                        shape = Species, col = Species)) +
    geom_point() + ggtitle("Holes index projection pursuit"))

