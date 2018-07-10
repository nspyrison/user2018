require("spinifex")
# slideshow works in seprate window, ut not on the rstudio Viewer tab. trying to install older ver.
#same issue even after reverting plotly, may revert rstudio.

#devtools::install_version("plotly", version = "4.5.6", 
#                          repos = "http://cran.us.r-project.org")


flea_std <- 
  apply(flea[,1:6], 2, function(x) ((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)))
data <- flea_std

proj1 <- proj_data(data, manip_var=3)
slideshow(proj1)

p <- ncol(data)
r_basis <- create_random_basis(p = p)
pch <- flea$species
col <- flea$species

proj2 <-
  proj_data(
    data = data,
    basis = r_basis,
    manip_var = 4,
    manip_type = "radial",
    phi_from = 0,
    phi_to = pi,
    n_slides = 20
  )
slideshow(proj2, col = col, pch = pch)


library("ggplot2")
ggplot(flea, aes(x=flea[,1], y=flea[,2])) + 
  geom_point(aes(colour=factor(label), 
                 fill = factor(label)), shape=21, size = 4) + 
  scale_fill_manual(values=c("blue", "cyan4")) + 
  scale_colour_manual(values=c("white", "black"))
