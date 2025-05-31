library(rrcov)
library(SpatialNP)
library(psych)
library(ggplot2)
library(dplyr)
library(mvtnorm)

data("diabetes")
X <- diabetes[, -6]

robEst <- function(X, type = "classic", ...) {
    if (type == "winsor") {
        w <- winsor(X, ...)
        res <- list("mu" = colMeans(w), "cov" = cov(w))        
    } else if (type == "classic") {
        res <- list("mu" = colMeans(X), "cov" = cov(X))       
    } else if (type == "mve") {
        w <- CovMve(X)
        res <- list("mu" = w$center, "cov" = w$cov)
    } else if (type == "mcd") {
        w <- CovMcd(X)
        res <- list("mu" = w$center, "cov" = w$cov)
    } else if (type == "Mest") {
        w <- CovMest(X)
        res <- list("mu" = w$center, "cov" = w$cov)
    } else if (type == "Sest") {
        w <- CovSest(X)
        res <- list("mu" = w$center, "cov" = w$cov)
    } else if (type == "sde") {
        w <- CovSde(X)
        res <- list("mu" = w$center, "cov" = w$cov)
    } else {
        stop("Not yet implemented!")
    }
    
    return(res)
}

scoredist <- function(X, type = "classic", ...) {
    r <- robEst(X, type, ...)
    n <- nrow(X)
    p <- ncol(X)
    -log(dmvnorm(X, mean = r$mu, sigma = r$cov))
}


# Calculate All Score distances
methods <- c("classic", "winsor", "mve", "mcd", "Mest", "Sest", "sde")

index <- 1
message(paste("Performing method", methods[index]))
s <- scoredist(X, type = methods[index])
ggplot(tibble("Index" = seq_along(s), "Distance" = s, "Group" = diabetes$group)) + 
    geom_point(aes(x = Index, y = Distance, color = Group), size = 2) + 
    ggtitle(paste("Method ", methods[index])) + theme_bw()



for (m in methods) {
    s <- scoredist(X, type = m)
    ggplot(tibble("Index" = seq_along(s), "Distance" = s, "Group" = diabetes$group)) + 
        geom_point(aes(x = Index, y = Distance, color = Group), size = 2) + 
        theme_bw()
    ggsave(paste0("Distance-", m, ".png"), path = "./images/")
}




















