library(pcalg)
library(causalsens)
library(dplyr)

data("lalonde.psid")
data("lalonde.exp")


create_data <- function(data) {
    Black <- factor(data$black)
    Hispanic <- factor(data$hispanic)
    Married <- factor(data$married)
    Nodegree <- factor(data$nodegree)
    # u74 <- factor(data$u74)
    # u75 <- factor(data$u75)
    Treatment <- factor(data$treat)
    AgeGroup <- cut(data$age, c(0, 20, 25, 30, 35, 40, 45, 50, 55))
    Education <- cut(data$education, c(-1, 5, 10, 12, 15, 17), 
                     labels = c("Primary","Secondary","Higher Secondary","Graduate","Post Graduate"))
    Re74 <- cut(data$re74, c(-1, 0, 20000, 40000, Inf), labels = c("Unemployed", "Low","Med","High"))
    Re75 <- cut(data$re75, c(-1, 0, 20000, 40000, Inf), labels = c("Unemployed", "Low","Med","High"))
    Re78 <- cut(data$re78, c(-1, 0, 20000, 40000, Inf), labels = c("Unemployed", "Low","Med","High"))
    
    # df <- tibble(Black, Hispanic, Married, Nodegree, u74, u75, AgeGroup, Education, Re74, Re75, Re78, Treatment)
    df <- tibble(Black, Hispanic, Married, Nodegree, AgeGroup, Education, Re74, Re75, Re78, Treatment)
    return(df)
}

data <- create_data(lalonde.psid)
data <- create_data(lalonde.exp)


Vars <- colnames(data)
m <- matrix(FALSE, nrow = length(Vars), ncol = length(Vars))
colnames(m) <- Vars
rownames(m) <- Vars
m["Re74","Re75"] <- TRUE
m["Re75","Re74"] <- TRUE
m["Re75","Re78"] <- TRUE
m["Re78","Re75"] <- TRUE
m["Treatment","Re78"] <- TRUE
m["Re78","Treatment"] <- TRUE




# define Suff Stat
suffStat <- list(dm = sapply(data, function(x){as.numeric(x) - 1}), 
                 nlev = sapply(data, nlevels), adaptDF = FALSE)

pc.D <- pc(suffStat, indepTest = disCItest, alpha = 0.01, labels = Vars, 
           verbose = FALSE)


pc.D <- pc(suffStat, indepTest = disCItest, alpha = 0.01, labels = Vars, 
           verbose = FALSE, fixedEdges = m)

plot(pc.D@graph, main = "Estimated Causal DAG for Lalonde Experimental Data",
     attrs = list(graph = list(bgcolor = "grey90"), 
                  node = list(color = "blue", fontcolor = "red", fillcolor = "white"),
                  edge = list(color = "blue")))

library(ggraph)

g <- igraph::graph_from_graphnel(pc.D@graph)

set.seed(1234)
ggraph(g, layout = "kk", circular = F) +
    geom_node_circle(aes(r = 0.2), fill = "white", colour = "blue") +
    geom_edge_link(aes(start_cap = ellipsis(1.5, 0.75, 'cm'),
                        end_cap = ellipsis(1.5, 0.75, 'cm')), 
                        arrow = arrow(length = unit(4, 'mm')), 
                   edge_width = 1, colour = "blue") + 
    geom_node_text(aes(label = Vars), colour = "red", size = 5) +
    ggtitle("Estimated Causal DAG (Restricted) for Lalonde Experimental Data") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

# Estimation

m <- igraph::get.adjacency(g)
m <- as.matrix(m)
m["Re75", "Re74"] <- 0
m["Nodegree", "Education"] <- 0
m["Married","AgeGroup"] <- 0

g <- igraph::graph.adjacency(m)
g <- igraph::igraph.to.graphNEL(g)
plot(g)

true.amat <- as(g, "matrix") != 0
rownames(true.amat) <- 1:10
colnames(true.amat) <- 1:10

backdoor(true.amat, x = 7, y= 10, type="cpdag")
adjustment(true.amat, amat.type = "cpdag", x = 7, y = 10, set.type = "all")


##################################

covs <- ability.cov$cov
vars <- diag(covs)

cors <- diag(1/sqrt(vars)) %*% covs %*% diag(1/sqrt(vars))

rownames(cors) <- rownames(covs)
colnames(cors) <- colnames(covs)

suffStat <- list(C = cors, n = ability.cov$n.obs)
pc.fit <- pc(suffStat, indepTest = gaussCItest, alpha = 0.05, 
             labels = colnames(cors), verbose = FALSE)
plot(pc.fit)

g <- igraph::graph_from_graphnel(pc.fit@graph)
Vars <- colnames(cors)

ggraph(g, layout = "kk") +
    geom_node_circle(aes(r = 0.2), fill = "white", colour = "blue") +
    geom_edge_link(aes(start_cap = ellipsis(1.5, 0.75, 'cm'),
                       end_cap = ellipsis(1.5, 0.75, 'cm')), 
                   arrow = arrow(length = unit(4, 'mm')), 
                   edge_width = 1, colour = "blue") + 
    geom_node_text(aes(label = Vars), colour = "red") +
    ggtitle("Estimated Causal DAG for Ability and Intelligence Tests Data") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))


ida(5, 6, covs, pc.fit@graph, method = "global")

weights <- c(0.8115, 0.894, 2.707, 0.144, 1.528 , 1.1885)

ggraph(g, layout = "kk") +
    geom_node_circle(aes(r = 0.2), fill = "white", colour = "blue") +
    geom_edge_link(aes(start_cap = ellipsis(1.5, 0.75, 'cm'),
                       end_cap = ellipsis(1.5, 0.75, 'cm'),
                       label = as.character(weights)),
                   angle_calc = 'along',
                   label_dodge = unit(4, 'mm'),
                   label_size = 5,
                   arrow = arrow(length = unit(4, 'mm')), 
                   edge_width = 1, colour = "blue") + 
    geom_node_text(aes(label = Vars), colour = "red", size = 7) +
    ggtitle("Estimated Causal DAG for Ability and Intelligence Tests Data") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

#############################################











