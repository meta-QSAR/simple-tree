rm(list=ls())
library(devtools)
load_all(pkg=".")

tree1.df <- data.frame(class.id = c(0, 1, 2, 3, 10),
                       parent.id = c(NA, 0, 0, 0, 1),
                       pref.name = c("root", "node1", "node2", "node3", "node10"),stringsAsFactors = F)

tree1 <- as.tree(tree1.df, id.col = "class.id", parent.id.col = "parent.id", name.col = "pref.name")
distToRoot(tree1, "10")
distToRoot(tree1, "3")
distToRoot(tree1, "0")
aa <- getPathToRoot(tree1, "10")
lca(tree1, "10", "3")

distNodes(tree1, "10","3")
distNodes(tree1, "1","3")
distNodes(tree1, "0","3")
distNodes(tree1, "1","2")
distNodes(tree1, "0","0")
distNodes(tree1, "1","1")
M1 <- distMatrix(tree1)
