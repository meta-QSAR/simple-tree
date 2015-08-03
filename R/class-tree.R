#' An S4 class to represent a tree.
#'
#' @slot tree.list A list of nodes (node class)
#'
.tree<-setClass(
  Class = "tree",
  slots=c(
    tree.list = "list"
  )
)

#' @title Convert a data frame to a tree object
#'
#' @description
#' \code{as.tree} returns a tree object
#'
#' @param df A data frame.
#' @param id.col Column name which contains the node IDs (default: "id").
#' @param parent.id.col Column name which contains the parent node IDs (default: "parent.id").
#' @param name.col Column name which contains the node names (default: "name").
#' @return \code{as.tree} returns a tree object.
#'
as.tree <- function(df, id.col = "id", parent.id.col = "parent.id", name.col = "name"){
  tmp <- lapply(1:nrow(df), function(x){
    .node(id = as.character(df[x, id.col]), parent.id = as.character(df[x, parent.id.col]), name = as.character(df[x, name.col]))
  })
  names(tmp) <- df[[id.col]]
  .tree(tree.list = tmp)
}


setGeneric(name="distToRoot",
           def=function(object, node.id){standardGeneric("distToRoot")}
)

#' @title Compute distance to the Root node
#'
#' @description
#' \code{distToRoot} returns the distance from a particular node to the root node.
#'
#' @param object A tree object.
#' @param node.id The ID of the node.
#' @return \code{distToRoot} returns a numeric value.
#'
setMethod(
  f="distToRoot", signature="tree",
  definition=function(object, node.id){
    f1 <- function(node.x){
      if(!is.na(object@tree.list[[node.x]]@parent.id)) {
        return(1 + f1(object@tree.list[[node.x]]@parent.id))
      } else return(0)
    }
    f1(node.id)
  }
)

setGeneric(name="distNodes",
           def=function(object, node1.id, node2.id){standardGeneric("distNodes")}
)

#' @title Compute distance between two nodes.
#'
#' @description
#' \code{distNodes} returns the distance between two nodes.
#'
#' @param object A tree object.
#' @param node1.id The ID of the 1st node.
#' @param node2.id The ID of the 2nd node.
#' @return \code{distNodes} returns a numeric value.
#'
setMethod(
  f="distNodes", signature="tree",
  definition=function(object, node1.id, node2.id){
    res <- distToRoot(object, node1.id) + distToRoot(object, node2.id) - 2*distToRoot(object, lca(object, node1.id, node2.id))
    res
  }
)

setGeneric(name="getPathToRoot",
           def=function(object, node.id){standardGeneric("getPathToRoot")}
)

#' @title Get path to root.
#'
#' @description
#' \code{getPathToRoot} returns the path from a particular node to root.
#'
#' @param object A tree object.
#' @param node.id The ID of the 1st node.
#' @return \code{getPathToRoot} returns a character vector.
#'
setMethod(
  f="getPathToRoot", signature="tree",
  definition=function(object, node.id){
    f1 <- function(node.x){
      if(!is.na(object@tree.list[[node.x]]@parent.id)) {
        return(c(f1(object@tree.list[[node.x]]@parent.id),node.x))
      } else return(node.x)
    }
    f1(node.id)
  }
)

setGeneric(name="lca",
           def=function(object, node1.id, node2.id){standardGeneric("lca")}
)

#' @title Find the lowest common ancestor.
#'
#' @description
#' \code{lca} returns the ID of the lowest common ancestor between two nodes.
#'
#' @param object A tree object.
#' @param node1.id The ID of the 1st node.
#' @param node2.id The ID of the 2nd node.
#' @return \code{lca} returns a character value.
#'
setMethod(
  f="lca", signature="tree",
  definition=function(object, node1.id, node2.id){
    path.n1 <- getPathToRoot(object, node1.id)
    path.n2 <- getPathToRoot(object, node2.id)
    if(length(path.n1)>length(path.n2)) path.n1 <- path.n1[1:length(path.n2)]
    else path.n2 <- path.n2[1:length(path.n1)]
    mch <- which(path.n1 == path.n2)
    path.n1[mch[length(mch)]]
  }
)

setGeneric(name="distMatrix",
           def=function(object){standardGeneric("distMatrix")}
)

#' @title Compute a matrix of distances between all nodes.
#'
#' @description
#' \code{distMatrix} returns a matrix of distances between all nodes.
#'
#' @param object A tree object.
#' @return \code{distMatrix} returns a numeric value.
#'
setMethod(
  f="distMatrix", signature="tree",
  definition=function(object){
    n.nodes <- length(object@tree.list)
    res <- matrix(nrow = n.nodes, ncol = n.nodes)
    nodes.id <- names(object@tree.list)
    rownames(res) <- nodes.id
    colnames(res) <- nodes.id
#    lapply(1:n.nodes, function(x){
#      lapply(x:n.nodes, function(y){
    for(x in 1:n.nodes){
      for(y in x:n.nodes){
        res[x,y] <- distNodes(object, nodes.id[x], nodes.id[y])
      }
    }

#      })
#    })
    res[lower.tri(res)] <- t(res)[lower.tri(res)]
    res
  }
)
