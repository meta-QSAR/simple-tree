.tree<-setClass(
  Class = "tree",
  slots=c(
    tree.list = "list"
  )
)

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
