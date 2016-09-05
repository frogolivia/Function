library(compiler)
#######################################################################################
### tree_distance is a function which returns a tree-distance matrix                ###
### X_i,j denotes the tree distance between node_i and node_j                       ###
###    i.e. In which leyer do node_i and node_j merge together (from the bottom)    ###
###  Input : A DCG-tree                                                             ###
### Outpur : An n by n matrix                                                       ###
#######################################################################################
tree_distance=function(tree){
  n=length(tree$order)
  tree_matrix_temp=matrix(0,n,n)
  temp=cutree(tree,h=sort(unique(tree$height)))
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      tree_matrix_temp[i,j]=which(temp[i,]-temp[j,]==0)[1]
    }
  }
  tree_matrix = tree_matrix_temp + t(tree_matrix_temp)
  diag(tree_matrix)=0
  return(tree_matrix)
}
tree_distance = cmpfun(tree_distance)

########################################################################################
### getgroupsize is a function reports the size of each group of  cutree resulst     ###
### Input : DCGtree(tree) / desired group numbers (g)                                ###
### Output : A vector of group size (length of g)                                    ###
########################################################################################
getgroupsize = function(tree, g){
    output = c()
    for (i in 1:g){
    output = c(output,length(which(cutree(tree,g)==i)))
    }
    return(output)
}