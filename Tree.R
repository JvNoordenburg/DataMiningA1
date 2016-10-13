
# Standard library that enables the creation of OOP objects that allow side effects. We use this to create a Queue data structure class to perform our BFS.
library(R6)

# Little macro to refresh the R session. This crashes the session, which allows you to restart it from scratch.
makeActiveBinding("refresh", function() { shell("Rgui"); q("no") }, .GlobalEnv)
makeActiveBinding("refresh", function() { system("R"); q("no") }, .GlobalEnv)

data.generate <- function(set, partindexes, k, vnmin, vminleaf) {
  
  # Rows nmin values, Cols minleaf values!
  error.matrix <- matrix(NA, nrow = length(vnmin), ncol = length(vnmin))
  
  for(nminIndex in 1:length(vnmin)) {
    
    value.min <- vnmin[nminIndex]
    
    for(minleafIndex in 1:nminIndex) {
      
      value.leaf <- vminleaf[minleafIndex]
      
      print(paste(value.min, value.leaf))
      
      error.rate <- validate.kfold(set, partindexes, value.min, value.leaf, 10)
      
      error.matrix[nminIndex, minleafIndex] <- error.rate
      
    }
  }
  
  dimnames(error.matrix) <- list(paste(vnmin), paste(vminleaf))
  
  error.matrix
  
}

dostuff <- function(x) {
  return(paste(x))
}








###########################
#                         #
#          MAIN           #
#                         #
###########################


# Name:
#     - tree.grow

# Parameters:
#     - x as a data.frame of all the data entries on which to grow the tree.
#     - y as a vector of classification labels (1 or 0).
#     - nmin as an integer restricting the amount of entries a node must have in order to be ligible to split.
#     - minleaf as an integer restricting how big a resulting child node must be in order to be legal.

# Returns:
#     - A tree object represented by a data.frame.

# Description:
#     - This function grows a tree on the input x and the classification labels y.
#     - The growing of the tree can be stopped prematurely by parsing nmin and minleaf paramters larger than 1.
#     - This is to prevent overfitting. 
#     - The parameter nmin should not be lower than minleaf as this would not make sense and would only lead to unnecessary calculations.
tree.grow <- function(x, y, nmin, minleaf) 
{
  # Create the empty tree.
  tree <- data.frame(parent = numeric(), lchild = numeric(), rchild = numeric(), splitvar = numeric(), splitval = numeric(), n = numeric(), imp = numeric(), gr = numeric(), pnode = numeric(), enode = numeric(), rnode = numeric())
  
  # Create the nodelist and prefil it with all the records.
  nodelist <- list(1:nrow(x))
  
  # Create a new queue and add the parent node to it as an entry point.
  queue <- Queue$new()
  queue$add(Node$new(1, x, y, nodelist[[1]], 0))
  
  # Enter the recursive call, which builds the actual tree.
  tree <- tree.visitNext(tree, queue, x, y, nodelist, nmin, minleaf) 
  
  # Set the column names of the tree and return the object.
  colnames(tree) <- c("parent", "lchild", "rchild", "splitvar", "splitval", "n", "imp", "gr", "pnode", "enode", "rnode")
  return(tree)
}










# Name:
#     - tree.classify

# Parameters:
#     - x as a data.frame of all the data entries and their classifications.
#     - tr as a classified and posibly simplified table, represented by a data.frame.

# Returns:
#     - A vector containing the predicted classifications of the parsed data set, as predicted by the parsed tree.

# Description:
#     - This function classifies an input data set x according to the classification tree tr.
tree.classify <- function(x, tr) 
{
  # Create empty vector
  class.results <- c()
  
  # Determine class for each case in x
  for(index in 1:nrow(x)) 
  {
    
    # Get case
    case <- x[index,]
    
    # Get root node
    node.current <- tr[1,]
    
    # While the current node is not a leafnode, the current case is not yet classified
    # and we should continue down the tree
    while (!tree.isLeafNode(node.current)) {
      
      # Get childnode's row
      child.row <- node.classify(case, node.current)
      
      # Set current node based on child's row name and not on row number
      # as tree.simplify might have deleted rows from the tree.
      node.current <- tr[toString(child.row),]
    }
    
    # Add the class of the resulting leafnode to the results
    class.results <- c(class.results, node.current$gr)
  }
  
  # Return results
  class.results
}








# Name:
#     - tree.simplify

# Parameters:
#     - tree as a data.frame representing the grown tree which we want to simplify.

# Returns:
#     - A pruned version of the parsed tree.

# Description:
#     - This function simplifies the parsed tree using cost-complexity pruning to prevent overfitting of the model.
#     - The algorithm calculates T1 from Tmax by checking if the resubstitution error of a parent node is equal to that of its children.
tree.simplify <- function(tree) 
{ 
  # Get all parent nodes
  parent.nodes <- tree[tree$splitvar != -1,]
  
  # For all parent nodes, starting from the last one
  for(index in nrow(parent.nodes):1) {
    
    # Get node values
    node.t <- parent.nodes[index,]
    
    # Get node indices
    lchild.index <- node.t$lchild
    rchild.index <- node.t$rchild
    
    # Get child nodes from tree
    lchild <- tree[lchild.index,]
    rchild <- tree[rchild.index,]
    
    if(tree.isLeafNode(lchild) && tree.isLeafNode(rchild)) {
      # If both child nodes are leaf nodes (splitvar == -1)
      
      # Add resubstitution errors of child nodes
      pruned.enode = lchild$enode + rchild$enode
      
      if(pruned.enode >= node.t$enode) {
        # If the total resubstitution error of the child nodes is equal or larger than the 
        # resubstitution error of node.t, prune in node.t
        
        # Convert node.t to leaf node
        tree[lchild$parent,] <- tree.makeLeafNode(node.t)
        
        # Remove the child nodes from the tree
        tree <- tree[-c(lchild.index, rchild.index),]
      }
    }
  }
  
  # Return simplified tree
  tree
}







###########################
#                         #
#   QUEUE DATASTRUCTURE   #
#                         #
###########################

Queue <- R6Class(
  "Queue",
  public = list(
    initialize = function()
    {
      private$mElements <- c()
      private$mAlreadyProcessed <- 0
    },
    getCount = function()
    {
      return(length(private$mElements) + private$mAlreadyProcessed)
    },
    add = function(element)
    {
      private$mElements <- c(private$mElements, list(element))
      return(self$getCount())
    },
    pop = function()
    {
      if(length(private$mElements) == 0)
      {   return(NULL)}
      else
      {   
        elements <- private$mElements  
        element <- elements[[1]]
        
        private$setElements(c(elements[-1]))
        private$mAlreadyProcessed <- private$mAlreadyProcessed + 1
        
        return(element)
      }
    },
    getElements = function()
    {
      return(private$mElements)
    }
  ),
  private = list(
    mElements = NULL,
    mAlreadyProcessed = NULL,
    setElements = function(elements)
    {
      private$mElements <- elements
    }
  )
)






###########################
#                         #
#    NODE DATASTRUCTURE   #
#                         #
###########################

Node <- R6Class(
  "Node",
  public = list(
    mIndex = NULL,
    mEntries = NULL,
    mClassifications = NULL,
    mRowNumbers = NULL,
    mParent = NULL,
    initialize = function(index, entries, classifications, rowNumbers, parent)
    {
      self$mParent <- parent
      self$mRowNumbers <- rowNumbers
      self$mIndex <- index
      self$mEntries <- entries
      self$mClassifications <- classifications
    }
  )
)








###########################
#                         #
#   SPLIT DATASTRUCTURE   #
#                         #
###########################

Split <- R6Class(
  "Split",
  public = list(
    mReduction = NULL,
    mValue = NULL,
    mAttribute = NULL,
    mLeftEntries = NULL,
    mRightEntries = NULL,
    mLeftClassifications = NULL,
    mRightClassifications = NULL,
    mLeftRowNumbers = NULL, 
    mRightRowNumbers = NULL,
    initialize = function(reduction, value)
    {
      self$mReduction <- reduction
      self$mValue <- value
    }
  )
)







###########################
#                         #
#          MAIN           #
#                         #
###########################

# Name:
#     - tree.visitNext

# Parameters:
#     - tree as a data.frame representing the current recursion's state of the tree.
#     - queue as a Queue R6Class object representing a Queue datastructure.
#     - x as a data.frame of all the data entries on which to grow the tree.
#     - y as a vector of classification labels (1 or 0).
#     - nodelist as a vector of vectors.
#     - nmin as an integer restricting the amount of entries a node must have in order to be ligible to split.
#     - minleaf as an integer restricting how big a resulting child node must be in order to be legal.

# Returns:
#     - A fully grown, unsimplified tree, represented by a data.frame.

# Description:
#     - This function uses recursion and a Queue datstructure to grow a tree on input x and labels y in a breadth-first manner.
tree.visitNext <- function(tree, queue, x, y, nodelist, nmin, minleaf)
{
  # Get the next node from the queue.
  node <- queue$pop()
  
  # Guard statement for when the queue is empty. This indicates the end of the recursion.
  if(is.null(node))
  {   return(tree)}
  
  # Get the details of the current node.
  index <- node$mIndex
  parent <- node$mParent
  entries <- node$mEntries
  classifications <- node$mClassifications
  rowNumbers <- node$mRowNumbers
  
  # This is messy. We should clean this up and just use data frames all the way through.
  if(class(entries) != "data.frame")
  {   entries <- data.frame(entries)}
  if(class(classifications) != "data.frame")
  {   classifications <- data.frame(classifications)}
  
  # Calculate the necessary data that is used for both a leaf and a branch node.
  imp <- impurity(classifications[,1])
  n <- nrow(entries)
  gr <- if(mean(classifications[,1]) <= 0.5) 0 else 1
  pnode <- n / nrow(x)
  enode <- resubstitutionError(classifications, gr)
  rnode <- pnode * enode
  
  # Add the rows of the node to the nodelist.
  nodelist[index] <- list(rowNumbers)
  
  # Get the best possible split of the remaining attributes.
  best_split <- getBestPossibleSplit(x, y, rowNumbers, minleaf)
  
  # If the impurity is zero, or the size of the node does not match or exceed nmin, we register a leaf node in the data frame and enter recursion.
  if(imp == 0 || length(rowNumbers) < nmin || is.null(best_split))
  {
    tree <- rbind(tree, c(parent, -1, -1, -1, -1.0, n, imp, gr, pnode, enode, rnode))
    return(tree.visitNext(tree, queue, x, y, nodelist, nmin, minleaf))
  }
  # If there is still impurity and the size of the node does not match or exceed nmin, we should keep splitting.
  else
  {
    # Calculate the remaining data, which is only used in case of a branch node.
    splitvar <- best_split$mAttribute
    splitval <- best_split$mValue
    
    leftEntries <- best_split$mLeftEntries
    rightEntries <- best_split$mRightEntries
    leftClassifications <- best_split$mLeftClassifications
    rightClassifications <- best_split$mRightClassifications
    leftRowNumbers <- best_split$mLeftRowNumbers
    rightRowNumbers <- best_split$mRightRowNumbers
    
    # Add the two new child nodes to the queue.
    lChild <- queue$getCount() + 1;
    queue$add(Node$new(lChild, leftEntries, leftClassifications, leftRowNumbers, index))
    rChild <- queue$getCount() + 1;
    queue$add(Node$new(rChild, rightEntries, rightClassifications, rightRowNumbers, index))
    
    
    # Register a branch node and enter the recursion.
    tree <- rbind(tree, c(parent, lChild, rChild, splitvar, splitval, n, imp, gr, pnode, enode, rnode))
    return(tree.visitNext(tree, queue, x, y, nodelist, nmin, minleaf))
  }
}











# Name:
#     - data.getSamples

# Parameters:
#     - data as a data.frame representing all the entries.
#     - k as a numerical indicating how many folds we should perform in our k-fold algorithm.
#     - trainingsize as a numerical representing the size of the training set.
#     - seed as a numerical representing the seef by which we draw random rows from the training data.

# Returns:
#     - A list containing the training samples, the test samples and a list of which index belongs to which part.

# Description:
#     - This function splits the data set in to a training sample, test sample and splits the training sample in to k equal sized parts for the k-fold algorithm.
data.getSamples <- function(data, k, trainingsize = 200, seed = NaN) 
{
  # Set seed to add the possibility of getting the same data back between sessions
  if(!is.nan(seed)) 
  {   set.seed(seed)}
  
  # Get x random unique rows from the dataset
  training.indexes <- sample(nrow(data), trainingsize, replace = FALSE)
  # Store rows corresponding to the random sample for us as training sample
  training.sample <- data[training.indexes,]
  # Store remaining rows for use as test sample
  test.sample <- data[-training.indexes,]
  
  # Get a list of parts, containing the indexes of training.sample
  parts.indexes <- indexes.equalparts(1:nrow(training.sample), k)
  
  # Store samples in named list
  samples.list <- list(training = training.sample, test = test.sample, partindexes = parts.indexes)
  
  # Return samples
  samples.list
}










# Name:
#     - validate.kfold

# Parameters:
#     - set as a data.frame of the training sample we received from data.getSamples.
#     - kindexes as a list of vectors that contain the rowindexes of all entries in that part.
#     - nmin as an integer restricting the amount of entries a node must have in order to be ligible to split.
#     - minleaf as an integer restricting how big a resulting child node must be in order to be legal.
#     - k as a numerical indicating how many folds we should perform in our k-fold algorithm.

# Returns:
#     - The error rate of the classification of the test sample using the parsed nmin and minleaf as the tree growing parameters.

# Description:
#     - This function performs a kfold on the training set using the parsed parameters.
validate.kfold <- function(set, kindexes, nmin, minleaf, k = 10) {
  
  training.results <- NULL
  
  for(index in 1:k) {
    
    # Get the the current fold's indexes
    k.indexes <- kindexes[[index]]
    
    # Get the rows corresponding to the current fold
    classify.dat <- set[k.indexes,]
    # Get the other values to use to grow the tree
    tree.dat <- set[-k.indexes,]
    
    # Get splittable attributes from tree.dat -> all columns except the last one
    tree.attributes <- tree.dat[, -ncol(set)]
    # Get classes from tree.dat -> last column
    tree.classes <- tree.dat[, ncol(set)]
    
    # Grow and simplify the tree using the given nmin and minleaf
     tree <- tree.simplify(tree.grow(tree.attributes, tree.classes, nmin, minleaf))
    # tree <- tree.grow(tree.attributes, tree.classes, nmin, minleaf)
    
    # Classify the current fold's data
    class <- tree.classify(classify.dat, tree)
    
    # Get the rownames of the current fold's data for errorrate calculation, as the 
    # data is randomized
    rowname <- rownames(classify.dat)
    
    # Create dataframe with the results
    results <- data.frame(rowname, class)
    
    # Append results to overall results to construct predicted values of all data in the set
    training.results <- rbind(training.results, results)
  }
  
  # Calculate errorrate on all results
  error.rate <- tree.errorrate(training.results, set)
  # Return errorrate
  error.rate
  
}







# Name:
#     - tree.errorrate

# Parameters:
#     - predict as a data.frame of rownames and classification labels (1 or 0) that were predicted by tree.classify.
#     - data as a data.frame containing the original input data.

# Returns:
#     - The error rate of the classification as predicted by the tree.

# Description:
#     - This function calculates the errorrate, defined as the amount of errors devided by the total amount of predictions.
tree.errorrate <- function(predict, data) 
{
  error.count <- 0
  
  for(index in 1:nrow(predict)) 
  {
    predict.entry <- predict[index,]
    # Get true class based on rowname, as the results are randomized
    true.class <- data[predict.entry$rowname, ncol(data)]
    
    # If predicted value doesn't match actual value, add 1 to error.count
    if(predict.entry$class != true.class) 
    {
      error.count <- error.count + 1
    }
  }
  
  print(error.count)
  
  error.rate <- error.count / nrow(predict)
  error.rate
}










# Name:
#     - indexes.equalparts

# Parameters:
#     - rowids as a vector of remaining row numbers that haven't been assigned a part yet.
#     - parts as a numerical indicating how many parts there are left to assign rows to.

# Returns:
#     - A list of rowids that were randomly assigned to the current part.

# Description:
#     - This function takes x amount of randomly selected rows from rowids where x is the size of rowids / parts.
#     - Every recursion the parts parameter hould be decreased by one so that every part is of equal size in the end.
#     - If there are any leftover rows, these are returned in the last iteration.
indexes.equalparts <- function(rowids, parts) 
{
  # Create result variable
  indexes.list <- NULL
  
  # If the (remaining) rowids need to be split in more than 1 part
  if(parts > 1) 
  {
    # Determine number of items to take from rowids and round to nearest integer
    # so the parts are approximately equal size and remainders are spread out over parts
    nitems <- round(length(rowids) / parts)
    
    # Get nitems randomly from the rowids & create a list from the result
    indexes.random <- sample(rowids, nitems, replace = FALSE)
    indexes.list <- list(indexes.random)
    
    # Remove indexes.random from the rowids, so no indexes will be used twice
    rowids.remainder <- rowids[!rowids %in% indexes.random]
    
    # Recursively add the rest of the parts, subracting 1 from parts as the remaining rowids
    # only need to be divided into one part less than this iteration.
    indexes.list <- append(indexes.list, indexes.equalparts(rowids.remainder, parts - 1))
  } 
  else 
  {
    # If last part (1), return remaining rowids
    indexes.list <- list(rowids)
  }
  
  # Return list of indexes
  indexes.list
}











###########################
#                         #
#        HELPERS          #
#                         #
###########################

# Name:
#     - tree.makeLeafNode

# Parameters:
#     - node as a data.frame.

# Returns:
#     - The parsed node converted to a leafnode.

# Description:
#     - This function takes a node (represented by a data.frame) and sets the necessary data members to -1 to turn it in to a leaf node.
tree.makeLeafNode <- function(node) 
{
  # Set all necessary values to -1 to make the node a leaf node
  node$lchild <- -1
  node$rchild <- -1
  node$splitvar <- -1
  node$splitval <- -1
  
  # Return node
  node
}






# Name:
#     - tree.isLeafNode

# Parameters:
#     - node as a data.frame.

# Returns:
#     - A Logical indicating whether or not the parsed node is a leaf node.

# Description:
#     - This function checks if the parsed node is a leaf node.
tree.isLeafNode <- function(node) 
{   node$splitvar == -1}











# Name:
#     - node.classify

# Parameters:
#     - case as a data.frame row from the test data.
#     - node as a data.frame row from the tree.

# Returns:
#     - The row index of the left or right child of the parsed node, to which the case should be assigned.

# Description:
#     - This function receives a data entry "case" and a tree node "node" and classifies it.
#     - This classification tells the caller to which node the case should be assigned, the left or the right child node.
node.classify <- function(case, node) 
{
  # Get respective value based on the splitvar of the node
  case.val <- case[, node$splitvar]
  
  if(case.val < node$splitval) 
  {
    # If case.val is smaller than the split value, return lchild row
    node$lchild
  } 
  else 
  {
    # If case.val is equal or larger than the split value, return rchild row
    node$rchild
  }
}












# Name:
#     - getBestPossibleSplit

# Parameters:
#     - x as a data.frame of all the data entries on which to grow the tree.
#     - y as a vector of classification labels (1 or 0).
#     - rowNumbers as a vector of row indexes that point to the nodes contained in the current node.
#     - minleaf as an integer restricting how big a resulting child node must be in order to be legal.

# Returns:
#     - A Split object containing the details of the best possible split we can do in the current situation.

# Description:
#     - This function looks at all the attributes in order to deside what the best possible split is.
#     - The details of the best possible split that are added in this method are the entries that go to the left, 
#     - the entries that go to the right child and the column index of the attribute on which we have split.
getBestPossibleSplit <- function(x, y, rowNumbers, minleaf)
{
  
  # Initialize working variables.
  optimum_reduction <- 0
  optimum_attribute_to_split <- -1
  optimum_split <- NULL
  
  # For each of our attributes.
  for(i in 1:ncol(x))
  {
    # Split this attribute on the best possible split value.
    split <- findBestSplitOnAttribute(x[,i], y, minleaf, rowNumbers)
    current_reduction <- split$mReduction
    
    # If the reduction of this split is better than our current optimum, replace our current optimum with the current split.
    if(is.null(current_reduction) == FALSE && is.na(current_reduction) == FALSE && current_reduction > optimum_reduction)
    {
      optimum_reduction <- current_reduction
      optimum_attribute_to_split <- i
      optimum_split <- split
    }
  }
  
  leftRowNumbers <- NULL
  rightRowNumbers <- NULL
  
  if(optimum_attribute_to_split == -1)
  {   return(NULL)}
  
  # Fill them according to whether they are above or below the optimum split value.
  for(i in 1:length(rowNumbers))
  {
    number <- rowNumbers[i]
    if(x[[optimum_attribute_to_split]][number] < optimum_split$mValue)
    {   leftRowNumbers <- c(leftRowNumbers, number)}
    else
    {   rightRowNumbers <- c(rightRowNumbers, number)}  
  }
  
  # Add the additional data to the Split object.
  optimum_split$mAttribute <- optimum_attribute_to_split
  optimum_split$mLeftRowNumbers <- leftRowNumbers
  optimum_split$mRightRowNumbers <- rightRowNumbers
  
  # Return the data as a Split object.
  return(optimum_split)
}










# Name:
#     - findBestSplitOnAttribute

# Parameters:
#     - x as a vector of all entries' values in a particular column.
#     - y as a vector of classification labels (1 or 0).
#     - minleaf as an integer restricting how big a resulting child node must be in order to be legal.
#     - rowNumbers as a vector of row indexes that point to the nodes contained in the current node.

# Returns:
#     - A Split object containing the details of the best possible split we can do on the current attribute values.

# Description:
#     - This function looks at a single attribute (column) of the input and selects the rows from it that occur in the current node.
#     - It then looks at every plausible splitvalue to decide which split causes the maximum reduction in impurity.
findBestSplitOnAttribute <- function(x, y, minleaf, rowNumbers)
{
  values <- c()
  labels <- c()
  
  for(i in 1:length(rowNumbers))
  {
    number <- rowNumbers[i]
    values <- c(values, x[number])
    labels <- c(labels, y[number])
  }
  
  # Create a list of mid-way points between the unique values of the current attribute, as a list of possible split points.
  values.sorted <- sort(unique(values))
  values.length <- length(values.sorted)
  values.splits <- (values.sorted[1:values.length - 1] + values.sorted[2:values.length]) / 2
  
  # Initialize working variables.
  optimum_split <- NULL
  
  # For each of our possible split values.
  for(current_index in 1:length(values.splits))
  {
    # Calculate the reduction if we split here.
    current_split <- split(values, labels, values.splits[current_index])
    
    # If the reduction is higher than our current optimum, and we don't violate the minleaf constraint, replace it.
    leftNodeIsBigEnough <- length(current_split$mLeftEntries) >= minleaf
    rightNodeIsBigEnough <- length(current_split$mRightEntries) >= minleaf
    currentSplitIsBetter <- TRUE
    if(is.null(optimum_split) == FALSE)
    {   currentSplitIsBetter <- current_split$mReduction >= optimum_split$mReduction}
    
    if(leftNodeIsBigEnough && rightNodeIsBigEnough && currentSplitIsBetter)
    {   optimum_split <- current_split}
  }
  
  
  # Return the optimum split on this attribute as a Split object.
  return(optimum_split)
}









# Name:
#     - split

# Parameters:
#     - values as a vector of numericals.
#     - labels as a vector of classification labels (1 or 0) belonging to each of the values.
#     - splitpoint as a numerical indicating on which to split the values.

# Returns:
#     - A Split object containing the details of this split.

# Description:
#     - This function splits the values and calculates the resulting reduction in impurity using the gini-index.
#     - The Split object is then filled with preliminary details such as the impurity reduction, splitvalue, values in the left and right childs and labels in the left and right childs.
split <- function(values, labels, splitpoint)
{
  #print(cat("Values:  ", values, "     labels:  ", labels, "   splitpoint: ", splitpoint))
  
  # Perform the split on the labels.
  left.labels <- labels [values <= splitpoint]
  right.labels <- labels [values > splitpoint]
  
  # Perform the split on the values.
  left.values <- values[values <= splitpoint]
  right.values <- values[values > splitpoint]
  
  # Calculate the variables necessary for calculating the impurity reduction of the split.
  values.impurity <- impurity(labels)
  left.proportion <-  length(left.labels)  / length(values)
  left.impurity <- impurity(left.labels)
  right.proportion <- length(right.labels) / length(values)
  right.impurity <- impurity(right.labels)
  
  #print(cat(values.impurity, left.proportion, left.impurity, right.proportion, right.impurity))
  
  reduction <- values.impurity - ((left.proportion) * left.impurity) - (right.proportion * right.impurity)
  
  # Create a Split object to hold the details of this split.
  split <- Split$new(reduction, splitpoint)
  split$mLeftEntries <- left.values
  split$mRightEntries <- right.values
  split$mLeftClassifications <- left.labels
  split$mRightClassifications <- right.labels
  
  return (split)
}








# Name:
#     - resubstitutionError

# Parameters:
#     - y as a vector of classification labels (1 or 0).
#     - majorityClass as a numerical indicating the prevalent classification label in y.

# Returns:
#     - The resubstitution error of the set of classification labels.

# Description:
#     - This function calculates the resubstitution error of the input vector y.
#     - This is equal to the proportion of errors we make when we classify all entries in a node to its majority class.
resubstitutionError <- function(y, majorityClass)
{
  bad_classifications <- sum(y != majorityClass)
  bad_classifications / nrow(y);
}








# Name:
#     - impurity

# Parameters:
#     - y as a vector of classification labels (1 or 0).

# Returns:
#     - The impurity of the set of classification labels.

# Description:
#     - This function calculates the impurity of the input vector y.
#     - This can be done using mean because the classification variable is binary 
#     - and so the mean of the labels equals the probability of a 1.
impurity <- function(y)
{
  p1 <- mean(y)
  p1 * (1 - p1)
}