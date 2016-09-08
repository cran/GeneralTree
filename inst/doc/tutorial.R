## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
# Create a root node with id = 0 and data = "root"
require(GeneralTree)
tree <- GeneralTree$new(id = 0, data = "root")

## ------------------------------------------------------------------------
print(tree)

## ------------------------------------------------------------------------
# Add a child by specifying the parent.
tree$addNode(parent = 0, id = 1, data = "child0.1")
print(tree)

# Add a child by searching its parent.
tree$searchNode(1)$addChild(id = 2, data = "child1.2")
print(tree)

## ------------------------------------------------------------------------
# Add a sibling by specifying the parent.
tree$addNode(parent = 0, id = 3, data = "child0.3")
print(tree)

# Add a sibling by searching its parent.
tree$searchNode(1)$addSibling(id = 4, data = "child0.4")
print(tree)

## ------------------------------------------------------------------------
# Let us create a mixed tree.
tree <- GeneralTree$new(0, "parent1")
tree$addNode(0, "a", "child.a")
tree$addNode(0, "b", "child.b")
tree$addNode("b", "c", "child.b.c")
tree$addNode("b", "d", "child.b.d")
tree$addNode("c", "e", "child.c.e")
tree$addNode("c", "f", "child.c.f")
tree$addNode("b", "g", "child.b.g")
tree$addNode("b", "h", "child.b.h")
tree$addNode("c", 1, "child.c.1")

## ------------------------------------------------------------------------
print(tree$searchNode("f"))

## ------------------------------------------------------------------------
tree$searchData("e")

## ------------------------------------------------------------------------
plot(tree)

## ------------------------------------------------------------------------
plot(tree, what = "data", color = "coral1", shape = "oval")

## ------------------------------------------------------------------------
as.data.frame(tree)

## ------------------------------------------------------------------------
# Let us define a data frame,
test_tree_df <- data.frame(
    ID = c("root", "child1", "child2", "child3"),
    DATA = c("parent1", "data3.1", "data1.2", "data1.3"),
    PARENT = c(NA, "child3", "root", "root"), stringsAsFactors = FALSE)
test_tree_df

## ------------------------------------------------------------------------
as.GeneralTree(test_tree_df, id = "ID", data = "DATA", parent = "PARENT")

## ------------------------------------------------------------------------
p <- parse(text = "tree <- GeneralTree$new(1, \"parent1\")
                   tree$addNode(1, 2, \"child.1.2\")
                   tree$addNode(2, 3, \"child.2.3\")",
           keep.source = TRUE)
print(as.GeneralTree(p), what = "data")

## ------------------------------------------------------------------------
p <- parse(text =
           "test_that(\"test that the tree_walker with while loop\", {
              tree <- GeneralTree$new(1, \"parent1\")
              tree$addNode(1, 2, \"child.1.2\")
              tree$addNode(2, 3, \"child.2.3\")
             })",
           keep.source = TRUE)
print(as.GeneralTree(p), what = "data")

## ------------------------------------------------------------------------
# Let us inspect the tree first,
print(tree, what = "data")

# Make a backup of the tree,
old_tree <- tree

i <- tree$iterator()
while (!is.null(i)) {
    i$setData(paste("id:", i$id, "-data", i$data))
    i <- tryCatch(i$nextElem(), error = function(e) NULL)
}

print(tree, what = "data")

## ------------------------------------------------------------------------
require(iterators)
require(foreach)
itx <- iter(old_tree, by = "id")
ids_in_tree <- foreach(i = itx, .combine = c) %do% c(i)
ids_in_tree

## ------------------------------------------------------------------------
p <- parse(text = "
            tree <- GeneralTree$new(1, \"parent1\")
            tree$addNode(1, 2, \"child.1.2\")
            tree$addNode(2, 3, \"child.2.3\")
            tree$addNode(3, 4, \"child.3.4\")
            tree$addNode(3, 5, \"child.3.5\")
            tree$addNode(1, 7, \"child.1.7\")
            tree$addNode(1, 8, \"child.1.8\")
            tree$addNode(8, 9, \"child.8.9\")
            tree$addNode(9, 10, \"child.9.10\")
            tree$addNode(9, 11, \"child.9.11\")
            tree$addNode(9, 12, \"child.9.12\")
            tree$addNode(12, 13, \"child.12.13\")
            tree$addNode(8, 14, \"child.8.14\")
            tree$addNode(2, 6, \"child.2.6\")", keep.source = TRUE)
tree <- as.GeneralTree(p)

require(microbenchmark)

microbenchmark({
  i <- tree$iterator()
  ids_in_tree <- c()
  while (!is.null(i)) {
    ids_in_tree <- c(ids_in_tree, i$id)
    i <- tryCatch(i$nextElem(), error = function(e) NULL)
  }
})

require(foreach)
require(iterators)
require(doParallel)
# Test below on  your machine.
# nThreads <- detectCores(logical = TRUE)
# cl <- makeCluster(nThreads)
# registerDoParallel(cl)
# microbenchmark({
#   itx <- iter(tree, by = "id")
#   ids_in_tree <- foreach(i = itx, .combine = c) %dopar% c(i)
# })

