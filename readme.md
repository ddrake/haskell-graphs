Dijkstra's Algorithm
--------------------

Goal: Find an optimal (least 'distance') path from one node to another in a simply connected, weighted graph.

Initialize

- Start with a graph and an empty list of 'checked' nodes
- Select a node to be the starting point
- For each node, initialize the 'distance' from the start node to the node and a 'precursor' node.  The initialization is based on the following three rules:
   - the precursor of the start node is itself and its distance is zero.
   - for any node that is not connected to the start node, the precursor is itself and its distance is positive infinity.
   - for any node that is connected to the start node, the precursor is the start node and the distance is the weight of the edge connecting the node to the start node.

Iterate on the following steps

- Select a non-checked node with minimal distance as the new current node (if none we're done)
- Add this new current node to the checked list
- For each node connected to the current node, recalculate its distance and precursor as follows:
   - If the sum of the distance to the current node and the weight of the connecting edge is less than the distance of the node, set its distance to this new distance and its precursor to the current node.
   - otherwise leave it as is.

When the algorithm terminates, we'll have an updated list of 'weighted nodes', where each such node has an optimal precursor that miminimizes the distance from the start node.  We can find an optimal path to any node by tracing back through the sequence of precursors.

Running the Program
-------------------

- install the [haskell platform](http://www.haskell.org/platform/)
- `$ ghc --make dijkstra`
- run interactively like this: `$ ./dijkstra`
- pipe it some input like this: `$ cat data | ./dijkstra`

The sample input file (data) is structured like this:
- a bunch of lines, each representing an edge of a weighted graph by two integers (nodes) and a float (the weight)
- a blank line, marking the end of the graph definition
- a line with a single integer representing a start node
- any number of lines representing end nodes of interest
