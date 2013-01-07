#About

This is the project for the Functional Programming course at the VUB.

The assignment of this project can be found in the Assignment folder of this repo. A short version is presented below

#Goal

##Part 1
The goal of this project is the implementation of a graph library in Haskell. Multiple implementations of a single type class should be implemented, it should also be possible to read and create a graph from a string. Afterwards, a few algorithms that work on this graph should be implemented. The dijkstra algorithm and the force-directed layout algorithm.
Finally, it should be possible to convert a graf into PsTricks code.

###Status

* Graph type class implementation
	* Adjacencymatrix representation: √
	* Binary search tree representation: √
	* Adjacencymatrix read: √
	* Binary search tre read: √
* Graph algorithms
	* Dijkstra: √
	* force-directed layout: √
* PSP: √

##Part2
The goal of part 2 is the implentation of a pacman simulation. Pacman and the ghosts should run around on a graph, nodes are places, while edges are tunnels connecting these places. It should be possible to parse a DOT file that specifies the layout of the graph and the initial positions of pacman and the ghosts.
The ghosts should attempt to block pacman by blocking the path to his goal node, each ghost should have a dedicated thread that calculates it's route. Pacman should try to avoid the ghosts and reach his goal node.
Finally, the course of a game has to be exported to a JSON file that can be rendered to show the flow of a game.

###Status

* Dot Parser: √
* Ghost algorithm: √
* Ghost threading: √
* Pacman strategy: √
* Exporting the game: x