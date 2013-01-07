What's new?
===========

Completly new code can be found in the following files:

Graph/TreeSearch.hs
Pacman/*
Project.hs

Changes to previously existing code
===================================

Graph/Kernel.hs Some new methods were added to the graph kernel, these were added to the class declaration
Graph/Dijkstra.hs The dijkstra algorithm now also includes the first node of the path.
Graph/LatexDraw.hs This file contains the code that used to be found in Project.hs. It contains the toPsP implementation
Graph/Layout.hs A constant has been changed to make the code converge a bit faster
Infinity.hs The infinity abstraction is now part of the top level of the project to be able to reuse it. The implementation remains the same as the previous version

What didn't make it into the project?
=====================================

Only a part of the project was completed due to time constraints and other projects. The parser, ghost strategy, ghost threads and the pacman strategy are implemented. Actually running the simulation and exporting this to the specified output format were not completed.

To run the existing code, you can call the parseFile function with the path to your DOT file, this will return a PacmanField with the necessary data to start the game. Afterwards you can call the runGame function with this PacmanField, it will calculate the paths that pacman and the ghosts will take.