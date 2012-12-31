What's new?
===========

Completly new code can be found in the following files:

Graph/TreeSearch.hs
Pacman/*
Project.hs

Changes to previously existing code
===================================

Graph/Kernel.hs Some new methods were added to the graph kernel, these were added to the class declaration
Graph/Dijkstra.hs The dijkstra algorithm now also includes the first node of the path
Graph/LatexDraw.hs This file contains the file that used to be found in Project.hs. It contains the toPsP implementation
Graph/Layout.hs A constant has been changed to make the code converge a bit faster
Infinity.hs The infinity abstraction is now part of the top level of the graph to be able to reuse it. The implementation remains the same as the previous version

What didn't make it into the project?
=====================================

Some bugs still exist in running a game. Furthermore, the functionality to export simulations has not been implemented due to time constraints.