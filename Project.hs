-- Layout.hs
-- Mathijs Saey
-- This module contains the top level of the project

import Pacman.Base
import Pacman.DotParser

filePath = "pacmanfield.dot"
field = parseFile filePath
