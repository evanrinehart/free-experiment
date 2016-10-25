module Maze where

type Real = Double

newtype VertexId = VId Int
newtype LinkId = LId Int
data Link = Lk LinkId VertexId VertexId Real
data Maze = Mz [VertexId] [Link]
data Location = LcV VertexId | LcL LinkId Real
data Segment = Sg Location Location
data Path = POne Segment | PMany Segment Path
data Motion = Mo { moPath :: Path, moSpeed :: Real }

