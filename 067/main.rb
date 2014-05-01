require "maffs"

# Euler #67:

triangle = File.read("067/triangle.txt").split(/\n/).map { |l|
  l.scan(/\d+/).map { |s| TriangleNode.new s.to_i }
}

p TriangleNode.compute triangle
