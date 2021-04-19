system('lua54 proc/hades.lua > output/hades.dot')
system('cat output/hades.dot')
#system('dot output/hades.dot -Ktwopi -Tpng > output/hades.png')
svg = `dot output/hades.dot -Ktwopi -Tsvg`
wrapper ||= File.read(File.dirname(__FILE__) + '/wrapper.html')
html = wrapper.sub('#svg#', svg)
File.write('output/hades.html', html)
p 'done'

=begin
dot − filter for drawing directed graphs
neato − filter for drawing undirected graphs
twopi − filter for radial layouts of graphs
circo − filter for circular layout of graphs
fdp − filter for drawing undirected graphs
sfdp − filter for drawing large undirected graphs
patchwork − filter for squarified tree map
=end
