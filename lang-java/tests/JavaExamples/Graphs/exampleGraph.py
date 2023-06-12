import networkx as nx
import matplotlib.pyplot as plt

def draw_graph(edges, sinks):
    G = nx.DiGraph()
    for edge in edges:
        src, label, dest = edge.split()
        G.add_edge(src, dest, label=label[1])
        G.nodes[src]['color'] = 'lightblue'
        G.nodes[dest]['color'] = 'lightblue'
    for sink in sinks:
        src, label, sink_label, *rest = sink.split()
        sink_label = f"{sink_label} {' '.join(rest)}"
        G.add_node(sink_label, color='lightcoral')
        G.add_edge(src, sink_label, label=label[1:3])
    pos = nx.spring_layout(G)
    colors = [G.nodes[n]['color'] for n in G]
    nx.draw(G, pos, node_color=colors, with_labels=True)
    edge_labels = nx.get_edge_attributes(G,'label')
    nx.draw_networkx_edge_labels(G,pos,edge_labels=edge_labels)
    plt.show()

def read_input_from_file(filename):

    return edges, sinks

edges, sinks = read_input_from_file('input.txt')

draw_graph(edges,sinks)
