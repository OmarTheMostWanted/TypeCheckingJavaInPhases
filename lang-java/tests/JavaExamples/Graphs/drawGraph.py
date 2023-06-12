import networkx as nx
import matplotlib.pyplot as plt
import sys

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

def draw_graph2(edges, sinks):
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
    pos = nx.kamada_kawai_layout(G)
    colors = [G.nodes[n]['color'] for n in G]
    nx.draw(G, pos, node_color=colors, with_labels=True)
    edge_labels = nx.get_edge_attributes(G,'label')
    nx.draw_networkx_edge_labels(G,pos,edge_labels=edge_labels)
    plt.show()

def draw_graph3(edges, sinks):
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
    for node in G.nodes:
        if G.nodes[node]['color'] == 'lightcoral':
            pos[node][1] += 0.1
    colors = [G.nodes[n]['color'] for n in G]
    nx.draw(G, pos, node_color=colors, with_labels=True)
    edge_labels = nx.get_edge_attributes(G,'label')
    nx.draw_networkx_edge_labels(G,pos,edge_labels=edge_labels)
    plt.show()


def draw_graph4(edges, sinks):
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
    for node in G.nodes:
        if G.nodes[node]['color'] == 'lightcoral':
            pos[node][1] += 0.1
    colors = [G.nodes[n]['color'] for n in G]
    nx.draw(G, pos, node_color=colors, with_labels=True, font_size=16)
    edge_labels = nx.get_edge_attributes(G,'label')
    nx.draw_networkx_edge_labels(G,pos,edge_labels=edge_labels)
    plt.show()


def draw_graph5(edges, sinks):
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
    ax = plt.gca()
    for node in G.nodes:
        if G.nodes[node]['color'] == 'lightcoral':
            pos[node][1] += 0.1
            x, y = pos[node]
            bbox = {'fc': 'lightcoral', 'boxstyle': 'round,pad=0.2'}
            t = ax.text(x, y, node, ha='center', va='center', bbox=bbox)
            bb = t.get_bbox_patch()
            bb.set_boxstyle("square", pad=0.3)
        else:
            x, y = pos[node]
            circle = plt.Circle((x,y), radius=0.05, color='lightblue')
            ax.add_patch(circle)
            ax.text(x, y+0.1, node, ha='center', va='center')
    for edge in G.edges:
        src, dest = edge
        x1, y1 = pos[src]
        x2, y2 = pos[dest]
        ax.annotate("",
                    xy=(x1,y1), xycoords='data',
                    xytext=(x2,y2), textcoords='data',
                    arrowprops=dict(arrowstyle="->",
                                    connectionstyle="arc3"))
        label = G.edges[src, dest]['label']
        x_mid = (x1 + x2) / 2
        y_mid = (y1 + y2) / 2
        ax.text(x_mid-0.05,y_mid-0.05,label)
    ax.set_xlim(-1.5, 1.5)
    ax.set_ylim(-1.5, 1.5)
    plt.axis('off')
    plt.show()

def read_input_from_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()

    edges_section = False
    sinks_section = False
    edges = []
    sinks = []

    for line in lines:
        line = line.strip()
        if line == 'edges:':
            edges_section = True
            sinks_section = False
        elif line == 'sinks:':
            edges_section = False
            sinks_section = True
        elif edges_section:
            edges.append(line)
        elif sinks_section:
            sinks.append(line)

    return edges, sinks



if len(sys.argv) != 2:
    print(f"Usage: {sys.argv[0]} INPUT_FILE")
else:
    input_file = sys.argv[1]
    edges, sinks = read_input_from_file(input_file)
    draw_graph4(edges,sinks) 

    # best is 3
