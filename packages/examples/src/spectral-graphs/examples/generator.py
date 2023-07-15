import networkx as nx


def basic_substance(nx_func, *args) -> str:
    sub = ''
    g = nx_func(*args)
    for node in g.nodes():
        sub += f'Node n{node}\n'
    for i, (a, b) in enumerate(g.edges()):
        sub += f'Edge e{i} := MakeEdge(n{a}, n{b})\n'
    return sub


def _grid_node_str(node: tuple[int]) -> str:
    return 'n' + 'x'.join(str(i) for i in node)


def grid_substance(nx_func, *args) -> str:
    g = nx_func(*args)
    sub = ''
    for node in g.nodes():
        sub += f'Node {_grid_node_str(node)}\n'
    for i, (a, b) in enumerate(g.edges()):
        sub += f'Edge e{i} := MakeEdge({_grid_node_str(a)}, {_grid_node_str(b)})\n'
    return sub


if __name__ == '__main__':
    # Usage examples:
    # sub = basic_substance(nx.truncated_cube_graph)
    sub = grid_substance(nx.hexagonal_lattice_graph, 9, 6, True)

    # See documentation:
    # https://networkx.org/documentation/stable/reference/generators.html

    # Print or save to file:
    print(sub)