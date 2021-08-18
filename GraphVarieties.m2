directedSpanningTree = method()
directedSpanningTree(Digraph) := Digraph => G -> (
  s = (sources(G))#0;
  V = vertices(G);
  E = {};
  visited = {s};
  Q = priorityQueue {s};
  while length(Q) != 0 do (
    w = pop Q;
    for e in findPaths(G, w, 1) do (
      if not any(visited, i -> (i == (e#1))) then (
        visited = append(visited, (e#1));
        insert(Q, (e#1));
        E = append(E, e);
        );
      );
  );
  return digraph(V, E);
  )

fundamentalCycle = method()
fundamentalCycle(List, Graph) := Graph => (e, undirectedT) -> (
  v = (e#0);
  w = (e#1);
  dist = distance(undirectedT, w, v);
  for p in findPaths(undirectedT, w, dist) do (
    if ((last p) == v) then (
      cycleEdges = {};
      for idx to (#p-2) do (
        cycleEdges = append(cycleEdges, take(p, {idx,idx+1}));
        );
      return digraph(p, cycleEdges);
      );
    );
  )

affinePictureSpace = method()
affinePictureSpace(Digraph) := Digraph => G -> (
  R = CC;
  for e in edges(G) do (R = R[m_e]);
  for v in vertices(G) do (R = R[x_v]);
  T = directedSpanningTree(G);
  S = edges(G);
  for f in edges(T) do (
    S = delete(f, S);
    );
  L = {};
  for e in S do (
    z = fundamentalCycle(e, underlyingGraph T);
    P = 0_R;
    for f in edges(T) do (
      if any(edges(z), i -> (i == f)) then (P = P+((m_e-m_f)*(x_(f#1)-x_(f#0))))
      else if any(edges(z), i -> (i == {f#1,f#0})) then (P = P+((m_f-m_e)*(x_(f#1)-x_(f#0))));
      );
      L = append(L, P);
    );
    return ideal(L);
  )
