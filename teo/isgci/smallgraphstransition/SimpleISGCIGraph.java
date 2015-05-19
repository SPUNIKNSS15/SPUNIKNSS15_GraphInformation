package teo.isgci.smallgraphstransition;

import org.jgrapht.EdgeFactory;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleGraph;
import teo.isgci.smallgraph.Graph;

/**
 * Created by dennis on 19.05.15.
 */
public class SimpleISGCIGraph extends SimpleGraph<Integer,DefaultEdge> {
    public SimpleISGCIGraph(Graph graph) {
        super(DefaultEdge.class);

        for(int i = 0; i<graph.countNodes(); i++) {
            this.addVertex(i);
        }

        for(int i = 1; i<graph.countNodes(); i++) {
            for (int j = 0; j < i; j++) {
                if (graph.getEdge(i,j)) {
                    this.addEdge(i,j);
                }
            }
        }
    }
}
