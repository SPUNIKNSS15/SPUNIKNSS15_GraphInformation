/*
 * Represents a (induced sub)graph.
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.smallgraph;
import org.jgrapht.Graphs;
import org.jgrapht.alg.ConnectivityInspector;
import org.jgrapht.alg.VertexCovers;
import org.jgrapht.experimental.subgraphisomorphism.VF2SubgraphIsomorphismInspector;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.ListenableUndirectedGraph;
import org.jgrapht.graph.SimpleGraph;
import java.util.ArrayList;
import java.util.Set;

public class Graph extends SmallGraph {

    //to retrieve component count and mask
    private ConnectivityInspector<Integer, DefaultEdge> connectivityInspector;
    private ListenableUndirectedGraph<Integer, DefaultEdge> graph;
    private boolean is_bottom;
    private ListenableUndirectedGraph<Integer, DefaultEdge> complement;

    /** Creates a new graph without nodes. */
    public Graph(){
        this(0);
    }

    /** Creates a new graph with <tt>n</tt> nodes. */
    public Graph(int n){
        super();
        graph = new ListenableUndirectedGraph<Integer, DefaultEdge>( new SimpleGraph<Integer, DefaultEdge>(DefaultEdge.class));

        connectivityInspector = new ConnectivityInspector<>(this.graph);
        /* make connectivity inspector be notified at changes */
        this.graph.addGraphListener(connectivityInspector);
        addNodesCount(n);
    }

    public Graph(Graph g){
        this(0);
        copyFrom(g);
    }

    /**
     * Set the nodecount of this to n.
     * Any previous nodes/edges are lost!
     */
    public void addNodesCount(int n) {
        is_bottom = false;
        graph = new ListenableUndirectedGraph<Integer, DefaultEdge>( new SimpleGraph<Integer, DefaultEdge>(DefaultEdge.class));
        clear();

        /* fill it with n nodes */
        for(int i=0; i<n; i++) {
            graph.addVertex(i);
        }

        /* make connectivity inspector be notified at changes */
        connectivityInspector = new ConnectivityInspector<>(this.graph);
        this.graph.addGraphListener(connectivityInspector);

    }
    
    /** Copy the contents of gs into this. */
    private void copyFrom(Graph gs){

        /* bottom graphs are empty */
        if ( gs.is_bottom ) {
            is_bottom = true;
            return;
        }

        clear();
        Graphs.addGraph(graph, gs.graph);
    }

    public void copyFromComplement() {

        super.copyFromComplement();

        /* clear this */
        clear();

        /* Then rebuild from complement. */

        Graphs.addAllVertices(graph, ((Graph)complement).graph.vertexSet());

        for(int from : ((Graph)complement).graph.vertexSet()) {
            for (int to : ((Graph)complement).graph.vertexSet()) {
                if ( from != to && ((Graph)complement).graph.getEdge(from, to) == null ) {
                    graph.addEdge(from, to);
                }
            }
        }
    }

    public void addEdge(int x, int y) { graph.addEdge(new Integer(x), new Integer(y)); }

    public boolean getEdge(int x, int y) { return graph.containsEdge(x,y); }

    public Set<Integer> getVertexCover() { return VertexCovers.findGreedyCover(graph); }

    public int degree(int v) { return graph.degreeOf(v); }

    private void clear() {
        graph.removeAllVertices(new ArrayList<>(graph.vertexSet()));
    }

    public boolean getBottom(){
        return is_bottom;
    }
    
    /** Counts the nodes in this graph. */
    public int countNodes(){
        return graph.vertexSet().size();
    }
    
    /** Counts the edges in this graph. */
    public int countEdges(){
        return graph.edgeSet().size();
    }


    
    /** Adds a node to the graph. */
    public void addNode(){
        graph.addVertex(graph.vertexSet().size());
    }
    
    // --------------------------------------------------------------
    public boolean isIsomorphic(Graph g){
        final boolean DEBUG = false;

        /* check if one of both is the bottom-graph*/
        if (g.is_bottom || is_bottom) {
            return false;
        }

        /* check if number of nodes and edges are equal */
        if( graph.vertexSet().size() != g.graph.vertexSet().size() || graph.edgeSet().size() != g.graph.edgeSet().size() ) {
            return false;
        }

        //TODO: do we need this? Maybe this is only overhead... (tassilo :D)?
        if (connectivityInspector.connectedSets().size() != g.connectivityInspector.connectedSets().size()) {
            return false;
        }

        /* after checked for equal edge count, sub-isomorphism is the same as isomorphism */
        return new VF2SubgraphIsomorphismInspector<>(graph, g.graph).isSubgraphIsomorphic();
    }


    /* liefert true, wenn g ein von this induzierter Teilgraph ist */
    public boolean isSubIsomorphic(Graph g) {
        return new VF2SubgraphIsomorphismInspector<>(graph, g.graph).isSubgraphIsomorphic();
    }

    public int getComponents(){
        return connectivityInspector.connectedSets().size();
    }

    /* returns the set of nodes in the i-th component */
    public Set<Integer> getComponent(int num){
        return connectivityInspector.connectedSets().get(num);
    }

    public ListenableUndirectedGraph<Integer, DefaultEdge> getGraph() { return this.graph; }
}
/* EOF */
