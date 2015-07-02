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
import org.jgrapht.alg.isomorphism.VF2GraphIsomorphismInspector;
import org.jgrapht.alg.isomorphism.VF2SubgraphIsomorphismInspector;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.ListenableUndirectedGraph;
import org.jgrapht.graph.SimpleGraph;

import java.util.ArrayList;
import java.util.Set;

public class Graph extends SmallGraph {


    /**
     * to retrieve component count and mask
     * TODO: describe connectivityInspector, graph, is_bottom
     * @connectivityInspector takes care of connected components
     * @graph
     * @is_bottom
     */
    private ConnectivityInspector<Integer, DefaultEdge> connectivityInspector;
    private ListenableUndirectedGraph<Integer, DefaultEdge> graph;
    private boolean is_bottom;



    /**
     * Creates a new graph without nodes.
     */
    public Graph(){
        this(0);
    }


    /**
     * Creates a new graph with <tt>n</tt> nodes.
     * @param n number of nodes
     */
    public Graph(int n){
        super();
        graph = new ListenableUndirectedGraph<Integer, DefaultEdge>( new SimpleGraph<Integer, DefaultEdge>(DefaultEdge.class));

        connectivityInspector = new ConnectivityInspector<>(this.graph);
        /* make connectivity inspector be notified at changes */
        this.graph.addGraphListener(connectivityInspector);
        addNodesCount(n);
    }

    /**
     * build the graph out of given Graph <tt>g</tt>
     * @param g Graph to be used
     */
    public Graph(Graph g){
        this(0);
        copyFrom(g);
    }

    /**
     * clears the Graph
     */
    private void clear() {
        graph.removeAllVertices(new ArrayList<>(graph.vertexSet()));
    }

    /**
     * Copy the contents of gs into this.
     * @param gs graph whose content should be copied
     */
    private void copyFrom(Graph gs){

        /* bottom graphs are empty */
        if ( ((Graph)gs).is_bottom ) {
            is_bottom = true;
            return;
        }

        clear();
        Graphs.addGraph(graph, ((Graph)gs).graph);
    }

    /**
     *  Builds a new Graph containing the complement
     *  @return a new instance of Graph, containing the complement of this.
     */
    public Graph buildComplement() {
        Graph c = (Graph) super.buildComplement();

        Graphs.addAllVertices(c.graph, this.graph.vertexSet());
        for(int from : this.graph.vertexSet()) {
            for(int to : this.graph.vertexSet()) {
                if ( from != to && this.graph.getEdge(from, to) == null) {
                    c.addEdge(from, to);
                }
            }
        }

        return c;
    }



    /**
     *
     * @return a vertexCover of the graph
     */
    public Set<Integer> getVertexCover() { return VertexCovers.findGreedyCover(graph); }

    /**
     * Set the nodecount of this to <tt>n</tt> .
     * Any previous nodes/edges are lost!
     * @param n nodecount
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

    /**
     * Adds a node to the graph.
     */
    public void addNode(){
        graph.addVertex(graph.vertexSet().size());
    }

    /**
     * adds an Edge between the two given nodes
     *
     * @param x node 1
     * @param y node 2
     */
    public void addEdge(int x, int y) { graph.addEdge(new Integer(x), new Integer(y)); }

    /**
     * checks if the edge between the two given nodes exists
     *
     * @param x node 1
     * @param y node 2
     * @return true if Edge(x,y) is contained, else false
     */
    public boolean getEdge(int x, int y) { return graph.containsEdge(x,y); }

    /**
     * computes the degree of a given node <tt>v</tt>
     *
     * @param v node whose degree should be computes
     * @return degree of node <tt>v</tt>
     */
    public int degree(int v) { return graph.degreeOf(v); }


    /**
     * Counts the nodes in this graph.
     * @return number of nodes
     */
    public int countNodes(){
        return graph.vertexSet().size();
    }

    /**
     * Counts the edges in this graph.
     * @return number of edges in this graph
     */
    public int countEdges(){
        return graph.edgeSet().size();
    }

    /**
     *
     * @param g
     * @return
     */
    public boolean isIsomorphic(Graph g){

        /* check if one of both is the bottom-graph*/
        if (g.is_bottom || is_bottom) {
            return false;
        }

        return new VF2GraphIsomorphismInspector<>(graph, g.graph).isomorphismExists();
    }

    /**
     * checks if a graph <tt>g</tt> is induced Subgraph
     *
     * @param g graph to be checked
     * @return true if g is induced Subgraph, else false
     */
    public boolean isSubIsomorphic(Graph g) {
        return new VF2SubgraphIsomorphismInspector<>(graph, g.graph).isomorphismExists();
    }

    /**
     *
     * @return is_Bottom
     */
    public boolean getBottom(){
        return is_bottom;
    }

    /**
     *
     * @return number of components
     */
    public int getComponents(){
        return connectivityInspector.connectedSets().size();
    }


    /**
     * returns the set of nodes in the <tt>num</tt>-th component
     *
     * @param num number of component
     * @return set of nodes in the <tt>num</tt>-th component
     */
    public Set<Integer> getComponent(int num){
        return connectivityInspector.connectedSets().get(num);
    }

    /**
     *
     * @return the graph from this class
     */
    public ListenableUndirectedGraph<Integer, DefaultEdge> getGraph() { return this.graph; }
}
/* EOF */
