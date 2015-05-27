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
import org.jgrapht.experimental.subgraphisomorphism.VF2SubgraphIsomorphismInspector;
import org.jgrapht.graph.DefaultEdge;

import java.util.ArrayList;
import java.util.Set;

public class Graph extends SmallGraph {

    //to retrieve component count and mask
    private ConnectivityInspector<Integer, DefaultEdge> connectivityInspector;

    private boolean is_bottom;

    /** Creates a new graph without nodes. */
    public Graph(){
        this(0);
    }

    /** Creates a new graph with <tt>n</tt> nodes. */
    public Graph(int n){
        super();

        connectivityInspector = new ConnectivityInspector<>(this);

        /* make connectivity inspector be notified at changes */
        this.addGraphListener(connectivityInspector);

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

        clear();

        /* fill it with n nodes */
        for(int i=0; i<n; i++) {
            this.addVertex(i);
        }
    }
    
    /** Copy the contents of gs into this. */
    private void copyFrom(SmallGraph gs){

        /* bottom graphs are empty */
        if ( ((Graph)gs).is_bottom ) {
            is_bottom = true;
            return;
        }

        clear();

        Graphs.addGraph(this, gs);
    }

    public void copyFromComplement() {
        super.copyFromComplement();

        /* clear this */
        clear();

        /* Then rebuild from complement. */
        Graphs.addAllVertices(this, complement.vertexSet());

        for(int from : complement.vertexSet()) {
            for (int to : complement.vertexSet()) {
                if ( from != to && complement.getEdge(from, to) == null ) {
                    this.addEdge(from, to);
                }
            }
        }
    }

    private void clear() {
        this.removeAllVertices(new ArrayList<>(this.vertexSet()));
    }

    public boolean getBottom(){
        return is_bottom;
    }
    
    /** Counts the nodes in this graph. */
    public int countNodes(){
        return this.vertexSet().size();
    }
    
    /** Counts the edges in this graph. */
    public int countEdges(){
        return this.edgeSet().size();
    }
    
    /** Adds a node to the graph. */
    public void addNode(){
        this.addVertex(this.vertexSet().size());
    }
    
    // --------------------------------------------------------------
    public boolean isIsomorphic(Graph g){
        final boolean DEBUG = false;

        /* check if one of both is the bottom-graph*/
        if (g.is_bottom || is_bottom) {
            return false;
        }

        /* check if number of nodes and edges are equal */
        if( this.vertexSet().size() != g.vertexSet().size() || this.edgeSet().size() != g.edgeSet().size() ) {
            return false;
        }

        //TODO: do we need this? Maybe this is only overhead... (tassilo :D)?
        if (connectivityInspector.connectedSets().size() != g.connectivityInspector.connectedSets().size()) {
            return false;
        }

        /* after checked for equal edge count, sub-isomorphism is the same as isomorphism */
        return new VF2SubgraphIsomorphismInspector<>(this, g).isSubgraphIsomorphic();
    }


    /* liefert true, wenn g ein von this induzierter Teilgraph ist */
    public boolean isSubIsomorphic(Graph g) {
        return new VF2SubgraphIsomorphismInspector<>(this, g).isSubgraphIsomorphic();
    }

    public int getComponents(){
        return connectivityInspector.connectedSets().size();
    }

    /* returns the set of nodes in the i-th component */
    public Set<Integer> getComponent(int num){
        return connectivityInspector.connectedSets().get(num);
    }
}

/* EOF */
