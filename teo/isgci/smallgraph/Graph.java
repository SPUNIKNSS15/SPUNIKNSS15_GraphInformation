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
import java.util.HashSet;
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

        /* add all edges and vertices gs to this *//*
        this.addNodesCount(gs.vertexSet().size());

        for (int from : gs.vertexSet()) {
            for (int to : gs.vertexSet()) {
                if (gs.getEdge(from, to) != null) {
                    this.addEdge(from, to);
                }
            }
        }*/
    }

    public void copyFromComplement() {
        super.copyFromComplement();

        /* clear this */
        clear();

        /* Then rebuild from complement. */
        Graphs.addAllVertices(this, complement.vertexSet());

        /*for(int i = 1; i<complement.vertexSet().size(); i++) {
            for (int j = 0; j < i; j++) {
                if (complement.getEdge(i,j) == null) {
                    this.addEdge(i,j);
                }
            }
        }*/

        for(int from : complement.vertexSet()) {
            for (int to : complement.vertexSet()) {
                if ( from != to && complement.getEdge(from, to) == null ) {
                    this.addEdge(from, to);
                }
            }
        }
    }

    private void clear() {
        /*
        for (int i = 0; i < this.vertexSet().size(); i++) {
            this.removeVertex(0);
        }
        */

        this.removeAllVertices(new ArrayList<>(this.vertexSet()));

        if (edgeSet().size() != 0 || vertexSet().size() != 0) {
            System.out.println("Graph was not empty after clear. It has " + edgeSet().size() + " edges and " + vertexSet().size() + " nodes.");
        }

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

    /**
     * REIMPLEMENTED WITH:
     *  -org.jgrapht.graph.UndirectedSubgraph
     *
     * @deprecated
     * parameter changes from int to V
     *
     * Returns the degree of the node at index <tt>v</tt> in the subgraph
     * induced by <tt>mask<tt>*/
    public int degree(int v, boolean mask[]){

        int degree = 0;
        /* count edges of activated nodes */
        for (int toNode : this.vertexSet()) {
            if (mask[toNode] && this.getEdge(v, toNode) != null) {
                degree++;
            }
        }
        return degree;
    }
    
    /** Adds a node to the graph. */
    public void addNode(){
        this.addVertex(this.vertexSet().size());
    }

    /*@Override
    public boolean removeVertex(Integer vertexToDelete) {

        *//* delete last vertex -> no shifting required *//*
        if (vertexToDelete == this.vertexSet().size() - 1) {
            return super.removeVertex(vertexToDelete);
        }
        *//* delete vertex in between -> replace vertexToDelete with biggest vertex *//*
        this.removeAllEdges(new ArrayList<DefaultEdge>(this.edgesOf(vertexToDelete)));
        
        int biggest = this.vertexSet().size() - 1;
        
        *//* move edges of biggest vertex to gap and remove biggest then *//*
        for (int v : this.vertexSet()) {
            if (this.getEdge(biggest, v) != null) {
                this.addEdge(vertexToDelete, v);
            }
        }
        return super.removeVertex(biggest);
    }*/
    
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

        /* after checked for equal edge count, sub-isomorphy is the same as isomorphy */
        return (new VF2SubgraphIsomorphismInspector<Integer, DefaultEdge>(this, g)).isSubgraphIsomorphic();
    }


    /* liefert true, wenn g ein von this induzierter Teilgraph ist */
    public boolean isSubIsomorphic(Graph g){
        return (new VF2SubgraphIsomorphismInspector<Integer, DefaultEdge>(this, g)).isSubgraphIsomorphic();
    }

    public int getComponents(){
        return connectivityInspector.connectedSets().size();
    }

    /* liefert eine Maske zurück, die angibt welche Knoten in der i-ten
     * Komponente enthalten sind */
    public Set<Integer> getComponent(int num){
        return connectivityInspector.connectedSets().get(num);
    }
}

/* EOF */
