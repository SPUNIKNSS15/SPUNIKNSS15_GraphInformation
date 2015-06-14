/*
 * Represents a configuration of graphs
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.smallgraph;

import org.jgrapht.EdgeFactory;
import org.jgrapht.graph.ClassBasedEdgeFactory;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.ListenableUndirectedGraph;
import org.jgrapht.graph.SimpleGraph;

import java.util.*;

/**
 * A Configuration consists of a base graph and a set
 * of optional edges (OPTEDGE). All graphs which can be built by
 * adding arbitrary subsets of those edges to the base graph
 * are part of the Configuration.
 *
 * Alternatively, a set of forbidden edges (NONEDGES) can
 * be specified, which are the complement of the optional edges.
 *
 * Be careful not to mix both approaches, as no validity checks
 * are made.
 */
public class Configuration extends SmallGraph{

    private ListenableUndirectedGraph<Integer, DefaultEdge> base;
    private ListenableUndirectedGraph<Integer, DefaultEdge> optEdges;
    private ListenableUndirectedGraph<Integer, DefaultEdge> nonEdges;

    final static int EDGE = 1;
    final static int NONEDGE = -1;
    final static int OPTEDGE = 0;
    final static int UNKNOWN = 2;

    /** Graphs contained in Configuration */
    private Vector<SmallGraph> contains;

    /**
     * Empty Configuration.
     */
    public Configuration(){
        this(0);
    }

    /**
     * Configuration with <tt>n</tt> nodes and no edges.
     * @param n number of nodes for initialization
     */
    public Configuration(int n){
        super();
        contains = null;
        addNodesCount(n);
    }

    /**
     * Creates a new Configuration, which is 'induced' by c.
     * This means copying all nodes which are specified by
     * the bitmask <tt>mask</tt> and adding all edges between those
     * nodes which exist in <tt>c</tt>.
     *
     * @param c Configuration which shall induce this
     * @param includedNodes determines which nodes should be taken over
     */
    public Configuration(Configuration c, Set<Integer> includedNodes) {
        super();
        addNodesCount(includedNodes.size());


        int k = -1;
        for (int i = 0; i < c.base.vertexSet().size(); i++) {

            if (includedNodes.contains(i)) {
                k++;
            } else {
                continue;
            }
            int l = -1;

            for (int j = 0; j < c.base.vertexSet().size(); j++) {
                if (includedNodes.contains(j)) {
                    l++;
                } else {
                    continue;
                }

                if (c.base.containsEdge(i, j)) {
                    base.addEdge(k, l);
                } else if (c.optEdges.containsEdge(i,j)) {
                    optEdges.addEdge(k,l);
                }
            }
        }
        contains = null;
        induced = null;
        link = null;
    }

    /**
     * Initialize with n nodes and reset the internal state.
     * Resets all edges to UNKNOWN.
     *
     * @param n number of nodes for initialization
     */
    public void addNodesCount(int n){
        base = new ListenableUndirectedGraph<>(new SimpleGraph<>(DefaultEdge.class));
        nonEdges = new ListenableUndirectedGraph<>(new SimpleGraph<>(DefaultEdge.class));
        optEdges = new ListenableUndirectedGraph<>(new SimpleGraph<>(DefaultEdge.class));
        for (int i = 0; i < n; i++) {
            base.addVertex(i);
            optEdges.addVertex(i);
            nonEdges.addVertex(i);
        }
        contains = null;
    }


    /**
     * @return the number of nodes in the Configuration
     */
    public int countNodes(){
        return base.vertexSet().size();
    }

    /**
     * Adds an edge to the Configuration.
     * <tt>type</tt> shows which edge should be added.
     */
    private void addEdge(int source, int dest, int type){
        if (type != EDGE && type != NONEDGE && type != OPTEDGE) {
            System.out.println("Wrong type of edge!");
            return;
        }
        if (source == dest) return;
        if (source<0 || dest<0 || source>=base.vertexSet().size() || dest>=base.vertexSet().size()) return;

        optEdges.removeEdge(source, dest);
        nonEdges.removeEdge(source, dest);
        base.removeEdge(source, dest);

        if (type == EDGE) {
            base.addEdge(source, dest);
        } else if (type == OPTEDGE) {
            optEdges.addEdge(source, dest);
        } else {
            nonEdges.addEdge(source, dest);
        }
    }

    /**
     * Adds a new edge to the base graph, from
     * <tt>a</tt> to <tt>b</tt>.
     *
     * @param a first node of the edge
     * @param b second node of the edge
     */
    public void addEdge(int a, int b){
        addEdge(a, b, EDGE);
    }

    /**
     * Adds a new forbidden edge.
     *
     * @param a first node of the edge
     * @param b second node of the edge
     */
    public void addNonedge(int a, int b){
        addEdge(a, b, NONEDGE);
    }

    /**
     * Adds a new optional edge.
     *
     * @param a first node of the edge
     * @param b second node of the edge
     */
    public void addOptedge(int a, int b){
        addEdge(a, b, OPTEDGE);
    }

    /**
     *
     * @param source first node of the edge
     * @param dest second node of the edge
     * @return returns the edge type
     */
    public int getEdge(int source, int dest){

        if (source == dest) {
            return NONEDGE;
        }

        if (base.containsEdge(source, dest)) {
            return EDGE;
        } else if (optEdges.containsEdge(source, dest)) {
            return OPTEDGE;
        } else if (nonEdges.containsEdge(source, dest)) {
            return NONEDGE;
        } else {
            return UNKNOWN;
        }
    }

    /**
     * Counts the edges of type <tt>type</tt>.
     *
     * @param type edge type
     * @return number of edges of type <tt>type</tt>
     */
    public int countEdges(int type){

        if (type == EDGE) {
            return base.edgeSet().size();
        } else if (type == OPTEDGE) {
            return optEdges.edgeSet().size();
        } else if (type == NONEDGE) {
            return nonEdges.edgeSet().size();
        } else {
            return (int)Math.pow(2, base.vertexSet().size())
                    - base.edgeSet().size() - optEdges.edgeSet().size() - nonEdges.edgeSet().size();
        }
    }

    /**
     *
     * @return number of normal edges
     */
    public int countEdges(){
        return countEdges(EDGE);
    }

    /**
     *
     * @return number of forbidden edges
     */
    public int countNonedges(){
        return countEdges(NONEDGE);
    }

    /**
     *
     * @return number of optional edges
     */
    public int countOptedges(){
        return countEdges(OPTEDGE);
    }



    /**
     * Calculates the degree of a node according to an edge type.
     *
     * @param v node index
     * @param type edge type to be counted
     * @return degree of type <tt>type</tt> of the node at index <tt>v</tt>
     */
    public int degree(int v, int type){
        boolean[] mask = new boolean[base.vertexSet().size()];
        Arrays.fill(mask, true);
        return degree(v, type, mask);
    }

    /**
     * Calculates the degree of a node in an induced configuration
     * according to an edge type.
     *
     * @param v node index
     * @param type edge type to be counted
     * @param mask which specifies the induced configuration
     * @return degree of type <tt>type</tt> of the node at index
     * <tt>v</tt> in the subgraph induced by <tt>mask<tt>
     */
    public int degree(int v, int type, boolean mask[]){
        if (v<0 || v>=base.vertexSet().size()) return -1; // illegal argument

        int n = 0;
        if (type == EDGE) {
            /* EDGE degree is just v's degree in base */
            for (int i = 0; i < base.vertexSet().size(); i++) {
                if( mask[i] && base.containsEdge(v, i)) {
                    n++;
                }
            }
            return n;

        } else if (type == OPTEDGE) {
            /* OPTEDGE degree is the amount of incident optedges */
            for (int i = 0; i < base.vertexSet().size(); i++) {
                if( mask[i] && optEdges.containsEdge(v, i))  {
                    n++;
                }
            }
            return n;

        } else if (type == NONEDGE) {
            /* NONEDGE degree is the amount of incident nonedges */
            for (int i = 0; i < base.vertexSet().size(); i++) {
                if( mask[i] && nonEdges.containsEdge(v, i)) {
                    n++;
                }
            }
            /* return n+1 because every node has a nonedge to itself, but this can't be expressed in our graphs */
            return n + 1;

        } else {
            /* UNKNOWN degree is the amount of nodes minus the EDGE, OPTEDGE and NONEDGE degree.
             * This is correct, as n-1 is the maximum degree of a node in a graph containing n nodes,
             * as we count the NONEDGE of a node to itself explicitely.
             */
            return base.vertexSet().size() - degree(v, EDGE, mask)
                    - degree(v, OPTEDGE, mask) - degree(v, NONEDGE, mask);
        }
    }

    /**
     * Calculates the normal-edge-degree of a node.
     *
     * @param v node index
     * @return the degree of the node at index <tt>v</tt>
     */
    public int degree(int v){
        return degree(v, EDGE);
    }

    /**
     * Calculates the normal-edge-degree of a node in an induced
     * configuration.
     *
     * @param v node index
     * @param mask which specifies the induced configuration
     * @return the degree of the node at index <tt>v</tt> in the subgraph
     * induced by <tt>mask<tt>
     */
    public int degree(int v, boolean mask[]){
        return degree(v, EDGE, mask);
    }

    /**
     * Calculates the optional-edge-degree of a node.
     *
     * @param v node index
     * @return the optDegree of the node at index <tt>v</tt>
     */
    public int optDegree(int v){
        return degree(v, OPTEDGE);
    }

    /**
     * Calculates the optional-edge-degree of a node in an induced
     * configuration.
     *
     * @param v node index
     * @param mask which specifies the induced configuration
     * @return the degree of the node at index <tt>v</tt> in the subgraph
     *   induced by <tt>mask<tt>
     */
    public int optDegree(int v, boolean mask[]){
        return degree(v, OPTEDGE, mask);
    }

    /**
     * Adds the graph <tt>elt</tt> to the list of contained
     * graphs.
     *
     * @param elt graph to be added
     *
     * TODO: why not private?
     */
    public void addContains(Graph elt){
        if (contains == null)
            contains = new Vector<SmallGraph>(2,2);
        if (!contains.contains(elt))
            contains.addElement(elt);
    }

    /**
     *
     * @return the list of contained graphs
     */
    public Vector<SmallGraph> getContains(){
        return contains;
    }

    /**
     * Creates a String representation of this Configuration.
     * edges displayed with '-'.
     * optional edges displayed with '='.
     *
     * @return the Configuration as a string
     */
    public String toString(){
        if(base.vertexSet().size() == 0)
            return "";

        int i,j;
        String s="{"+String.valueOf(base.vertexSet().size())+"} ";
        s += namesToString() + " ";
        /* These for loops are a reminder from the matrix implementation to preserve output order */
        for(i=0; i<base.vertexSet().size(); i++)
            for(j=0; j<i; j++) {
                /* containsEdge(i,j) returns true in an undirected graph, iff the edge (i, j) or (j, i) was added */
                if(base.containsEdge(i,j)) {
                    s += (j + " - " + i + "; ");
                /* whereas .equals (used by ArrayList.contains) returns true for (i, j) if and only if (i,j) was added */
                } else if(optEdges.containsEdge(i, j)) {
                    s += (j + " = " + i + "; ");
                }
            }
        return s;
    }

    /**
     * Adds <tt>g</tt> to the internal list of induced subgraphs.
     *
     * @param g the induced subgraph to be added
     *
     * TODO: why not private?
     */
    public void addInduced(Graph g){
        Vector<SmallGraph> innerVec = new Vector<SmallGraph>();
        innerVec.addElement(g);
        this.addInduced(innerVec);
    }

    /**
     * @return all representatives of the configuration, if less than 100
     */
    public Vector<Graph> getGraphs(){
        return getGraphs(100);
    }

    /**
     * Returns a list containing all graphs of the
     * Configuration.
     *
     * @param maxGraphs amount of graphs to calculate
     * @return a Vector containing the graphs or null if
     *    if more than <tt>maxGraphs</tt> were built
     *
     * TODO:
     *   - why Vector and not ArrayList ?
     */
    public Vector<Graph> getGraphs(int maxGraphs){
        final boolean DEBUG = false; /* auf true setzen für Debug-Ausgaben */
        /* because we have signed 32 bit Integers as bitmask for optional edges
         we restrict our calculations to 30 optional edges */
        final int maxOptEdges = 30;

        /* optEdgeList contains a representation of all optional edges,
        indexed by the occurence in the matrix from left to right,
        top down. */
        int optEdgeList[][] = new int[maxOptEdges][2];// x=[][0], y=[][1]
        int i, j, cntOpt = 0, allOptionalEdges;
        Vector<Graph> confGraphs = new Vector<Graph>();

        long ta = 0, te;

        if (DEBUG) {
            System.out.print("Name: " + getName() + "\n");
            System.out.print("  " + this.toString() + "\n");
            System.out.print("  Knoten: " + base.vertexSet().size() + "\n");
            ta = System.currentTimeMillis();
        }

        // fill optEdgeList array
        for (i=0; i<base.vertexSet().size()-1; i++)
            for (j=i+1; j<base.vertexSet().size(); j++)
                if (optEdges.containsEdge(i, j))  {
                    if (cntOpt == maxOptEdges){
                        if (DEBUG)
                            System.out.print("  mehr als " + maxOptEdges
                                    + " optionale Kanten\n\n");
                        return null;
                    }
                    optEdgeList[cntOpt][0] = i;
                    optEdgeList[cntOpt++][1] = j;
                }

        if (DEBUG) {
            System.out.print("  optionale Kanten: " + cntOpt + "\n");
            for (i=0; i < cntOpt; i++)
                System.out.print("    " + i + ": " + optEdgeList[i][0]
                                + " = " + optEdgeList[i][1] + "\n");
        }

        // vector to store all automorphisms (except identity)
        Vector Transformationen = new Vector();

        // there exist configurations without optional edges...
        if (cntOpt > 1) {
            // get vector of all permutations which are automorphisms
            Vector automorph = getAutomorphisms();

            if (DEBUG) {
                System.out.print("  Automorphismen: " + automorph.size() + "\n");
            }

            // iterate over all such permutations p ...
            for (i = 0; i < automorph.size(); i++) {
                int p[] = (int []) automorph.elementAt(i);

                /* trafo stores the respective permutation of the optional edges:
                 entry trafo[j] = k means, that the current automorphism p
                 maps an optional edge, namely optEdgeList[j] == (x, y) to another
                 optional edge, namely optEdgeList[k] == (p[x], p[y]) */
                int trafo[] = new int[cntOpt];

                if (DEBUG) {
                    System.out.print("    (");
                    for (j = 0; j < base.vertexSet().size(); j++)
                        System.out.print(" " + p[j] + " ");
                    System.out.print(")  ---> ");
                }
                loop: for (j=0; j < cntOpt; j++) {
                    int x_a = optEdgeList[j][0];
                    int y_a = optEdgeList[j][1];
                    int x_b = p[x_a];
                    int y_b = p[y_a];

                    /* Search the edge (x_b, y_b) in optEdgeList */
                    for (int k = 0; k < cntOpt; k++) {
                        if (optEdgeList[k][0] == x_b && optEdgeList[k][1] == y_b
                                    || optEdgeList[k][1] == x_b
                                    && optEdgeList[k][0] == y_b) {
                                trafo[j]=k;
                                continue loop;
                            }
                    }

                    /* If the permutation of edge j is not in optEdgeList
                     something goes utterly wrong. */
                    System.err.print("Denkfehler in Configuration."
                                    + "getGraphs()!!!\n");
                }

                /* transformation is the inverse permutation of trafo */
                int transformation[] = new int [cntOpt];
                for (j = 0; j < cntOpt; j++) {
                    transformation[trafo[j]] = j;
                }

                if (DEBUG) {
                    System.out.print("Trafo:  (");
                    for (j = 0; j < cntOpt; j++)
                        System.out.print(" " + transformation[j] + " ");
                    System.out.print(")");
                }

                /* add 'transformation' to Transformationen, if
                 it is not just the identity function */
                for (j = 0; j < cntOpt; j++) {
                    if (transformation[j] != j) {
                        Transformationen.addElement(transformation);
                        if (DEBUG)
                            System.out.print(" X");
                        break;
                    }
                }
            }
        }        /* if (cntOpt > 1) */

        if (DEBUG)
            System.out.print("  Kantenabbildungen: "
                            + Transformationen.size() + "\n");


        if (DEBUG) {
            te = System.currentTimeMillis();
            System.out.print("  Zeit bisher: " + (te - ta)/10 + "ms\n");
        }

        // the limit of *optionalMask* (see loop below)
        allOptionalEdges = 1 << cntOpt;

        // ASSERTION HERE: Transformationen contains all permutations of the optional
        // edges which are not just the identity

        // Create the base graph of this configuration.
        // It is used as a shape for all representatives,
        Graph shape = new Graph(base.vertexSet().size());
        for (i=0; i<base.vertexSet().size(); i++)
            for (j=i+1; j<base.vertexSet().size(); j++)
                if (base.containsEdge(i, j))
                    shape.addEdge(i, j);

        int zaehler = 0;

        /* *optionalMask* is used to represent all possible representatives of
         the Configuration */
        loop: for (int optionalMask=0; optionalMask < allOptionalEdges; optionalMask++) {

            int permutatedBitmask;

            /* getSmallerNumber: look at all isomorphic representatives using the automorphisms
             on the optional edges
             (was formerly the method getSmallerNumber) */
            for (int z = 0; z < Transformationen.size(); z++) {
                int permutation[] = (int []) Transformationen.elementAt(z);

                permutatedBitmask = 0;
                for (int bitPosition = 0; bitPosition < cntOpt; bitPosition++) {
                    if ((optionalMask & (1 << bitPosition)) != 0) {
                        permutatedBitmask |= (1 << permutation[bitPosition]);
                    }
                }

                // if permutedBitmask < optionalMask, then optionalMask is
                // isomorphic to a representative that we have already stored
                if (permutatedBitmask < optionalMask) {
                    continue loop;
                }
            }

            zaehler++;

            /* ASSERTION HERE: optionalMask specifies a representative
             which is *not* isomorphic to already constructed
             representatives

             construct the new representative from the base graph shape
             using optionalMask */
            Graph rep = new Graph(shape);
            for (j = cntOpt - 1; j >= 0; j--)
                if ((optionalMask & (1 << j)) != 0)
                    rep.addEdge(optEdgeList[j][0], optEdgeList[j][1]);

            /* check if new representative is isomorphic to any previously
             added graphs. This is still possible. (why?) */
            for (j = 0; j < confGraphs.size(); j++)
               if (confGraphs.elementAt(j).isIsomorphic(rep))
                   continue loop;        /* ein goto */

            /* new representative, can finally be added */
            confGraphs.addElement(rep);

            if (confGraphs.size() > maxGraphs) {
               /* zuviele, bottom-Graph erzeugen und raus */
               /*rep.setBottom();
               System.err.print("Erzeuge Bottom-Graph\n");
               break;*/

                /* zuviele, leere Liste zurückliefern */
                if (DEBUG) {
                    te = System.currentTimeMillis();
                     System.out.print("  Repräsentanten: >" + maxGraphs
                                     + " von " + allOptionalEdges + "\n");
                     System.out.print("  Zeit: " + (te - ta)/10 + "ms\n\n");
                }
                return null;
            }
        }

        if (DEBUG) {
            te = System.currentTimeMillis();
            System.out.print("  Repräsentanten: " + confGraphs.size()
                            + " (" + zaehler + ") von " + allOptionalEdges + "\n");
            System.out.print("  Zeit: " + (te - ta)/10 + "ms\n\n");
        }
        return confGraphs;
    }

    /**
     * TODO: comment this
     * @return
     */
    public Vector getAutomorphisms(){
        final boolean DEBUG = false;

        int i, j;
        boolean darf[][] = new boolean[base.vertexSet().size()][base.vertexSet().size()];
        boolean mask[][] = new boolean[2][base.vertexSet().size()];
        Vector ret = new Vector();

        for (i = 0; i < base.vertexSet().size(); i++) {
            mask[0][i] = true;
            mask[1][i] = true;
            for (j = 0; j < base.vertexSet().size(); j++)
                    darf[i][j] = true;
        }

        isIsomorphicIntern(this, darf, mask); /* rückgabe egal, da immer */
        /* zu sich selbst isomorph */

        Permutation perm = new Permutation(base.vertexSet().size(),darf);
        do {
            int p[] = perm.get();
            if (check(this, p))
            ret.addElement(p);
        } while (perm.next());

        if (DEBUG) {
            System.err.print(" getAutomorphisms(" + getName() + ")\n");
            System.err.print("  bijektive Funktionen: " + perm.count() + "\n");
            System.err.print("  Automorphismen: " + ret.size() + "\n");
        }

        return ret;
    }

    public int countAutomorphisms(){
        return getAutomorphisms().size();
    }

    public boolean isIsomorphic(Configuration c){
        /* check if numbers of nodex, Edges and non-Edges are equal */
        if (base.vertexSet().size()!=c.base.vertexSet().size() || countEdges()!=c.countEdges() ||
                          countNonedges()!=c.countNonedges())
            return false;

        int i, j;
        boolean darf[][] = new boolean[base.vertexSet().size()][base.vertexSet().size()];
        boolean mask[][] = new boolean[2][base.vertexSet().size()];

        for (i = 0; i < base.vertexSet().size(); i++) {
            mask[0][i] = true;
            mask[1][i] = true;
            for (j = 0; j < base.vertexSet().size(); j++)
                    darf[i][j] = true;
        }

        if (!isIsomorphicIntern(c, darf, mask))
                return false;

        Permutation perm = new Permutation(base.vertexSet().size(),darf);
        do {
            if (check(c, perm.get())) return true;
        } while (perm.next());

        return false;
    }

    /** Hilfsfunktion zur Bestimmung von <tt>darf<tt>.
     * In diesen Fall nicht-rekursiv, da der Aufwand dafür zu groß ist. Die
     * Matrix <tt>darf</tt> enthält an der Stell i,j eine 1, wenn der Knoten i
     * in <tt>this<tt> den gleichen Grad hat wie der Knoten j in <tt>g<tt>. In
     * mask ist hierbei gespeichert welcher Teil des Graphen bearbeitet werden
     * soll. In mask[0] beschreibt hierbei <tt>this<tt> und mask[1] <tt>g<tt>.
     */
    private boolean isIsomorphicIntern(
            Configuration c,
            boolean darf[][],
            boolean mask[][]){
        int i, j;
        int Grad[][] = new int[2][base.vertexSet().size()];
        int OptGrad[][] = new int[2][base.vertexSet().size()];
        boolean changed = false;

        /* Speichere in Grad[0][i] den Grad des Knoten i in der Configuration
         * this Analog für die Configuration c wenn der Index 1 gewählt ist.
         * Das ganze aber nur dann, wenn in der jeweiligen Maske das Flag für
         * den entsprechenden Knoten gesetzt ist. Nebenbei wird ermittelt, ob
         * die Gradzahlen in beiden Graphen übereinsstimmen.*/
        for (i=0; i < base.vertexSet().size(); i++) {
            int h;

            if (mask[0][i]) {
                Grad[0][i] = degree(i,mask[0]);
                OptGrad[0][i] = optDegree(i,mask[0]);
            }

            if (mask[1][i]) {
                Grad[1][i] = c.degree(i,mask[1]);
                OptGrad[1][i] = c.optDegree(i,mask[1]);
            }
        }

        /* jetzt darf[][] bearbeiten */
        for (i=0; i < base.vertexSet().size(); i++) {
            if (!mask[0][i])
                continue;

            for (j=0; j < base.vertexSet().size(); j++) {
                if (!mask[1][j])
                    continue;

                if ((Grad[0][i] != Grad[1][j]
                    || OptGrad[0][i] != OptGrad[1][j])
                    && darf[i][j]) {
                    changed = true;
                    darf[i][j] = false;
                }
            }
        }

        if (!changed)
            return true;       /* nix passiert */

        /* untersuchen, ob es Knoten gibt, für die es keine
         * Positionsmöglichkeiten gibt */
        for (i = 0; i < base.vertexSet().size(); i++) {
            if (mask[0][i]) { /* nur wenn in diesen Durchlauf eventuell */
                    /*angefaßt */
                boolean ziel = false;;
                for (j = 0; j < base.vertexSet().size(); j++) {
                    if (darf[i][j])
                        ziel = true;;
                }
                if (! ziel) 
                    return false;
            }
        }

        /* untersuchen, ob es Knoten gibt, für die es keine Quelle gibt */
        for (i = 0; i < base.vertexSet().size(); i++) {
            if (mask[1][i]) { /* nur wenn in diesen Durchlauf eventuell */
                    /* angefaßt */
                boolean quelle = false;;
                for (j = 0; j < base.vertexSet().size(); j++) {
                    if (darf[j][i])
                        quelle = true;;
                }
                if (! quelle)
                    return false;
            }
        }

        return true;
    }

    /**
     * Checks if g is an induced subgraph of the Configuration,
     * which means that all representative graphs of the Configuration
     * must induce g. When this fails, it is either not an induces subgraph
     * or it cannot be checked with this simple test
     *
     * @param g Graph to be checked
     * @return true, i
     */
    public boolean isInducedSubgraph(Graph g){
        /* check if /g/ is nonempty and not the bottom-graph */
        if (g == null || g.countNodes() == 0 || g.getBottom())
            return false;

        /* check if /g/ is "smaller" than /this/ */
        if (g.countNodes() > base.vertexSet().size() || g.countEdges() > countEdges())
            return false;

        /* /g/ hat einen Knoten */
        if (g.countNodes() == 1)
            return base.vertexSet().size() >= 1;    /* der K1 ist in jeder nichtleeren */
            /* Konfiguration enthalten */
        int i, j;

        /* /g/ hat zwei Knoten */
        if (g.countNodes() == 2) {
            if (g.degree(0) == 1) {
                /* /g/ ist ein K2 */
                for (i = 0; i < base.vertexSet().size() - 1; i++)
                    for (j = i + 1; j < base.vertexSet().size(); j++)
                        if (base.containsEdge(i,j))
                            return true;
            } else {
                /* /g/ ist ein 2K1 */
                for (i = 0; i < base.vertexSet().size() - 1; i++)
                    for (j = i + 1; j < base.vertexSet().size(); j++)
                        if (!base.containsEdge(i, j) && !optEdges.containsEdge(i, j) ) {
                            return true;
                        }
            }

            return false;   /* keine aufwändigeren Tests für diesen Fall */
        }


        /* Der Rumpf einer Konfiguration ist der Graph, der entsteht wenn man
         * alle optionalen Kanten wegläßt */
        Graph rumpf = new Graph(base.vertexSet().size());
        for (i = 0; i < base.vertexSet().size() - 1; i++)
            for (j = i + 1; j < base.vertexSet().size(); j++)
                if (base.containsEdge(i,j))
                    rumpf.addEdge(i, j);

        switch ( countOptedges() ) {
            case 0:
                /* keine optionale Kanten */
                return rumpf.isSubIsomorphic(g);

            case 1:
                /* eine optionale Kante -> beide Repräsentanten
                 * erzeugen und testen */
                if (! rumpf.isSubIsomorphic(g))
                        return false;

                for (i = 0; i < base.vertexSet().size() - 1; i++)
                    for (j = i + 1; j < base.vertexSet().size(); j++)
                        if ( optEdges.containsEdge(i,j))
                        {
                            rumpf.addEdge(i, j);
                            return rumpf.isSubIsomorphic(g);
                        }
        }

        if (rumpf.getComponents() > 1) {
            /* /rumpf/ nicht zusammenhängend. Teilkonfigurationen erzeugen und
             * Rekursion */

            for (i = 0; i < rumpf.getComponents(); i++) {
                Configuration c =
                        new Configuration(this, rumpf.getComponent(i));

                if (c.isInducedSubgraph(g))
                    return true;
            }

            if (g.getComponents() == 1) /* g zusammenhängend */
                return false;
        }

        /* jetzt Knoten entfernen, so daß keine optionalen Kanten
         * mehr vorkommen */
        {
            Graph h = new Graph(base.vertexSet().size());

            for (i = 0; i < base.vertexSet().size() - 1; i++)
                for (j = i + 1; j < base.vertexSet().size(); j++)
                    if ( optEdges.containsEdge(i, j)) {
                        h.addEdge(i, j);
                    }

            Set<Integer> vertexCover = h.getVertexCover();

            Set<Integer> invertedCover = new HashSet<>();
            for (int candidate = 0; candidate < base.vertexSet().size(); candidate++) {
                if ( !vertexCover.contains(candidate) ) {
                    invertedCover.add(candidate);
                }
            }

            Configuration c = new Configuration(this, invertedCover);

            if (c.isInducedSubgraph(g))
                return true;
        }

        return false;
    }

    /**
     * Checks whether the matrices of <tt>this</tt> and <tt>c</tt>
     * are equal, if the nodes of <tt>c</tt> are reordered
     * according to the permutation given by <tt>perm</tt>.
     *
     * @param c the Configuration to be matched
     * @param perm the permutation used for matching
     * @return true, if <tt>perm</tt> reorders the nodes of c
     *   such that the matrices of <tt>this</tt> and <tt>c</tt> are equal,
     *   false otherwise.
     */
    private boolean check(Configuration c, int perm[]){
        // this.cnt==g.cnt is checked before
        int i, j;
        for (i=0; i<base.vertexSet().size(); i++)
            for (j=i+1; j<base.vertexSet().size(); j++)
                if (    base.containsEdge(i,j) != c.base.containsEdge(perm[i], perm[j]) &&
                        optEdges.containsEdge(i,j) != c.optEdges.containsEdge(perm[i], perm[j]) &&
                        nonEdges.containsEdge(i,j) != c.nonEdges.containsEdge(perm[i], perm[j])
                        //matrix[i][j] != c.matrix[perm[i]][perm[j]]
                        ) {
                    return false;
                }
        return true;
    }
}
