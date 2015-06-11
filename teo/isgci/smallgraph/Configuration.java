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

import org.jgrapht.alg.VertexCovers;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

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
    private int matrix[][];
    private int cnt;  // number of nodes in Configuration
    
    final static int EDGE = 1;
    final static int NONEDGE = -1;
    final static int OPTEDGE = 0;
    final static int UNKNOWN = 2;   /* XXX was soll das? */

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
     * TODO: comment this
     */
    public void copyFromComplement() {
        int i, j;

        super.copyFromComplement();
        //---- First copy the definition
        Configuration c = (Configuration) complement;
        cnt = c.cnt;
        matrix = new int[cnt][cnt];
        for (i=0; i<cnt; i++)
            for (j=0; j<cnt; j++)
                matrix[i][j] = c.matrix[i][j];
        contains = (c.contains != null) ? (Vector) c.contains.clone() : null;

        //--- Then complement it
        for(i=0; i<cnt; i++)
            for(j=0; j<cnt; j++)
                if(i!=j) matrix[i][j] = -matrix[i][j];
        if (contains != null) {
            Vector<SmallGraph> newContains = new Vector<SmallGraph>();
            for (SmallGraph g : contains)
                newContains.add(g.getComplement());
            contains = newContains;
        }
    }

    public Configuration(Configuration c, boolean mask[]){
        super();
        this.initWithMaskedConfiguration(c, mask);
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

        boolean[] mask = new boolean[c.cnt];

        for (int v : includedNodes) {
            mask[v] = true;
        }

        initWithMaskedConfiguration(c, mask);
    }

    private void initWithMaskedConfiguration(Configuration c, boolean mask[]) {
        int i;

        cnt = 0;
        for (i = 0; i < c.cnt; i++)
            if (mask[i])
                cnt++;

        matrix = new int[cnt][cnt];

        int j, k, l;

        k = -1;
        for (i = 0; i < c.cnt; i++) {
            if (mask[i])
                k++;
            else
                continue;

            l = -1;
            for (j = 0; j < c.cnt; j++) {
                if (mask[j])
                    l++;
                else
                    continue;

                matrix[k][l] = c.matrix[i][j];
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
        cnt = n;
        matrix = new int[cnt][cnt];
        for (int i=0; i<cnt; i++)
            for (int j=0; j<cnt; j++)
                matrix[i][j] = i==j ? NONEDGE : UNKNOWN;
        contains = null;
    }

    /**
     * @return the number of nodes in the Configuration
     */
    public int countNodes(){
        return cnt;
    }

    /**
     * Adds an edge to the Configuration.
     * <tt>type</tt> shows which edge should be added.
     */
    private void addEdge(int a, int b, int type){
        if (type != EDGE && type != NONEDGE && type != OPTEDGE) {
            System.out.println("Wrong type of edge!");
            return;
        }
        if (a == b) return;
        if (a<0 || b<0 || a>=cnt || b>=cnt) return;
        if (matrix[a][b]==type || matrix[b][a]==type) {
            String s = new String();
            if (type == EDGE)
                s = "Edge";
            else if (type == NONEDGE)
                s = "Nonedge";
            else if (type == OPTEDGE)
                s = "Optedge";
            System.err.println(getName() +":"+ s +" \""+ a +"-"+ b +
                    "\" already exists!");
        }
        matrix[a][b] = type;
        matrix[b][a] = type;
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
     * @param a first node of the edge
     * @param b second node of the edge
     * @return returns the edge type
     */
    public int getEdge(int a, int b){
        return matrix[a][b];
    }

    /**
     * Counts the edges of type <tt>type</tt>.
     *
     * @param type edge type
     * @return number of edges of type <tt>type</tt>
     */
    public int countEdges(int type){
        int i, j, n=0;
        for (i=0; i<cnt; i++)
            for (j=0; j<i; j++)
                if (matrix[i][j] == type)
                    n++;
        return n;
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
        if (v<0 || v>=cnt) return -1; // illegal argument
        int i, n=0;
        for(i=0; i<cnt; i++){
            if (matrix[v][i] == type) n++;
            // matrix[v][v] is always a NONEDGE
        }
        return n;
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
        if (v<0 || v>=cnt || !mask[v]) return -1; // illegal argument
        int i, n=0;
        for(i=0; i<cnt; i++){
            if (matrix[v][i] == type && mask[i]) n++;
            // matrix[v][v] is always a NONEDGE
        }
        return n;
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
        if(cnt == 0)
            return "";

        int i,j;
        String s="{"+String.valueOf(cnt)+"} ";
        s += namesToString() + " ";
        for(i=0;i<cnt;i++)
            for(j=0;j<i;j++)
                if(matrix[i][j] == EDGE)
                    s+=(j+" - "+i+"; ");
                else if(matrix[i][j] == OPTEDGE)
                    s+=(j+" = "+i+"; ");
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

        /* optEdges contains a representation of all optional edges,
        indexed by the occurence in the matrix from left to right,
        top down. */
        int optEdges[][] = new int[maxOptEdges][2];// x=[][0], y=[][1]
        int i, j, cntOpt = 0, allOptionalEdges;
        Vector<Graph> confGraphs = new Vector<Graph>();

        long ta = 0, te;

        if (DEBUG) {
            System.out.print("Name: " + getName() + "\n");
            System.out.print("  " + this.toString() + "\n");
            System.out.print("  Knoten: " + cnt + "\n");
            ta = System.currentTimeMillis();
        }

        // fill optEdges array
        for (i=0; i<cnt-1; i++)
            for (j=i+1; j<cnt; j++)
                if (matrix[i][j] == OPTEDGE) {
                    if (cntOpt == maxOptEdges){
                        if (DEBUG)
                            System.out.print("  mehr als " + maxOptEdges
                                    + " optionale Kanten\n\n");
                        return null;
                    }
                    optEdges[cntOpt][0] = i;
                    optEdges[cntOpt++][1] = j;
                }

        if (DEBUG) {
            System.out.print("  optionale Kanten: " + cntOpt + "\n");
            for (i=0; i < cntOpt; i++)
                System.out.print("    " + i + ": " + optEdges[i][0]
                                + " = " + optEdges[i][1] + "\n");
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
                 maps an optional edge, namely optEdges[j] == (x, y) to another
                 optional edge, namely optEdges[k] == (p[x], p[y]) */
                int trafo[] = new int[cntOpt];

                if (DEBUG) {
                    System.out.print("    (");
                    for (j = 0; j < cnt; j++)
                        System.out.print(" " + p[j] + " ");
                    System.out.print(")  ---> ");
                }
                loop: for (j=0; j < cntOpt; j++) {
                    int x_a = optEdges[j][0];
                    int y_a = optEdges[j][1];
                    int x_b = p[x_a];
                    int y_b = p[y_a];

                    /* Search the edge (x_b, y_b) in optEdges */
                    for (int k = 0; k < cntOpt; k++) {
                    if (optEdges[k][0] == x_b && optEdges[k][1] == y_b
                                || optEdges[k][1] == x_b
                                && optEdges[k][0] == y_b) {
                            trafo[j]=k;
                            continue loop;
                        }
                    }

                    /* If the permutation of edge j is not in optEdges
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
        Graph shape = new Graph(cnt);
        for (i=0; i<cnt; i++)
            for (j=i+1; j<cnt; j++)
                if (matrix[i][j] == EDGE)
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
                    rep.addEdge(optEdges[j][0], optEdges[j][1]);

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
        boolean darf[][] = new boolean[cnt][cnt];
        boolean mask[][] = new boolean[2][cnt];
        Vector ret = new Vector();

        for (i = 0; i < cnt; i++) {
            mask[0][i] = true;
            mask[1][i] = true;
            for (j = 0; j < cnt; j++)
                    darf[i][j] = true;
        }

        isIsomorphicIntern(this, darf, mask); /* rückgabe egal, da immer */
        /* zu sich selbst isomorph */

        Permutation perm = new Permutation(cnt,darf);
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
        if (cnt!=c.cnt || countEdges()!=c.countEdges() ||
                          countNonedges()!=c.countNonedges())
            return false;

        int i, j;
        boolean darf[][] = new boolean[cnt][cnt];
        boolean mask[][] = new boolean[2][cnt];

        for (i = 0; i < cnt; i++) {
            mask[0][i] = true;
            mask[1][i] = true;
            for (j = 0; j < cnt; j++)
                    darf[i][j] = true;
        }

        if (!isIsomorphicIntern(c, darf, mask))
                return false;

        Permutation perm = new Permutation(cnt,darf);
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
        int Grad[][] = new int[2][cnt];
        int OptGrad[][] = new int[2][cnt];
        boolean changed = false;

        /* Speichere in Grad[0][i] den Grad des Knoten i in der Configuration
         * this Analog für die Configuration c wenn der Index 1 gewählt ist.
         * Das ganze aber nur dann, wenn in der jeweiligen Maske das Flag für
         * den entsprechenden Knoten gesetzt ist. Nebenbei wird ermittelt, ob
         * die Gradzahlen in beiden Graphen übereinsstimmen.*/
        for (i=0; i < cnt; i++) {
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
        for (i=0; i < cnt; i++) {
            if (!mask[0][i])
                continue;

            for (j=0; j < cnt; j++) {
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
        for (i = 0; i < cnt; i++) {
            if (mask[0][i]) { /* nur wenn in diesen Durchlauf eventuell */
                    /*angefaßt */
                boolean ziel = false;;
                for (j = 0; j < cnt; j++) {
                    if (darf[i][j])
                        ziel = true;;
                }
                if (! ziel) 
                    return false;
            }
        }

        /* untersuchen, ob es Knoten gibt, für die es keine Quelle gibt */
        for (i = 0; i < cnt; i++) {
            if (mask[1][i]) { /* nur wenn in diesen Durchlauf eventuell */
                    /* angefaßt */
                boolean quelle = false;;
                for (j = 0; j < cnt; j++) {
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
        if (g.countNodes() > cnt || g.countEdges() > countEdges())
            return false;

        /* /g/ hat einen Knoten */
        if (g.countNodes() == 1)
            return cnt >= 1;    /* der K1 ist in jeder nichtleeren */
            /* Konfiguration enthalten */
        int i, j;

        /* /g/ hat zwei Knoten */
        if (g.countNodes() == 2) {
            if (g.degree(0) == 1) {
                /* /g/ ist ein K2 */
                for (i = 0; i < cnt - 1; i++)
                    for (j = i + 1; j < cnt; j++)
                        if (matrix[i][j] == EDGE)
                            return true;
            } else {
                /* /g/ ist ein 2K1 */
                for (i = 0; i < cnt - 1; i++)
                    for (j = i + 1; j < cnt; j++)
                        if (matrix[i][j] == NONEDGE || matrix[i][j] == UNKNOWN)
                            return true;
            }

            return false;   /* keine aufwändigeren Tests für diesen Fall */
        }


        /* Der Rumpf einer Konfiguration ist der Graph, der entsteht wenn man
         * alle optionalen Kanten wegläßt */
        Graph rumpf = new Graph(cnt);
        for (i = 0; i < cnt - 1; i++)
            for (j = i + 1; j < cnt; j++)
                if (matrix[i][j] == EDGE)
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

                for (i = 0; i < cnt - 1; i++)
                    for (j = i + 1; j < cnt; j++)
                        if (matrix[i][j] == OPTEDGE) {
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
            Graph h = new Graph(cnt);

            for (i = 0; i < cnt - 1; i++)
                for (j = i + 1; j < cnt; j++)
                    if (matrix[i][j] == OPTEDGE)
                        h.addEdge(i,j);

            Set<Integer> vertexCover = h.getVertexCover();

            Set<Integer> invertedCover = new HashSet<>();
            for (int candidate = 0; candidate < cnt; candidate++) {
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
        for (i=0; i<cnt; i++)
            for (j=i+1; j<cnt; j++)
                if (matrix[i][j] != c.matrix[perm[i]][perm[j]])
                    return false;
        return true;
    }
}
