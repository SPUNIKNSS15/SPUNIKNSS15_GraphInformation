/*
 * Head-Mid-Tail families
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.smallgraph;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

public class HMTGrammar{
    /**
     * @head constituent graph and his attachment/extension
     * @mid constituent graph and his attachment/extension
     * @tail constituent graph and his attachment/extension
     * @type type of the grammar
     * @name name of the grammar
     */
    HMTGraph head, mid, tail;
    int type;
    String name;

    /**
     * creates a new HMT grammar of the given type without a name
     *
     * @param type type of the new HMT grammar
     */
    public HMTGrammar(int type){
        head = mid = tail = null;
        this.type = type;
        name = null;
    }

    /**
     * Create a new HMT grammar of the given type with a given name.
     *
     * @param type type of the new HMT grammar
     * @param name name of the HMT grammar
     */
    public HMTGrammar(int type, String name){
        this(type);
        this.name = name;
    }

    /**
     *
     * @return type of this grammar
     */
    public int getType(){
        return type;
    }

    /**
     *
     * @return name of this grammar if this grammar exists outside family.
     */
    public String getName(){
        return name;
    }

    /**
     * Set head (with attachment).
     *
     * @param head
     * @param atth attachment
     */
    public void setHead(Graph head, int[] atth){
        if (atth.length != type)
            throw new IllegalArgumentException("Type of head wrong");
        this.head = new HMTGraph(head, null, atth);
    }

    /**
     *
     * @return head (with attachment)
     */
    public HMTGraph getHead(){
        return head;
    }

    /**
     * Set mid (with attachment/extension)
     *
     * @param mid
     * @param extm extension
     * @param attm attachment
     */
    public void setMid(Graph mid, int[] extm, int[] attm){
        if (extm.length != type  ||  attm.length != type) {
            throw new IllegalArgumentException("Type of mid wrong");
        }

        HMTGraph midHMT = new HMTGraph(mid, extm, attm);

        if (!isNormalized(midHMT)) {
            throw new IllegalArgumentException("Mid graph not normalised");
        }

        this.mid = midHMT;
    }


    private boolean isNormalized(HMTGraph mid) {

        /*
         * For a HMT grammar G=(H,M,T), define Q(M) := ext(M) \cap att(M)
         * A transitive cycle of M is a minimal set of vertices C \subseteq Q(M) such that
         * ext_M(C) = att_M(C). The transitive set of M is the maximal set of vertices
         * M_tr \subseteq Q(M) such that ext(M_tr) = att(M_tr), therefore M_tr is the
         * union of all transitive cycles.
         *
         * Normalised MTR Grammars  must fulfill
         *  1. Q(M) = M_tr
         *  2. \forall v \in M_tr: ext(v) = att(v)
         *  3. M_tr is edgeless.
         */


        /* Check if Q(M) = M_{tr} */

        Set<Integer> qM = new HashSet<>();

        /* Search for att[] \cap ext[] */
        buildQM: for (int i = 0; i < mid.att.length; i++) {
            for (int j = 0; j < mid.ext.length; j++ ) {
                if (mid.att[i] == mid.ext[j]) {
                    qM.add(mid.att[i]);
                    continue buildQM;
                }
            }
        }

        Set<Integer> mTr = new HashSet<>();

        /* Check if all vertices of qM are in mTr */
        System.out.println(this.name);
        System.out.println(mid.getGraph().vertexSet().toString());
        System.out.println("qM: " + qM.toString());

        loop: for (Integer v : qM) {
            if (mTr.contains(v)) {
                continue loop;
            }

            Integer currentV = v;

            do {
                /* 'cycle' leaves qM */
                if (!qM.contains(mid.att[currentV])) {
                    System.out.println("Error in " + this.name + "qM != M_tr");
                    return false;
                }
                mTr.add(currentV);
                currentV = mid.ext[currentV];
            } while (!currentV.equals(v));
        }

        /* check if forall v \in M_{tr}: ext(v) == att(v) */
        for (Integer i : mTr) {
            if (mid.att[i] != mid.ext[i]) {
                System.out.println("Error in " + this.name + " +\\Exists v \\in M_tr: ext(v) != att(v)");
                return false;
            }
        }

        /* Check if M[M_{tr}] is edgeless */
        for (Integer i : mTr) {
            for (Integer j : mTr) {
                if (i != j && mid.getGraph().containsEdge(i, j)) {
                    System.out.println("Error in " + this.name + "M_tr contains edges!");
                    return false;
                }

            }
        }

        return true;
    }

    /**
     *
     * @return mid (with attachment/extension)
     */
    public HMTGraph getMid(){
        return mid;
    }

    /**
     * set tail(with extension)
     * @param tail
     * @param extt extension
     */
    public void setTail(Graph tail, int[] extt){
        if (extt.length != type)
            throw new IllegalArgumentException("Type of tail wrong");
        this.tail = new HMTGraph(tail, extt, null);
    }

    /**
     *
     * @return tail (with extension)
     */
    public HMTGraph getTail(){
        return tail;
    }

    /**
     *
     * @param n
     * @return the graph Head Mid^n Tail
     */
    public Graph getElement(int n){
        HMTGraph left = head;
        for (int i = 0; i < n; i++)
            left = compose(left, mid);
        return new Graph(compose(left, tail));
    }

    /**
     *
     * @param n
     * @return the graphs Head Mid^i Tail, for 0 <= i <= n
     */
    public Vector getSmallElements(int n){
        Vector res = new Vector();
        HMTGraph left = head;
        res.addElement(new Graph(compose(left, tail)));
        for (int i = 1; i <= n; i++) {
            left = compose(left, mid);
            res.addElement(new Graph(compose(left, tail)));
        }
        return res;
    }

    /**
     *
     * @param x graph to compose with y
     * @param y graph to compose with x
     * @return the composition xy
     */
    private HMTGraph compose(HMTGraph x, HMTGraph y){
        if (x.att.length != y.ext.length)
            throw new IllegalArgumentException("Types for compose mismatch");
        
        int i, p, q;
        int [] attz = null;

        Graph z = new Graph(x);                 // Z = XY
        p = z.countNodes();
        int[] f = new int[y.countNodes()];      // f: V(y) -> V(z)
        for (i = 0; i < f.length; i++) {
            if ((q = index(y.ext, i)) >= 0)
                f[i] = x.att[q];
            else {
                //TODO: node could not be added accidentially (see implementation in Graph.java)
                //TODO: if we previously deleted not-last node
                z.addNode();
                f[i] = p++;
            }
        }

        /* Add edges from y */
        for (p = 0; p < y.countNodes()-1; p++)
            for (q = p+1; q < y.countNodes(); q++)
                if (y.getEdge(p, q))
                    z.addEdge(f[p], f[q]);

        /* Make attachment */
        if (y.att != null) {
            attz = new int[y.att.length];
            for (i = 0; i < attz.length; i++)
                attz[i] = f[y.att[i]];
        }

        return new HMTGraph(z, x.ext, attz);
    }

    /**
     *
     * @param a array
     * @param x index
     * @return the index of x in a, or -1 if not found
     */
    private int index(int[] a, int x){
        for (int i = 0; i < a.length; i++)
            if (a[i] == x)
                return i;
        return -1;
    }
    
    public String toString(){
        String s = "HMTGrammar:";
        if (name != null)
            s+="\n"+getName();
        s+="\nHead:\n"+getHead().toString()+"\nMid:\n"+
                getMid().toString()+"\nTail:\n"+getTail().toString()+"\n";
        return s;
    }

    /*====================================================================*/

    /**
     * A graph with attachment and/or extension.
     */
    public class HMTGraph extends Graph{
        int[] ext, att;

        /**
         * Create a new graph with the given attachment/extension.
         * Att/ext are arrays containing the nodes in att/ext, in the proper
         * order. That is, if att(0) = 0, att(5) = 1, att(2) = 2, then
         * att=[0,5,2].
         * If the graph has no att (ext), use null.
         *
         * @param graph graph which should be extended
         * @param ext nodes with which the Graph should be extended
         * @param att nodes to be attached
         */
        public HMTGraph(Graph graph, int[] ext, int[] att){
            super(graph);

            if (att != null  &&  att.length != type  ||
                    ext != null  &&  ext.length != type) {
                throw new IllegalArgumentException("Type of graph wrong");
            }
            this.att = null;
            this.ext = null;

            boolean[] isAttachment = new boolean[graph.countNodes()];
            boolean[] isExtension  = new boolean[graph.countNodes()];

            if (att != null) {
                for (int i = 0; i < att.length; i++) {
                    if (isAttachment[att[i]] || att[i] < 0 || att[i] >= graph.countNodes()) {
                        throw new IllegalArgumentException("Non-bijective att mapping");
                    } else {
                        isAttachment[att[i]] = true;
                    }
                }

                this.att = new int[att.length];
                System.arraycopy(att, 0, this.att, 0, type);
            }

            if (ext != null) {
                for (int i = 0; i < ext.length; i++) {
                    if(isExtension[ext[i]] || ext[i] < 0 || ext[i] >= graph.countNodes()) {
                        throw new IllegalArgumentException("Non-bijective ext mapping");
                    } else {
                        isExtension[ext[i]] = true;
                    }
                }

                this.ext = new int[ext.length];
                System.arraycopy(ext, 0, this.ext, 0, type);
            }
        }

        /**
         *
         * @return attachment array
         */
        public int[] getAtt(){
            return att;
        }

        /**
         *
         * @return extension array
         */
        public int[] getExt(){
            return ext;
        }

        /**
         *
         * @return string with the given attachment and extension
         */
        public String toString(){
            String s = super.toString()+"\n";
            if (ext != null)
                s+="Extension: "+ext+"\n";
            if (att != null)
                s+="Attachment: "+att+"\n";
            return s;
        }
    }
}

/* EOF */
