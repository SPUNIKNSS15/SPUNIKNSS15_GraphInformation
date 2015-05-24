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

import org.jgrapht.experimental.subgraphisomorphism.VF2SubgraphIsomorphismInspector;
import org.jgrapht.graph.DefaultEdge;

import java.util.Vector;

public class Graph extends SmallGraph{
    private int size;   // size of matrix and KomponentenVektor
    private int Komponenten;    // Anzahl von Zusammenhangskomponenten
    private int KomponentenVektor[];
    private boolean Komponenten_isKanonisch;

    private boolean is_bottom;

    /** Creates a new graph without nodes. */
    public Graph(){
        this(0);
    }

    /** Creates a new graph with <tt>n</tt> nodes. */
    public Graph(int n){
        super();
        addNodesCount(n);
    }

    public Graph(Graph g){
        this(0);
        copyFrom(g);
    }

    /**
     * Set the nodecount of this to n. Any previous nodes/edges are lost!
     */
    public void addNodesCount(int n) {
        size=n;
        cnt=n;
        Ecnt = 0;
        Komponenten = n;    /* noch gibt es keine Kanten */
        Komponenten_isKanonisch = false;
        is_bottom = false;
        matrix=new boolean[size][size];
        KomponentenVektor = new int[size];
        int i,j;
        for(i=0;i<size;i++) {
            KomponentenVektor[i] = i;
            for(j=0;j<size;j++)
                matrix[i][j]=false;
        }
    }
    
    /** Copy the contents of gs into this. */
    private void copyFrom(SmallGraph gs){
        Graph g = (Graph)gs;

        if (g.is_bottom) {
            is_bottom = true;
            return;
        }

        size=g.size;
        cnt=g.cnt;
        Ecnt=g.Ecnt;
        Komponenten = g.Komponenten;
        Komponenten_isKanonisch = g.Komponenten_isKanonisch;
        matrix=new boolean[size][size];
        KomponentenVektor = new int[size];
        int i,j;
        for(i=0;i<size;i++) {
            KomponentenVektor[i] = g.KomponentenVektor[i];
            for(j=0;j<size;j++)
                matrix[i][j]=g.matrix[i][j];
        }
    }

    public Graph(Graph g, boolean mask[]){
        super();

        int i;

        cnt = 0;
        for (i = 0; i < g.cnt; i++)
            if (mask[i])
                cnt++;

        size = cnt;

        matrix = new boolean[size][size];
        KomponentenVektor = new int[size];

        int j, k, l;

        k = -1;
        for (i = 0; i < g.cnt; i++) {
            if (mask[i])
                k++;
            else
                continue;

            l = -1;
            for (j = 0; j < g.cnt; j++) {
                if (mask[j])
                    l++;
                else
                    continue;

                matrix[k][l] = g.matrix[i][j];
            }
        }

        Ecnt = 0;
        for (i = 0; i < cnt - 1; i++)
            for (j = i + 1; j < cnt; j++)
                if (matrix[i][j])
                    Ecnt++;

        updateKomponentenVektor();
    }

    public void copyFromComplement() {
        super.copyFromComplement();
        copyFrom(complement);

        //---- Then complement it.
        for(int i=0;i<cnt;i++)
            for(int j=0;j<cnt;j++)
                if(i!=j) matrix[i][j] = !matrix[i][j];
        Ecnt=(((cnt-1)*cnt)/2) - Ecnt;
        updateKomponentenVektor();
    }

    public boolean getBottom(){
        return is_bottom;
    }

    public void setBottom(){
        is_bottom = true;
        cnt = 0;
        Ecnt = 0;
    }
    
    
    /** Counts the nodes in this graph. */
    public int countNodes(){
        return cnt;
    }
    
    /** Counts the edges in this graph. */
    public int countEdges(){
        return Ecnt;
    }
    
    /**
     * REIMPLEMENTED WITH:
     *  -org.jgrapht.graph.SimpleGraph.degreeOf(V vertex)
     *
     * @deprecated
     * parameter changes from int to V
     *
     * Returns the degree of the node at index <tt>v</tt> */
    public int degree(int v){
        if(v<0 || v>=cnt) return -1; // illegal argument
        int i,n=0;
        for(i=0;i<cnt;i++){
            if(matrix[v][i]) n++;
            // matrix[v][v] is always false
        }
        return n;
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
        if(v<0 || v>=cnt || ! mask[v]) return -1; // illegal argument
        int i,n=0;
        for(i=0;i<cnt;i++){
            if(matrix[v][i] && mask[i]) n++;
            // matrix[v][v] is always false
        }
        return n;
    }
    
    /** Returns an array with all adjacent nodes of <tt>v</tt>. */
    public int[] adjList(int v){
        int i,n=degree(v);
        if(n<0) return null;
        int list[]=new int[n];
        n=0;
        for(i=0;i<cnt;i++){
            if(matrix[v][i]) list[n++]=i;
        }
        return list;
    }
    
    /** Adds a node to the graph. */
    public void addNode(){
        int i,j;
        if(cnt==size) increment();
        cnt++;
        j = -1; /* größte Zahl im KomponentenVektor in j speichern */
        for(i=0;i<cnt;i++){
            matrix[i][cnt-1]=false;
            matrix[cnt-1][i]=false;
            if (KomponentenVektor[i] > j)
                j = KomponentenVektor[i];
        }
        KomponentenVektor[cnt-1] = j + 1;
        Komponenten++;  /* der neue Knoten bildet eine neue Komponente */
        /* Komponenten_isKanonisch ändert sich nicht */
    }
    
    /** Increases the size of the matrix by 10. */
    private void increment(){
        size+=10;
        boolean newMatrix[][]=new boolean[size][size];
        int newKomponentenVektor[]=new int[size];
        int i,j;
        for(i=0;i<size;i++){
            if (i<cnt)
                newKomponentenVektor[i] = KomponentenVektor[i];
            else
                newKomponentenVektor[i] = -1;

            for(j=0;j<size;j++){
                if(i<cnt && j<cnt) newMatrix[i][j]=matrix[i][j];
                else newMatrix[i][j]=false;
            }
        }
        matrix=newMatrix;
        KomponentenVektor=newKomponentenVektor;
    }
    
    /** Adds an edge to the graph. */
    public void addEdge(int a,int b){
        if(a==b) return;
        if(a<0 || b<0 || a>=cnt || b>=cnt) return;
        if (matrix[a][b] || matrix[b][a])
            System.err.println("Edge \""+a+" - "+b+
            "\" already exists in graph "+this.getName()+"!");
        matrix[a][b]=true;
        matrix[b][a]=true;
        Ecnt++;

        /* Test, ob die neue Kante zwei Zusammenhangskomponenten verbindet */
        if (KomponentenVektor[a] != KomponentenVektor[b]) {
            int i, x, y;

            x = java.lang.Math.min(KomponentenVektor[a], KomponentenVektor[b]);
            y = java.lang.Math.max(KomponentenVektor[a], KomponentenVektor[b]);

            for (i = 0; i < cnt; i++)
                if (KomponentenVektor[i] == y)
                    KomponentenVektor[i] = x;

            Komponenten--;
            Komponenten_isKanonisch = false;
        }
    }
    
    /** Removes a node and all its adjacent edges from the graph. */
    public void delNode(int v){
        int i,j;

        /* Die Anzahl der Kanten im Graphen aktualisieren */
        for (i=0;i<cnt;i++)
            if (matrix[i][v])
                Ecnt--;

        /* Die Adjazenzmatrix aktualisieren */
        for(i=v+1;i<cnt;i++)
            for(j=0;j<cnt;j++)
                matrix[i-1][j]=matrix[i][j];
        for(i=0;i<cnt;i++)
            for(j=v+1;j<cnt;j++)
                matrix[i][j-1]=matrix[i][j];

        /* KomponentenVektor verschieben */
        for (i = v + 1; i < cnt; i++)
            KomponentenVektor[i-1] = KomponentenVektor[i];

        /* Anzahl der Knoten aktualisiseren */
        cnt--;

        /* KomponentenVektor aktualisieren */
        updateKomponentenVektor();
    }
    
    /** Removes the edge between the two given nodes. */
    public void delEdge(int a,int b){
        matrix[a][b]=false;
        matrix[b][a]=false;
        Ecnt--;
        updateKomponentenVektor();
    }

    private void updateKomponentenVektor(){
        int i, j, k;

        Komponenten = cnt;

        for (i = 0; i < cnt; i++)
           KomponentenVektor[i] = i;

        for (i = 0; i < cnt; i++) {
            for (j = 0; j < cnt; j++) {
                if (matrix[i][j] && KomponentenVektor[i] !=
                        KomponentenVektor[j]) {
                    int x, y;

                    x = java.lang.Math.min(KomponentenVektor[i],
                            KomponentenVektor[j]);
                    y = java.lang.Math.max(KomponentenVektor[i],
                            KomponentenVektor[j]);

                    for (k = 0; k < cnt; k++)
                        if (KomponentenVektor[k] == y)
                            KomponentenVektor[k] = x;

                    Komponenten--;
                }
            }
        }
        Komponenten_isKanonisch = false;
    }

    /* kanonisiert den KomponentenVektor */
    private void kanonKomponentenVektor(){
        int i, j;
        int frei;
        int zugross;

        while (true) {
            /* suche Komponente mit zu großer Nummer */
            zugross = -1;
            for (i = 0; i < cnt; i++) {
                if (KomponentenVektor[i] >= Komponenten) {
                    zugross = KomponentenVektor[i];
                    break;
                }
            }

            if (zugross == -1) {
                Komponenten_isKanonisch = true;
                return; /* fertig */
            }

            /* suche Lücke in Numerierung */
            frei = 0;
loop:       while (true) {
                for (i = 0; i < cnt; i++) {
                    if (KomponentenVektor[i] == frei) {
                        frei++;
                        continue loop;
                    }
                }
                break loop;
            }

            if (frei >= Komponenten) {
                System.err.println("es gibt keine kanonische Numerierung!!!");
                System.exit(1);
            }

            /* jetzt alle Vorkommen von /zugross/ durch /frei/ ersetzen. */
            for (i = 0; i < cnt; i++)
                if (KomponentenVektor[i] == zugross)
                    KomponentenVektor[i] = frei;
        }
    }

    
    /** Returns <tt>true</tt> if there is an edge between the given nodes. */
    public boolean getEdge(int a,int b){
        return matrix[a][b];
    }
    
    /**
     * Returns a string that represents the graph.
     * It contains the number of nodes, the name(s) of the graph
     * and the list of edges.
     */
    public String toString(){
        if(cnt==0) return "";
        int i,j;
        String s="{"+String.valueOf(cnt)+"}\n";
        s += namesToString() + "\n";
        for(i=0;i<cnt;i++)
            for(j=0;j<i;j++)
                if(matrix[i][j])
                    s+=(j+" - "+i+"\n");
        return s;
    }
    
    // --------------------------------------------------------------
    public boolean isIsomorphic(Graph g){
        final boolean DEBUG = false;

        /* check if one of both is the bottom-graph*/
        if (g.is_bottom || is_bottom) {
            return false;
        }

        /* check if number of nodes and edges are equal */
        if( !( this.vertexSet().size() == g.vertexSet().size() && this.edgeSet().size() == g.edgeSet().size() ) ) {
            return false;
        }

        //TODO: do we need this? Maybe Komponenten becomes unnecessary... (tassilo :D)?
        if (Komponenten != g.Komponenten) {
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
        return Komponenten;
    }

    /* liefert eine Maske zurück, die angibt welche Knoten in der i-ten
     * Komponente enthalten sind */
    public boolean[] getComponents(int num){
        boolean ret[] = new boolean[cnt];

        if (num < 0 && num >= Komponenten)
            return null;

        if (! Komponenten_isKanonisch)
            kanonKomponentenVektor();

        for (int i = 0; i < cnt; i++)
            ret[i] = KomponentenVektor[i] == num;

        return ret;
    }

    /* liefert Maske, die VertexCover bildet */
    public boolean[] getVertexCover(){
        /* eine einfache Greedy-Strategie */

        int grad[] = new int[cnt];
        boolean ret[] = new boolean[cnt];

        for (int i = 0; i < cnt; i++) {
            grad[i] = degree(i);
            ret[i] = false;
        }

        int max, pos;

        while (true) {

            max = 0;
            pos = -1;

            for (int i = 0; i < cnt; i++)
                if (grad[i] > max) {
                    max = grad[i];
                    pos = i;
                }

            if (pos == -1)
                break;

            ret[pos] = true;
            grad[pos] = 0;

            for (int i = 0; i < cnt; i++)
                if (matrix[i][pos])
                    grad[i]--;

        }

        return ret;
    }


    public static void main(String args[]){
/*
        Graph g1 = new Graph(5);
        Graph g2 = new Graph(5);

        g1.addEdge(0,1);
        g1.addEdge(1,2);
        g1.addEdge(2,3);
        g1.addEdge(3,4);
        g1.addEdge(0,3);

        g2.addEdge(0,1);
        g2.addEdge(1,2);
        g2.addEdge(2,3);
        g2.addEdge(0,4);
        g2.addEdge(2,4);
*/
        Graph g1 = new Graph(9);
        Graph g2 = new Graph(9);

        g1.addEdge(0,2);
        g1.addEdge(0,3);
        g1.addEdge(1,3);
        g1.addEdge(0,4);
        g1.addEdge(1,4);
        g1.addEdge(2,4);
        g1.addEdge(1,5);
        g1.addEdge(3,5);
        g1.addEdge(0,6);
        g1.addEdge(3,6);
        g1.addEdge(0,7);
        g1.addEdge(5,7);
        g1.addEdge(4,8);
        g1.addEdge(5,8);
        g1.addEdge(6,8);
        g1.addEdge(7,8);

        g2.addEdge(0,2);
        g2.addEdge(0,3);
        g2.addEdge(1,3);
        g2.addEdge(0,4);
        g2.addEdge(1,4);
        g2.addEdge(2,4);
        g2.addEdge(1,5);
        g2.addEdge(3,5);
        g2.addEdge(0,6);
        g2.addEdge(5,6);
        g2.addEdge(4,7);
        g2.addEdge(5,7);
        g2.addEdge(6,7);
        g2.addEdge(0,8);
        g2.addEdge(1,8);
        g2.addEdge(6,8);

        System.out.print("G1: " + g1 + "\n");
        System.out.print("G2: " + g2 + "\n");
        if (g1.isIsomorphic(g2))
            System.out.print("g1 und g2 sind isomorph\n");
        else
            System.out.print("g1 und g2 sind NICHT isomorph\n");
    }
}

/* EOF */
