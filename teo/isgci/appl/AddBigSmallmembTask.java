package teo.isgci.appl;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import teo.isgci.smallgraph.Graph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Vector;
import java.util.concurrent.Semaphore;

/**
 * Created by dennis on 21.06.15.
 */
public class AddBigSmallmembTask implements Runnable {
    protected ArrayList<Graph> topo;
    protected Graph bigG;
    protected Graph bigGComplement;
    protected HashSet<Graph> transitiveInduced;
    protected Hashtable<Graph, Vector<Graph>> inducedTable;
    protected SimpleDirectedGraph<Graph,DefaultEdge> resultGraph;
    protected Semaphore sem;

    public AddBigSmallmembTask(ArrayList<Graph> topo, Graph bigG,
                               Graph bigGComplement,
                               SimpleDirectedGraph<Graph, DefaultEdge> resultGraph,
                               Hashtable<Graph, Vector<Graph>> inducedTable,
                               Semaphore sem) {
        this.topo = topo;
        this.bigG = bigG;
        this.bigGComplement = bigGComplement;
        this.resultGraph = resultGraph;
        this.inducedTable = inducedTable;
        this.sem = sem;

        this.transitiveInduced = new HashSet<>();
    }

    public void run()
    {
        try {
            sem.acquire();
            System.out.println("wire up " + bigG.getName() + " and its complement in resultgraph");
            resultGraph.addVertex(bigG);
            resultGraph.addVertex(bigGComplement);
        }
        catch (InterruptedException e) {
            return;
        }
        finally {
            sem.release();
        }

        for (Graph v : topo) {
            /* don´t need to build transitive hull, information is already there */
            if (transitiveInduced.contains(v)) {
                continue;
            }

            if (bigG.isSubIsomorphic(v)) {
                transitiveInduced.add(v);
                transitiveInduced.addAll(inducedTable.get(v));
                try {
                    sem.acquire();
                    resultGraph.addEdge(bigG, v);
                    resultGraph.addEdge(bigGComplement, (Graph) v.getComplement());
                }
                catch (InterruptedException e) {
                    return;
                }
                finally {
                    sem.release();
                }
            }
        }
    }

}
