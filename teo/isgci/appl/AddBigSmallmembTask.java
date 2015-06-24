package teo.isgci.appl;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import teo.isgci.grapht.GAlg;
import teo.isgci.smallgraph.Graph;

import java.util.ArrayList;
import java.util.concurrent.Semaphore;

/**
 * Created by dennis on 21.06.15.
 */
public class AddBigSmallmembTask implements Runnable {
    protected ArrayList<Graph> topo;
    protected Graph bigG;
    protected Graph bigGComplement;
    protected ArrayList<Graph> vs = new ArrayList<>();
    protected ArrayList<Graph> vsComplement = new ArrayList<>();
    protected SimpleDirectedGraph<Graph,DefaultEdge> resultGraph;
    protected boolean finished = false;
    protected Semaphore sem;

    public AddBigSmallmembTask(ArrayList<Graph> topo, Graph bigG, Graph bigGComplement, SimpleDirectedGraph<Graph, DefaultEdge> resultGraph, Semaphore sem) {
        this.topo = topo;
        this.bigG = bigG;
        this.bigGComplement = bigGComplement;
        this.resultGraph = resultGraph;
        this.sem = sem;
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
            if (bigG.isSubIsomorphic(v)) {
                vs.add(v);
                vsComplement.add((Graph) v.getComplement());
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
        finished = true;
    }

    public ArrayList<Graph> getResult() {
        return vs;
    }

    public boolean isFinished() {
        return finished;
    }


}
