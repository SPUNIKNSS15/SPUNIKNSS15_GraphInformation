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
public class AddSubgraphRelationsTask implements Runnable {
    private final boolean useInducedInfo;
    protected Vector<Graph> knownGraphs;
    protected Graph graph;
    protected Graph graphComplement;
    protected HashSet<Graph> transitiveInduced;
    protected HashSet<Graph> transitiveInducedComplement;
    protected Hashtable<Graph, Vector<Graph>> inducedTable;
    protected SimpleDirectedGraph<Graph,DefaultEdge> resultGraph;
    protected Semaphore resultGraphSem;
    protected Semaphore inducedTableSem;

    public AddSubgraphRelationsTask(Vector<Graph> knownGraphs, Graph graph,
                                    Graph graphComplement,
                                    SimpleDirectedGraph<Graph, DefaultEdge> resultGraph,
                                    Hashtable<Graph, Vector<Graph>> inducedTable,
                                    Semaphore resultGraphSem,
                                    Semaphore inducedTableSem,
                                    boolean useInducedInfo) {
        this.knownGraphs = knownGraphs;
        this.graph = graph;
        this.graphComplement = graphComplement;
        this.resultGraph = resultGraph;
        this.inducedTable = inducedTable;
        this.resultGraphSem = resultGraphSem;
        this.inducedTableSem = inducedTableSem;
        this.useInducedInfo = useInducedInfo;

        this.transitiveInduced = new HashSet<>();
        this.transitiveInducedComplement = new HashSet<>();
    }

    public void run() {
        System.out.println("wire up " + graph.getName() + " and its complement in resultgraph");

        for (Graph v : knownGraphs) {

            /* don´t need to build transitive hull in resultGraph, information is already there */
            if (useInducedInfo && transitiveInduced.contains(v)) {
                continue;
            }

            if (graph != v && graph.isSubIsomorphic(v)) {
                transitiveInduced.add(v);
                transitiveInducedComplement.add((Graph) v.getComplement());

                if (useInducedInfo) {
                    /* collect all transitive subgraphs for subgraph table
                     * and to avoid additional VF2 Calls */
                    Vector<Graph> allSubs = null;
                    Vector<Graph> allSubsComplement = null;
                    try {
                        inducedTableSem.acquire();
                        allSubs = inducedTable.get(v);
                        allSubsComplement = inducedTable.get(v.getComplement());
                    } catch (InterruptedException e) {
                    } finally {
                        inducedTableSem.release();
                    }
                    if (allSubs != null) {
                        transitiveInduced.addAll(allSubs);
                    }
                    if (allSubsComplement != null) {
                        transitiveInducedComplement.addAll(allSubsComplement);
                    }
                }

                /* add relation edge to resultgraph */
                try {
                    resultGraphSem.acquire();
                    resultGraph.addEdge(graph, v);
                    resultGraph.addEdge(graphComplement, (Graph) v.getComplement());
                } catch (InterruptedException e) {
                    return;
                } finally {
                    resultGraphSem.release();
                }
            }
        }

        /* add to subgraphs table */
        try {
            inducedTableSem.acquire();
            inducedTable.put(graph, new Vector<>(transitiveInduced));
            inducedTable.put(graphComplement, new Vector<>(transitiveInducedComplement));
        } catch (InterruptedException e) {
            return;
        } finally {
            inducedTableSem.release();
        }
    }
}
