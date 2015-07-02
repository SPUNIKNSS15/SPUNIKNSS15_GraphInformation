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

        /* sets to save transitive subgraph relations temporarily */
        this.transitiveInduced = new HashSet<>();
        this.transitiveInducedComplement = new HashSet<>();
    }

    /** searches in all known graphs for subgraph relations to graph
     * and its complement at the same time
     * saving the results into inducedTable and resultGraph
     */
    public void run() {
        System.out.println("wire up " + graph.getName() + " and its complement in resultgraph");

        for (Graph v : knownGraphs) {

            if (useInducedInfo && transitiveInduced.contains(v)) {
                /* don´t need to build transitive hull in resultGraph,
                 * information is already there */
                continue;
            }

            if (graph != v && graph.isSubIsomorphic(v)) {
                /* v is transitively induced by graph */
                transitiveInduced.add(v);
                transitiveInducedComplement.add((Graph) v.getComplement());

                /* and all its subgraphs as well */
                if (useInducedInfo) {
                    /* collect all transitive subgraphs for subgraph table
                     * and to avoid additional VF2 Calls */
                    Vector<Graph> allSubs = null;
                    Vector<Graph> allSubsComplement = null;
                    try {
                        /* prevent dirty read by locking induced table */
                        inducedTableSem.acquire();
                        allSubs = inducedTable.get(v);
                        allSubsComplement = inducedTable.get(v.getComplement());
                    } catch (InterruptedException e) {
                    } finally {
                        inducedTableSem.release();
                    }
                    /* save gathered information for later */
                    if (allSubs != null) {
                        transitiveInduced.addAll(allSubs);
                    }
                    if (allSubsComplement != null) {
                        transitiveInducedComplement.addAll(allSubsComplement);
                    }
                }

                /* add relation edge to resultgraph */
                try {
                    /* prevent concurrent write to resultGraph by locking the semaphore */
                    resultGraphSem.acquire();
                    /* write information for graph and its complement to resultgraph */
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
            /* prevent concurrent write by locking inducedTable */
            inducedTableSem.acquire();
            /* write information about transitively induced subgraphs into inducedTable */
            inducedTable.put(graph, new Vector<>(transitiveInduced));
            inducedTable.put(graphComplement, new Vector<>(transitiveInducedComplement));
        } catch (InterruptedException e) {
            return;
        } finally {
            inducedTableSem.release();
        }
    }
}
