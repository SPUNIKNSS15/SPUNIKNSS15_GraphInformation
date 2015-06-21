package teo.isgci.appl;

import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import teo.isgci.grapht.GAlg;
import teo.isgci.smallgraph.Graph;

import java.util.ArrayList;

/**
 * Created by dennis on 21.06.15.
 */
public class ThreadClass extends Thread {
    protected ArrayList<Graph> topo;
    protected Graph bigG;
    protected ArrayList<Graph> vs = new ArrayList<>();
    protected SimpleDirectedGraph<Graph,DefaultEdge> resultGraph;
    protected boolean finished = false;

        public ThreadClass(ArrayList<Graph> topo, Graph bigG, SimpleDirectedGraph<Graph,DefaultEdge> resultGraph) {
            this.topo = topo;
            this.bigG = bigG;
            this.resultGraph = resultGraph;
        }


        public void run()
        {
            for (Graph v : topo) {
                if (GAlg.getPath(resultGraph, bigG, v) == null)
                    if (bigG.isSubIsomorphic(v)) {
                        vs.add(v);
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
