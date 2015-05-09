package teo.isgci.smallgraph.test;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import teo.isgci.smallgraph.Graph;

import static org.junit.Assert.*;

/**
 * Contains tests for those methods in teo.isgci.smallgraph.Graph
 * which use the adjacency matrix internally.
 *
 * This test set will allow us to continuously replace all of those
 * methods with their jgrapht counterparts, without API breaks.
 *
 * Created by Moritz Renftle on 09.05.15.
 */
public class GraphTest {

    Graph graph;

    @Before
    public void setUp() throws Exception {
        graph = new Graph();
    }

    @After
    public void tearDown() throws Exception {

    }

    /*
    Checks if the number of nodes and components in the graph
    match *noNodes* and if the number of edges in the graph
    is zero.
     */
    private void testNewNodesAndEdges(int noNodes) {
        assertEquals(graph.countNodes(), noNodes);
        assertEquals(graph.countEdges(), 0);
        assertEquals(graph.getComponents(), noNodes);
    }

    @Test
    public void testConstructor1() throws Exception {
        testNewNodesAndEdges(0);
    }

    @Test
    public void testConstructor2() throws Exception {
        graph = new Graph(10);
        testNewNodesAndEdges(10);
    }

    @Test
    public void testAddNodesCount() throws Exception {
        graph.addNodesCount(11);
        testNewNodesAndEdges(11);

        graph.addNodesCount(8);
        testNewNodesAndEdges(8);
    }

    @Test
    public void testDegree() throws Exception {

    }

    @Test
    public void testDegree1() throws Exception {

    }

    @Test
    public void testAdjList() throws Exception {

    }

    @Test
    public void testAddNode() throws Exception {

    }

    @Test
    public void testAddEdge() throws Exception {

    }

    @Test
    public void testDelEdge() throws Exception {

    }

    @Test
    public void testIsIsomorphic() throws Exception {

    }

    @Test
    public void testIsSubIsomorphic() throws Exception {

    }

    @Test
    public void testGetVertexCover() throws Exception {
        graph.addNodesCount(9);
        graph.addEdge(0, 1);
        graph.addEdge(2, 3);
        graph.addEdge(2, 4);
        graph.addEdge(3, 4);
        graph.addEdge(3, 5);
        graph.addEdge(3, 6);
        graph.addEdge(4, 6);
        graph.addEdge(6, 7);
        graph.addEdge(7, 8);
        graph.addEdge(8, 5);

        // check for each node in the graph, if an adjacent node
        // exists in the cover
        boolean[] cover = graph.getVertexCover();
        boolean foundNeighbour = false;
        for (int node1 = 0; node1 < graph.countNodes(); node1++) {
            for (int node2 = 0; node2 < graph.countNodes(); node2++) {
                if (graph.getEdge(node1, node2) && cover[node2]) {
                    foundNeighbour = true;
                } else {
                    break;
                }
            }
        }
        assertTrue(foundNeighbour);
    }
}