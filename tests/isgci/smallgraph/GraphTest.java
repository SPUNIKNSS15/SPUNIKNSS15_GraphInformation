package tests.isgci.smallgraph;

import org.junit.Test;
import teo.isgci.smallgraph.Graph;
import teo.isgci.smallgraph.SmallGraph;

import static org.junit.Assert.*;

/**
 * Created by tassilokarge on 21.06.15.
 */
public class GraphTest {

    @Test
    public void testIsIsomorphic() throws Exception {

        Graph x171Integer = new Graph();
        x171Integer.addNodesCount(6);
        x171Integer.addEdge(new Integer(0), new Integer(1));
        x171Integer.addEdge(new Integer(1), new Integer(2));
        x171Integer.addEdge(new Integer(2), new Integer(3));
        x171Integer.addEdge(new Integer(3), new Integer(4));
        x171Integer.addEdge(new Integer(5), new Integer(2));
        x171Integer.addEdge(new Integer(5), new Integer(3));
        x171Integer.addEdge(new Integer(5), new Integer(4));

        Graph x170Integer = new Graph();
        x170Integer.addNodesCount(6);
        x170Integer.addEdge(new Integer(0), new Integer(1));
        x170Integer.addEdge(new Integer(1), new Integer(2));
        x170Integer.addEdge(new Integer(2), new Integer(3));
        x170Integer.addEdge(new Integer(4), new Integer(5));
        x170Integer.addEdge(new Integer(4), new Integer(2));
        x170Integer.addEdge(new Integer(5), new Integer(2));
        x170Integer.addEdge(new Integer(5), new Integer(3));


        /*** fails ***/
        assertFalse(x171Integer.isIsomorphic(x170Integer));
        /*** does not fail ***/
        assertFalse(x170Integer.isIsomorphic(x171Integer));

        Graph x171int = new Graph();
        x171int.addNodesCount(6);
        x171int.addEdge(0, 1);
        x171int.addEdge(1, 2);
        x171int.addEdge(2, 3);
        x171int.addEdge(3, 4);
        x171int.addEdge(5, 2);
        x171int.addEdge(5, 3);
        x171int.addEdge(5, 4);

        Graph x170int = new Graph();
        x170int.addNodesCount(6);
        x170int.addEdge(0, 1);
        x170int.addEdge(1, 2);
        x170int.addEdge(2, 3);
        x170int.addEdge(4, 5);
        x170int.addEdge(4, 2);
        x170int.addEdge(5, 2);
        x170int.addEdge(5, 3);

        /*** fails ***/
        assertFalse(x171int.isIsomorphic(x170int));
        /*** does not fail ***/
        assertFalse(x170int.isIsomorphic(x171int));


        /*** those two tests pass ***/
        Graph x170IntegerEdgeDirChanged = new Graph();
        x170IntegerEdgeDirChanged.addNodesCount(6);
        x170IntegerEdgeDirChanged.addEdge(new Integer(0), new Integer(1));
        x170IntegerEdgeDirChanged.addEdge(new Integer(1), new Integer(2));
        x170IntegerEdgeDirChanged.addEdge(new Integer(2), new Integer(3));
        x170IntegerEdgeDirChanged.addEdge(new Integer(5), new Integer(4));
        x170IntegerEdgeDirChanged.addEdge(new Integer(4), new Integer(2));
        x170IntegerEdgeDirChanged.addEdge(new Integer(5), new Integer(2));
        x170IntegerEdgeDirChanged.addEdge(new Integer(5), new Integer(3));


        assertFalse(x171Integer.isIsomorphic(x170IntegerEdgeDirChanged));
        assertFalse(x170IntegerEdgeDirChanged.isIsomorphic(x171Integer));

        Graph x170intEdgeDirChanged = new Graph();
        x170intEdgeDirChanged.addNodesCount(6);
        x170intEdgeDirChanged.addEdge(0, 1);
        x170intEdgeDirChanged.addEdge(1, 2);
        x170intEdgeDirChanged.addEdge(2, 3);
        x170intEdgeDirChanged.addEdge(5, 4);
        x170intEdgeDirChanged.addEdge(4, 2);
        x170intEdgeDirChanged.addEdge(5, 2);
        x170intEdgeDirChanged.addEdge(5, 3);

        assertFalse(x171int.isIsomorphic(x170intEdgeDirChanged));
        assertFalse(x170intEdgeDirChanged.isIsomorphic(x171int));
    }
}