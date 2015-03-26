/*
 * The Cliquewidth parameter on graphs.
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.parameter;

import org.jgrapht.DirectedGraph;
import teo.isgci.grapht.*;
import teo.isgci.gc.*;
import teo.isgci.relation.*;

/**
 * Stores information about the cliquewidth of a graph.
 */
public class Cliquewidth extends GraphParameter {
    public Cliquewidth(String id, String name,
            DirectedGraph<GraphClass,Inclusion> g) {
        super(id, name, g);
    }


    /**
     * Do special deductions for a particular problem.
     * Deduce probe X from X.
     */
    protected void distributeSpecial() {
        for (GraphClass n : graph.vertexSet()) {
            if ( !(n instanceof ProbeClass) ||
                    getDerivedBoundedness(n).betterOrEqual(
                            Boundedness.BOUNDED) )
                continue;

            GraphClass base = ((ProbeClass) n).getBase();
            if (getDerivedBoundedness(base).betterOrEqual(Boundedness.BOUNDED))
                createProof(n, Boundedness.BOUNDED, "From the base class.");
        }
    }
}

/* EOF */
