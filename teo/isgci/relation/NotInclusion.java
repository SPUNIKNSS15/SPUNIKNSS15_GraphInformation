/*
 * A parameter does not bound another.
 * @author vector
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.relation;

import teo.isgci.gc.GraphClass;

/**
 * The Relation "bounds" (<=) between parameters will be realized as Inclusion
 * between parameter pseudoclasses. For not <= this relation is needed.
 */
public class NotInclusion extends AbstractRelation {
    /**
     * Create a new par1 not >= par2.
     * @param par1 the first parameter-PseudoClass
     * @param par2 the second parameter-PseudoClass
     */
    public NotInclusion(GraphClass par1, GraphClass par2) {
        gc1 = par1;
        gc2 = par2;
    }

    @Override
    public String toString() {
        return gc1 + " not -> " + gc2;
    }
}

/* EOF */
