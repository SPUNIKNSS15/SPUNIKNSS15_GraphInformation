/*
 * Some relation between two classes.
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
 * Records a relation between two classes.
 */
public abstract class AbstractRelation extends RelationData {
    protected GraphClass gc1, gc2;

    public GraphClass get1() {
        return gc1;
    }

    public GraphClass get2() {
        return gc2;
    }
}

/* EOF */
