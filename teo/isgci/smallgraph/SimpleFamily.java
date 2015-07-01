/*
 * Represents a simple family of graphs
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.smallgraph;

import java.util.Vector;

public class SimpleFamily extends Family{
    
    /**
     * @contains Smallgraphs contained in Family
     * @inducedRest Induced subgraphs common for all members of Family except for those in contains
     */
    private Vector<SmallGraph> contains;
    private Vector<Vector<SmallGraph> > inducedRest;

    /**
     * Creates a new SimpleFamily without Graphs
     */
    public SimpleFamily(){
        super();
        contains = null;
        induced = null;
        inducedRest = null;
    }


    /**
     * Builds a new SimpleFamily containing the complement
     * @return a new instance of SimpleFamily, containing the complement of this.
     */

    public SimpleFamily buildComplement() {
        SimpleFamily c = (SimpleFamily) super.buildComplement();

        //---- First copy the complement
        if (getContains() != null)
            c.contains = (Vector) getContains().clone();
        else
            contains = null;
        if (getInducedRest() != null) {
            c.inducedRest = new Vector<Vector<SmallGraph> >();
            for (Vector v : getInducedRest())
                c.inducedRest.addElement((Vector) v.clone());
        }
        else
            inducedRest = null;

        //---- Then complement
        int i, j;

        if (c.contains != null)
            for (i=0; i<c.contains.size(); i++)
                c.contains.setElementAt(c.contains.elementAt(i).getComplement(),i);

        if (c.inducedRest != null)
            for (i=0; i<c.inducedRest.size(); i++) {
                Vector<SmallGraph> v = c.inducedRest.elementAt(i);
                if (v != null)
                    for (j=0; j<v.size(); j++)
                        v.setElementAt(v.elementAt(j).getComplement(), j);
            }
        return c;
    }

    /**
     * Adds contains <tt>parsedContains</tt> to SimpleFamily
     *
     * @param parsedContains  smallgraph which content should be added to simpleFamily
     */
    public void addContains(SmallGraph parsedContains){
        if (contains == null)
            contains = new Vector<SmallGraph>(2,2);
        contains.addElement(parsedContains);
    }

    /**
     *
     * @return Vector contains
     */
    public Vector<SmallGraph> getContains(){
        return contains;
    }

    /**
     * Adds inducedRest <tt>parsedInducedRest</tt> to SimpleFamily
     *
     * @param parsedInducedRest inducedRest which should be added to SimpleFamily
     */
    public void addInducedRest(Vector<SmallGraph> parsedInducedRest){
        if (inducedRest == null)
            inducedRest = new Vector<Vector<SmallGraph> >(2,2);
        inducedRest.addElement(parsedInducedRest);
    }

    /**
     *
     * @return Vector inducedRest
     */
    public Vector<Vector<SmallGraph> > getInducedRest(){
        return inducedRest;
    }

    /**
     *
     * @return
     */
    public String toString(){
        int i, j;
        Vector v = null;
        String s = "Name: "+getName();
        if (contains != null) {
            s+="\nContains: ";
            for (i=0; i<contains.size(); i++)
                s+= contains.elementAt(i).getName()+"; ";
        }
        if (induced != null) {
            s+="\nInduced: ";
            for (i=0; i<induced.size(); i++) {
                v = (Vector) induced.elementAt(i);
                for (j=0; j<v.size()-1; j++)
                    s+=((SmallGraph) v.elementAt(j)).getName()+"; ";
            }
        }
        if (inducedRest != null) {
            s+="\nInducedRest: ";
            for (i=0; i<inducedRest.size(); i++) {
                v = inducedRest.elementAt(i);
                for (j=0; j<v.size()-1; j++)
                    s+=((SmallGraph) v.elementAt(j)).getName()+"; ";
            }
        }
        s+="\nLink: "+link+"\nComplement: "+complement.getName();
        return s;
    }
}
    
/* EOF */
