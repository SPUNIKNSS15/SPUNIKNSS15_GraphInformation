/*
 * A common definition of Graph and Family
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.smallgraph;

import java.util.Collections;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

public abstract class SmallGraph {
    
    /** List of names, the first is the prime, the others are aliases */
    protected List<String> names;
    protected String link;
    protected SmallGraph complement;
    /**
     * Of a graph and its complement, one (the primary) is specified in the
     * xml-file, the other one (secondary) is created by complementing the
     * primary one.
     */
    protected boolean primary;
    /** Induced subgraphs common for all members of the SmallGraph */
    protected Vector< Vector<SmallGraph> > induced;
    
    /** Creates a new empty SmallGraph */
    public SmallGraph() {
        names = null;
        link = null;
        complement = null;
        primary = true;
        induced = null;
    }
    

    /** Adds a new name */
    public void addName(String parsedName){
        if (names == null)
            names = new ArrayList<>(2);
        names.add(parsedName);
    }
    
    /** Returns the first name. */
    public String getName(){
        return names == null ? null : names.get(0);
    }
    
    /** Returns the list with all names. */
    public List<String> getNames(){
        return Collections.unmodifiableList(names);
    }

    /** Returns a string between [] with all the names of this. */
    protected String namesToString() {
        int i, j;
        StringBuilder s = new StringBuilder();
        if (names != null) {
            s.append("[");
            j = names.size();
            for (i=0; i<j; i++) {
                s.append( names.get(i) );
                if (i < j-1)
                    s.append("=");
            }
            s.append("]");
        }
        return s.toString();
    }
    
    /** Adds a link */
    public void addLink(String parsedLink){
        link = parsedLink;
    }
    
    /** Returns a link to the drawing */
    public String getLink(){
        return link;
    }
    
    /** Sets complement */
    public void setComplement(SmallGraph comp){
        complement = comp;
    }
    
    /** Returns complement */
    public SmallGraph getComplement(){
        return complement;
    }
    
    public void setPrimary(boolean prim){
        primary = prim;
    }
    
    public boolean isPrimary(){
        return primary;
    }

    /** Adds induced <tt>parsedInduced</tt> to induced */
    public void addInduced(Vector<SmallGraph> parsedInduced){
        if (induced == null) {
            induced = new Vector<>();
        }
        induced.add(parsedInduced);
    }
    
    /** Returns Collection induced */
    public Vector< Vector<SmallGraph> > getInduced(){
        return induced;
    }

    
    @Override
    public int hashCode() {
        return this.getName().hashCode() + this.complement.getName().hashCode()*2;
    }


    /**
     * Builds a new SmallGraph instance which represents the complement of this.
     * Class specific actions have to be implemented in the respective subclass.
     *
     * @return new SmallGraph instance which represents the complementary data common to all subclasses.
     */
    public SmallGraph buildComplement() {

        SmallGraph c = null;
        try {
            c = getClass().newInstance();
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        c.setPrimary(false);
        c.setComplement(this);
        setComplement(c);

        return c;
    }
    
}

/* EOF */