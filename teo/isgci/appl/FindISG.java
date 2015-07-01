/*
 * Find the relation between induced subgraphs.
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.appl;

import java.util.*;
import java.net.URL;
import java.io.*;
import java.util.concurrent.*;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.jgrapht.graph.*;
import gnu.getopt.Getopt;
import teo.isgci.grapht.GAlg;
import teo.isgci.xml.*;
import teo.isgci.smallgraph.*;

public class FindISG{

    private static Vector<Graph> graphs;
    private static Vector families, configurations, grammars;
    private static Hashtable<Graph, Vector<Graph>> inducedTable;
    private static SimpleDirectedGraph<Graph,DefaultEdge> resultGraph;

    private static int usg; // Running number for unknown subgraphs
    //private static Annotation<Graph> graphAnn; // Graph in resultGraph
    private static int minCnt=4; // Minimum number of nodes in small graphs
    private static int maxCnt=0; // The largest size of graphs given in the
                                 // beginning
    private static final String USG="USG", ISG="ISG";

    static final String XMLDECL =
        "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n" +
        "<!DOCTYPE SMALLGRAPHS SYSTEM \"smallgraphs.dtd\">\n";

    private static boolean noComplements = false;  // Don't handle complements

    private static int verbose = 0;

    public static void main(String args[]) throws IOException,
            InterruptedException {
        boolean transitivelyClosed = false;

        long t1,t2,totalStart,totalEnd;
        int c;

        graphs = new Vector();
        families = new Vector();
        configurations = new Vector();
        grammars = new Vector();
        inducedTable = new Hashtable();
        resultGraph = new SimpleDirectedGraph<>(
                DefaultEdge.class);
        usg=0;

        totalStart = System.currentTimeMillis();

        Getopt opts = new Getopt("FindISG", args, "ctv");
        while ((c = opts.getopt()) != -1) {
            switch (c) {
                case 'c':
                    noComplements = true;
                    break;
                case 't':
                    transitivelyClosed = true;
                    break;

                case 'v':
                    verbose++;
                    break;

                default:
                    usage();    /* doesn't return */
                    break;
            }
        }

        /* stimmt Anzahl der verbliebenen Parameter? */
        if (args.length != opts.getOptind() + 2) {
            usage();    /* doesn't return */
        }

        String inxml = args[opts.getOptind()];
        String outxml = args[opts.getOptind()+1];

        if (verbose != 0)
            System.out.println("Lese " + inxml + " ein");

        t1=System.currentTimeMillis();
        try{
            readXMLFile(inxml);
            /*
            See comments on readXMLFile. The function does far more than
             just xml reading: All graphs/families/grammars/ are added
             to this's respective member vectors and generated graphs
             from e.g. families are linked to already known graphs
             from the graphs vector to obtain infos names, already known
             subgraph isomorphisms etc.

             readXML excessively uses the respective isomorphy checking methods
             */

        }catch(Exception ex){
            ex.printStackTrace();//System.out.println(ex);
            System.exit(1);
        }
        t2=System.currentTimeMillis();

        if (verbose != 0) {
            long zeit = t2 - t1;
            System.out.print(". (" + time2String(zeit) + ")\n");

            System.out.print("Graphen         : " + graphs.size() + "\n");
            System.out.print("Familien        : " + families.size() + "\n");
            System.out.print("Konfigurationen : " + configurations.size() + "\n");

            System.out.print("Bestimme Teilgraphen\n");
        }

        t1=System.currentTimeMillis();

        /* find subgraph relations among known graphs */
        createSubgraphRelations();
        GAlg.transitiveReduction(resultGraph);

        t2=System.currentTimeMillis();

        if (verbose != 0) {
            long zeit = t2 - t1;

            System.out.print(". ("+ time2String(zeit) + ")\n");
            System.out.print("Graphen         : " + graphs.size() + "\n");
            System.out.print("Familien        : " + families.size() + "\n");
            System.out.print("Konfigurationen : " + configurations.size()
                            + "\n");
            System.out.print("Relationen      : " + resultGraph.edgeSet().size() + "\n");

            System.out.print("Bestimme Teilgraphen der Konfigurationen");
        }

        t1=System.currentTimeMillis();

        /*
        add every KNOWN graph that is induced subgraph of EVERY of the
        configurations graphs to the induced subgraph list of C
         */
        for (int i = 0; i < configurations.size(); i++) {
            graphs: for (int j = 0; j < graphs.size(); j++) {
                Configuration conf = (Configuration) configurations.elementAt(i);
                Graph g = (Graph) graphs.elementAt(j);

                /* Information about subisomorphic graphs is already there in inducedTable
                 * this jields more complete results than old method, which could miss on
                 * some induced graphs */
                for (SmallGraph representative : conf.getContains()) {
                    Graph rep = (Graph)representative;
                    /* continue if not induced by all representatives */
                    if (g != rep && !inducedTable.get(rep).contains(g)) {
                        continue graphs;
                    }
                }

                /* at this point, we know that g is induced by all representatives of conf */
                System.out.print("  Graph " + g.getName() + " ist in allen Repräsentanten von "
                        + conf.getName() + " enthalten \n");
                conf.addInduced(g);
            }
        }

        t2=System.currentTimeMillis();

        if (verbose != 0) {
            long zeit = t2 - t1;

            System.out.print(". ("+ time2String(zeit) + ")\n");

            System.out.print("Graphen         : " + graphs.size() + "\n");
            System.out.print("Familien        : " + families.size() + "\n");
            System.out.print("Konfigurationen : " + configurations.size()
                            + "\n");

            System.out.print("Sortiere Graphen");
        }

        t1=System.currentTimeMillis();

        /*Sort graphs by their number of nodes*/
        sortNum(graphs,0,graphs.size()-1);
        t2=System.currentTimeMillis();

        if (verbose != 0) {
            long zeit = t2 - t1;

            System.out.print(". (" + time2String(zeit) + ")\n");
            System.out.println("Schreibe " + outxml);
        }

        System.out.println("Digraph is made. Starting to add big smallmembers");
        addBigSmallmembers();

        /* if -t option is set */
        if (transitivelyClosed) {
            GAlg.transitiveClosure(resultGraph);
        }

        t1=System.currentTimeMillis();
        try{
            /* write out graphs, families, grammars...
            * with the newly calculated relationships
            */
            makeNewXMLFile(outxml);
        }catch(Exception ioe){
            ioe.printStackTrace();
        }
        t2=System.currentTimeMillis();

        totalEnd = System.currentTimeMillis();

        if (verbose != 0) {

            System.out.print("Relationen gesamt: " + resultGraph.edgeSet().size() + "\n");

            long zeit = t2-t1;
            long totalTime = totalEnd - totalStart;

            System.out.print(". (" + time2String(zeit) + ")\n");
            System.out.print("Total time: " + time2String(totalTime)+ "\n");
        }
    }

    private static void usage(){
        System.out.println(
            "Usage: FindISG [opts] input.xml out.xml\n"+
            "   -c: don't handle complements\n"+
            "   -t: create transitively closed out.dig\n"+
            "   -v: be verbose\n");
        System.exit(1);
    }

    /* konvertiert eine Zeit in ms in eine Zeichenkette */
    private static String time2String(long zeit)
    {
        /* kleiner eine Sekunde */
        if (zeit < 1500) {
            return zeit + "ms";

        } else if (zeit < 120000) {
            return (zeit + 500)/1000 + "s (" + zeit + "ms)";

        /* im Minutenbereich */
        } else {
            return (zeit + 500)/1000/60 + "m (" + zeit + "ms)";
        }
    }


    /** Creates a new XML file */
    public static void makeNewXMLFile(String outFile)
                throws IOException, SAXException{
        SmallGraphWriter sgw=new SmallGraphWriter(new FileWriter(outFile));
        sgw.writeSmallGraphs(XMLDECL, graphs, grammars, families,
                configurations, resultGraph);
    }

    /**
     * finds subgraph relations amongst known simple graphs
     * (inside graphs)
     * thereby building transitive hull in resultgraph
     * of simple graph relations
     * and saving the results in inducedTable
     */
    public static void createSubgraphRelations() {
        /* add node to resultGraph for each graph in graphs */
        for (Graph g : graphs) {
            resultGraph.addVertex(g);
        }

        /* exclude bigger complements */
        HashSet<Graph> thinnerCounterparts = new HashSet<>(graphs);
        for (Graph g : graphs) {
            Graph c = (Graph)g.getComplement();
            if (g == c || !(thinnerCounterparts.contains(g) && thinnerCounterparts.contains(c))) {
                /* already excluded complement or graph is self-complementary */
                continue;
            }
            /* remove bigger counterpart from Hashset */
            if(g.getGraph().edgeSet().size() > c.getGraph().edgeSet().size()) {
                thinnerCounterparts.remove(g);
            } else {
                thinnerCounterparts.remove(c);
            }
        }

        ExecutorService poolExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        /* for synchronizing access to resultgraph data structure */
        Semaphore resultGraphSem = new Semaphore(1);
        /* for synchronizing access to to induced table */
        Semaphore inducedTableSem = new Semaphore(1);

        /* start search for each thinner graph or complement */
        for (Graph g : thinnerCounterparts) {
            /* search for induced subgraphs without using information
             * about transitively subisomorphic graphs, thus building transitive hull
             * in resultGraph */
            poolExecutor.execute(new AddSubgraphRelationsTask(graphs, g, (Graph)g.getComplement(),
                    resultGraph, inducedTable,
                    resultGraphSem, inducedTableSem, false));
        }

        poolExecutor.shutdown();
        try {
            poolExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
        } catch (InterruptedException e) {}
    }

    /**
     * Reads the graphs from XML format (their names, nodes, edges and aliases
     * and links, if there are any).
     */
    private static void readXMLFile(String file) throws Exception{
        SmallGraphReader handler = new SmallGraphReader();
        int i, j, ci;


        URL url = new File(file).toURI().toURL();
        InputSource input = new InputSource(url.openStream());
        input.setSystemId(url.toString());

        /**
         * XML Parser is a wrapper for the external XMLReader class
         *
         * @param input - Input source - an open stream, connected to file
         *                (most likely the input XML from main())
         *
         * @param handler - Content Handler - stores the read data and
         *                sanitizes it, see SmallGraphReader
         *
         * @param EntityResolver - Unused in this case, specified by the external
         *                       XMLReader
         *
         * @param NoteFilter - Declaration of XML tags used in the input format
         */
        XMLParser xr = new XMLParser(input, handler, null,
                new NoteFilter(SmallGraphTags.EXPL));
        xr.parse();

        System.out.println("parsed graphs");


        /*
         * Store all read graphs, which by now are in the handler,
         * in the respective local member vectors
         *
         * (this also applies for configs/families/grammars respectively)
         */

        for (HMTGrammar gram : handler.getGrammars())
            grammars.addElement(gram);

        Collection<SmallGraph> smallGraphs = handler.getGraphs();

        for (SmallGraph g : smallGraphs) {
            System.out.println(g.getClass());
            if (noComplements  &&  !g.isPrimary()) {
                continue;
            }

            if (g instanceof Graph) {
                graphs.addElement((Graph)g);
                System.out.println("added Graph");
            }
            else if (g instanceof Family) {
                families.addElement(g);
                System.out.println("added Family");
            }
            else if (g instanceof Configuration) {
                configurations.addElement(g);
                System.out.println("added Configuration");
            }
            else {
                System.out.println("Don't know how to handle " + g.getName());
            }
        }


        /*
         * Check all graphs against each other for isomorphisms -
         * if 2 isomorphic graphs are found, print an error message
         *
         * XXX This is in O(graphs.size()^2),
         * This can be optimized for inputs with many isomorphic graphs
         * since isomorphy is transitive.
         */
        for (i=0; i<graphs.size(); i++) {
            for (j = i + 1; j < graphs.size(); j++) {
                if (((Graph) graphs.elementAt(i)).
                        isIsomorphic((Graph) graphs.elementAt(j)))
                    System.out.println("Mistake!!! " +
                            ((Graph) graphs.elementAt(i)).getName() +
                            " isomorphic to " +
                            ((Graph) graphs.elementAt(j)).getName());
            }
        }

        System.out.println("checked Graphs");

        /*
         * Iterates over the configurations vector and checks whether
         * the configuration generates too many representatives, what means
         * more than 100 in this case. (see getGraphs())
         */
        for (ci = 0; ci < configurations.size(); ci++) {
            Configuration c = (Configuration) configurations.elementAt(ci);

            /* returns null when there are >100 representatives */
            Vector contained = c.getGraphs(100);

            if (contained == null) {
               System.out.print("Warning: " + c.getName()
                               + " has too many representatives\n");
                continue;
            }

            /* Iterates over all graphs generated by the current configuration
             * (not more than 100) and checks them against the FindISG.graphs
             * member vector for isomorphisms.
             */
            contConf:   for (i=0; i<contained.size(); i++) {
                for (j=0; j<graphs.size(); j++)
                    if (((Graph)contained.elementAt(i)).
                            isIsomorphic((Graph)graphs.elementAt(j))) {
                        /* If graph[j] is isomorphic to one of the graphs
                         * generated by the current configuration c, add this
                         * graph to c's contains vector
                         */
                        c.addContains((Graph)graphs.elementAt(j));
                        /* Continue with the next configuration
                         * We assume the graphs in the graphs vector aren't
                         * isomorphic to each other, so we won't find another
                         * one here
                         */
                        continue contConf;
                    }
                /* We found all the graphs, to which one of the graphs
                 * generated by configuration c is isomorphic.
                 */
                ((Graph)contained.elementAt(i)).addLink(c.getLink());
                /*
                 * add the link to the found isomorphic graph from graphs
                 * to the configurations link list and vice versa
                 */
                addUSG((Graph)contained.elementAt(i), graphs, ISG);
                c.addContains((Graph)contained.elementAt(i));
            }
        }


        /*
        Check configurations for isomorphism against each other,
        as with graphs before
         */
        for (i=0; i<configurations.size(); i++) {
            for (j = i + 1; j < configurations.size(); j++) {
                if (((Configuration) configurations.elementAt(i)).
                        isIsomorphic((Configuration) configurations.elementAt(j)))
                    System.out.println("Mistake!!! " +
                            ((Configuration) configurations.elementAt(i)).getName() +
                            " isomorphic to " +
                            ((Configuration) configurations.elementAt(j)).getName());
            }
        }

		System.out.println("checked Configurations");

        int curCnt = 0;
        for (i=0; i<graphs.size(); i++){
            curCnt = ((Graph)graphs.elementAt(i)).countNodes();
            if (curCnt>maxCnt)
                maxCnt = curCnt;
            if (curCnt < minCnt)
                minCnt = curCnt;
        }

        for (i=0; i<families.size(); i++) {
            if (families.elementAt(i) instanceof HMTFamily) {
                if (((HMTFamily) families.elementAt(i)).getGrammar() != null) {
                    HMTFamily fhmt = (HMTFamily) families.elementAt(i);

                    /*
                    Initialize the fmht.smallmembers Vector with smallgraphs
                    conforming to FHMTGrammar with a maximum of maxCnt nodes
                     */
                    fhmt.initFromGrammar(maxCnt);

                    /*
                    Iterate over all of fhmt.smallmembers graphs which were
                    previously generated and check, whether one of the generated
                    smallgraphs is isomorphic to one of the graphs
                    in FindISG.graphs.
                     */
                    Vector smMem = fhmt.getSmallmembers();
                    contFHMT: for (j = 0; j < smMem.size(); j++) {
                        if (((Graph) smMem.elementAt(j)).countNodes() <= maxCnt) {
                            for (int k = 0; k < graphs.size(); k++)
                                if (((Graph) smMem.elementAt(j)).isIsomorphic(
                                        graphs.elementAt(k))) {

                                    //Found an isomorphic graph, set it to the
                                    //element from graphs, so additional info
                                    //like names, links etc. are available
                                    smMem.setElementAt(
                                            graphs.elementAt(k), j);
                                    continue contFHMT;
                                }
                            ((Graph) smMem.elementAt(j)).addLink(fhmt.getLink());
                            addUSG((Graph) smMem.elementAt(j), graphs, ISG);
                        }
                    }
                }
            }
        }

        // Completing information about ComplementFamilies which are complement
        // to HMTFamilies with HMT-grammars
        for (i=0; i<families.size(); i++) {
            if ((families.elementAt(i) instanceof HMTFamily) &&
                    !((Family)families.elementAt(i)).isPrimary()) {
                HMTFamily fcomp = (HMTFamily)families.elementAt(i);
                HMTFamily fhmt = (HMTFamily)fcomp.getComplement();

                //fcomp.smallmembers = new Graph[fhmt.smallmembers.length];
                Vector smMem = fhmt.getSmallmembers();
                Vector compSmMem = new Vector();
                for (j=0; j<smMem.size(); j++)
                    if (((Graph)smMem.elementAt(j)).countNodes()<=maxCnt)
                        compSmMem.addElement(((Graph)smMem.elementAt(j)).
                                getComplement());
                fcomp.setSmallmembers(compSmMem);
            }
        }

        System.out.println("checked Families");
    }

    private static void addUSG(Graph g, Vector graphs, String type){
        usg++;
        g.addName(type+usg);
        Graph co = (Graph)g.makeComplement();
        usg++;
        co.addName(type+usg);
        graphs.addElement(g);
        graphs.addElement(co);
    }

    /** Name of Graph TO Number of nodes */
    private static int n2n(Object graph){
        return graph==null ? -1 : ((Graph)graph).countNodes();
    }

    /** Sorts graphs by their number of nodes */
    private static void sortNum(Vector vec, int left, int right) {
        Object y;
        int x;
        int i = left;
        int j = right;
        // x is the element for comparison. (pivot)
        x = n2n(vec.elementAt((i+j)>>1));
        while (i <= j) {
            while(n2n(vec.elementAt(i))<x) i++;
            while(n2n(vec.elementAt(j))>x) j--;
            if (i <= j) {
                // Swap vec.elementAt(i) and vec.elementAt(j)
                y = vec.elementAt(i);
                vec.setElementAt(vec.elementAt(j),i);
                vec.setElementAt(y,j);
                i++;
                j--;
            }
        }
        if(left < j) sortNum(vec, left, j);
        if(i < right) sortNum(vec, i, right);
    }

    /*
    addBigSmallmembers operates on the families smallmembers which
    had more than maxCnt nodes and were therefore not processed yet

    Every bigSmallgraph is either unknown (and then added to bigSmallmemb)
    or induces an already known bigSmallgraph according to VF2
     */
    public static void addBigSmallmembers() throws
            IOException {

        /* Contains graphs of size larger than maxCnt*/
        Vector<Graph> bigSmallmemb = new Vector();

        /* iterate over all families in this.families */
        for (int i=0; i<families.size(); i++) {
            if (families.elementAt(i) instanceof HMTFamily) {
                if (((HMTFamily) families.elementAt(i)).getGrammar() != null) {
                    HMTFamily fhmt = (HMTFamily) families.elementAt(i);
                    HMTFamily fcomp = (HMTFamily) fhmt.getComplement();
                    Vector<SmallGraph> smMem = fhmt.getSmallmembers();
                    Vector compSmMem = new Vector();

                    //iterate over all of the families small members...
                    contBig:
                    for (int j = 0; j < smMem.size(); j++) {

                        if (((Graph) smMem.elementAt(j)).countNodes() > maxCnt) {

                            for (int k = 0; k < bigSmallmemb.size(); k++) {
                                /* If one of the already found big smallmembers
                                 * (stored in bigSmallmemb) is isomorphic...
                                 */
                                if (((Graph) smMem.elementAt(j)).isIsomorphic(bigSmallmemb.elementAt(k))) {
                                    /* ...set the graph and its complement within
                                     * the family accordingly, copying all known
                                     * information about that graph
                                     */
                                    smMem.setElementAt(bigSmallmemb.elementAt(k), j);
                                    compSmMem.addElement((bigSmallmemb.elementAt(k)).getComplement());
                                    continue contBig;
                                }
                            }

                            //If no isomorphic big smallgraph is found,
                            //add it as an USG to bigSmallmemb
                            ((Graph) smMem.elementAt(j)).addLink(fhmt.getLink());
                            addUSG((Graph) smMem.elementAt(j), bigSmallmemb, ISG);
                            compSmMem.addElement(smMem.
                                    elementAt(j).getComplement());
                            /* System.out.println(fhmt.getName()+".smMem["+j+"]="+
                                    ((Graph)smMem.elementAt(j)).getName());
                            System.out.println(fcomp.getName()+".smMem["+j+"]="+
                                   ((Graph)compSmMem.elementAt(j)).getName());*/
                        } else if (fcomp.getSmallmembers() != null) {
                            compSmMem.addElement(fcomp.getSmallmembers().elementAt(j));
                        }
                    }

                    fcomp.setSmallmembers(compSmMem);
                }
            }
        }

        Vector<Graph> topo = new Vector<>();

        /*
        Add the present resultgraphs members to topo according
        to their topological order, which is provided by jgrapht
         */
        for (Graph v : GAlg.topologicalOrder(resultGraph)) {
            topo.add(v);
        }

        /* prepare new result graph nodes:
         *  add a vertex to resultGraph for every element of bigSmallmemb
         */
        for (Graph g : bigSmallmemb) {
            resultGraph.addVertex(g);
        }

        System.out.println("All big smallmembers are added.");

        /* exclude bigger complements */
        HashSet<Graph> thinnerCounterparts = new HashSet<>(bigSmallmemb);
        for (Graph g : bigSmallmemb) {
            Graph c = (Graph)g.getComplement();
            if (g == c || !(thinnerCounterparts.contains(g) && thinnerCounterparts.contains(c))) {
                /* already excluded complement or graph is self-complementary */
                continue;
            }
            /* remove bigger counterpart from Hashset */
            if(g.getGraph().edgeSet().size() > c.getGraph().edgeSet().size()) {
                thinnerCounterparts.remove(g);
            } else {
                thinnerCounterparts.remove(c);
            }
        }

        ExecutorService poolExecutor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        /* for synchronizing access to resultgraph data structure */
        Semaphore resultGraphSem = new Semaphore(1);
        /* for synchronizing access to to induced table */
        Semaphore inducedTableSem = new Semaphore(1);

        /* start search for each thinner graph or complement */
        for (Graph g : thinnerCounterparts) {
            /* check for every of the bigSmallgraphs whether it induces any one of
             * the already known graphs in resultGraph
             * without building transitive relations in resultGraph
             */
            poolExecutor.execute(new AddSubgraphRelationsTask(topo, g, (Graph)g.getComplement(),
                    resultGraph, inducedTable,
                    resultGraphSem, inducedTableSem, true));
        }
        poolExecutor.shutdown();
        try {
            poolExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
        } catch (InterruptedException e) {}

        /* Add all graphs from bigSmallmemb to the global graph list */
        for (int i=0; i<bigSmallmemb.size(); i++) {
            graphs.addElement(bigSmallmemb.elementAt(i));
        }
    }
}

/* EOF */
