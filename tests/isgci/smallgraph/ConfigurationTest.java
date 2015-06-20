package tests.isgci.smallgraph;

import org.junit.*;
import org.junit.rules.TestRule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import teo.isgci.smallgraph.Configuration;
import teo.isgci.smallgraph.Graph;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Test class for teo.isgci.smallgraph.Configuration.
 *
 *
 * Created by momits on 02.06.15.
 */
public class ConfigurationTest {

    // place to look for configuration samples
    private final String samplePath = "tests/data/configsamples/";

    // number of available configuration samples
    private final int numSamples = 54;

    private final String normalEdgeSep = "-";
    private final String optionalEdgeSep = "=";

    private Pattern noNodesPattern;
    private Pattern namePattern;
    private Pattern nodesPattern;
    private Pattern edgesPattern;

    private Configuration conf;

    /**
     * Class describing the differences between two sets of objects,
     * one coming from the sample and one from the current implementation.
     */
    private class Diff<E> {
        public final int sampleNo;
        public final int both;
        public final List<E> sampleOnly;
        public final List<E> internalOnly;

        /**
         * Set up the diff.
         *
         * @param sampleNo the sample the diff points against
         * @param sampleList the list of objects specified by the sample
         * @param internalList the list of objects calculated by the implementation
         */
        public Diff(int sampleNo,
                    List<E> sampleList,
                    List<E> internalList) {
            this(sampleNo, sampleList, internalList, (o1, o2) -> o1.equals(o2) ? 0 : -1);
        }

        /**
         * Set up the diff using a special comparison function.
         *
         * @param sampleNo the sample the diff points against
         * @param sampleList the list of objects specified by the sample
         * @param internalList the list of objects calculated by the implementation
         * @param c the comparison function for objects in the lists (only
         *          equality matters)
         */
        public Diff(int sampleNo,
                    List<E> sampleList,
                    List<E> internalList,
                    Comparator<E> c) {
            int both = 0;
            for (Iterator<E> sampleIt = sampleList.iterator(); sampleIt.hasNext();) {
                E sampleObj = sampleIt.next();
                for (Iterator<E> internalIt = internalList.iterator(); internalIt.hasNext();) {
                    E internalObj = internalIt.next();
                    if (c.compare(sampleObj, internalObj) == 0) {
                        sampleIt.remove();
                        internalIt.remove();
                        both++;
                    }
                }
            }
            this.sampleNo = sampleNo;
            this.both = both;
            this.sampleOnly = sampleList;
            this.internalOnly = internalList;
        }

        /**
         * @return true if sampleOnly and internalOnly are both empty
         */
        public boolean isMatch() {
            return sampleOnly.isEmpty() && internalOnly.isEmpty();
        }
    }

    public ConfigurationTest() {
        // patterns for parsing the Configuration spec
        noNodesPattern = Pattern.compile("\\{[0-9]*\\}");
        namePattern = Pattern.compile("\\[.*\\]");

        // patterns for parsing the Graph spec
        nodesPattern = Pattern.compile("\\[(([0-9]+,\\ )*[0-9]+)*\\]");
        // the following edges pattern only works if the first
        // occurrence of nodesPattern is removed from the matched
        // string first
        edgesPattern = Pattern.compile("\\[.*\\]");
    }

    @Rule
    public TestRule watcher = new TestWatcher() {
        protected void starting(Description description) {
            System.out.println("--> Running test: " + description.getMethodName());
        }
    };

    @Before
    public void setUp() throws Exception {
        conf = new Configuration();
    }

    @After
    public void tearDown() throws Exception {}

    /**
     * Prints out a nice tabular evaluation of a set of Diff-objects,
     *
     * @param diffList Diff objects to evaluate
     */
    private void evaluateDiffList(List<Diff> diffList) {
        if (diffList.size() > 0) {
            System.out.println("Some samples could not be matched.\n");
            System.out.format("%20s%20s%15s%15s\n", "Sample no", "In Sample only", "Internal only", "Both");

            for (Diff d : diffList) {
                System.out.format("%20s%20d%15d%15d\n", d.sampleNo, d.sampleOnly.size(), d.internalOnly.size(), d.both);
            }

            System.out.println();
            System.out.format("%10s%10d%20d%15d%15d\n", "Total:",
                    diffList.size(),
                    diffList.stream().map(d -> d.sampleOnly.size()).reduce(0, Integer::sum),
                    diffList.stream().map(d -> d.internalOnly.size()).reduce(0, Integer::sum),
                    diffList.stream().map(d -> d.both).reduce(0, Integer::sum));
        } else {
            System.out.println("All samples could be matched.");
        }
    }

    /**
     * Matches against a pattern and returns the first match.
     *
     * @param pattern pattern to be matched
     * @param string candidate string
     * @return the first matched substring
     * @throws Exception if string has no substring which matches
     */
    private String matchRegex(Pattern pattern, String string) throws Exception {
        Matcher m = pattern.matcher(string);
        if (m.find()) {
            return m.group();
        } else {
            throw new Exception("Pattern could not be matched: " + pattern.pattern());
        }
    }

    private String cutFirstAndLast(String s) {
        return s.substring(1, s.length() - 1);
    }

    /**
     * Adds an an edge to the internal Configuration object
     * using a string representation. If a, b are nodes then
     * "a - b" specifies a normal edge and
     * "a = b" an optional edge.
     *
     * See teo.isgci.smallgraph.Configuration.toString().
     *
     * @param edge string representation of the edge
     */
    private void addEdgeFromString(String edge) {
        String[] nodes;
        boolean optional = false;

        if (edge.contains(normalEdgeSep)) {
            nodes = edge.replace(" ", "").split(normalEdgeSep);
        } else if (edge.contains(optionalEdgeSep)) {
            nodes = edge.replace(" ", "").split(optionalEdgeSep);
            optional = true;
        } else {
            System.err.println("Unknown edge type: " + edge);
            return;
        }

        int node1 = new Integer(nodes[0]);
        int node2 = new Integer(nodes[1]);

        if (!optional) {
            conf.addEdge(node1, node2);
        } else {
            conf.addOptedge(node1, node2);
        }
    }

    /**
     * Configure the internal Configuration object according to the
     * string representation confSpec.
     * This representation is given through Configuration.toString().
     *
     * @param confSpec the string representation
     */
    private void setupConfigurationFromString(String confSpec) {
        try {
            Integer noNodes = new Integer(cutFirstAndLast(matchRegex(noNodesPattern, confSpec)));
            String name = cutFirstAndLast(matchRegex(namePattern, confSpec));

            String edgeSpec = confSpec.replace("\n", "").split("] ")[1];
            String[] edges = edgeSpec.split("; ");

            conf.addNodesCount(noNodes);
            conf.addName(name);

            for (String edge : edges) {
                addEdgeFromString(edge);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Create a graph from a string representation.
     *
     * @param graphSpec string representation of the graph, given by
     *                  org.jgrapht.graph.AbstractGraph.toString().
     * @return the newly created graph
     */
    private Graph createGraphFromString(String graphSpec) {
        try {
            String[] nodeSpecs = cutFirstAndLast(matchRegex(nodesPattern, graphSpec)).split(", ");

            String edges = matchRegex(edgesPattern,
                    graphSpec.replaceFirst(nodesPattern.pattern(), ""));

            String[] edgeSpecs = cutFirstAndLast(edges).split(", ");

            Graph g = new Graph(nodeSpecs.length);

            // split produces array containing the empty string, if edges are empty
            if (!edges.equals("[]")) {
                for (String edgeSpec : edgeSpecs) {
                    addGraphEdgeFromString(g, edgeSpec);
                }
            }
            return g;

        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Adds an edge from a string representation.
     *
     * @param g Graph to add the edge to
     * @param edgeSpec string representation of the edge as in
     *                 org.jgrapht.graph.AbstractGraph.toString()
     */
    private void addGraphEdgeFromString(Graph g, String edgeSpec) {
        String[] nodes = cutFirstAndLast(edgeSpec).split(",");
        g.addEdge(new Integer(nodes[0]), new Integer(nodes[1]));
    }

    /**
     * Reads in a sample from "tests/data/configsamples" and sets
     * up the internal Configuration object according to the sample spec.
     * Returns a list of all the specs in the same order as found
     * in the sample.
     *
     * @param sampleNo the number of the sample to parse
     * @return an array containing the different specs of the sample
     */
    private String[] readSample(int sampleNo) throws IOException {
        String file = String.format(samplePath + "sample-configuration-%02d", sampleNo);

        // Parse the sample configuration
        String confSample = new String(Files.readAllBytes(Paths.get(file)), StandardCharsets.UTF_8);
        String[] specs = confSample.split("(?m)^-- .*$");
        setupConfigurationFromString(specs[0]);

        return specs;
    }

    /**
     * Matches Configuration.getGraphs() against the representatives of one sample.
     *
     * @param sampleNo the number of the sample to parse
     * @return a Diff object containing the representatives which could not be
     * matched
     */
    private Diff<Graph> testGetGraphsConfigurationSample(int sampleNo) throws IOException {
        String contained = readSample(sampleNo)[1];

        List<Graph> confGraphs = new ArrayList<>();
        List<String> graphSpecs = new ArrayList<>(Arrays.asList(contained.trim().split("\n")));

        // create all graphs which are known to be in the configuration
        for (String graphSpec : graphSpecs) {
            confGraphs.add(createGraphFromString(graphSpec));
        }

        List<Graph> calculatedGraphs= new ArrayList<>(conf.getGraphs());

        return new Diff<>(sampleNo, confGraphs, calculatedGraphs, (g1, g2) -> g1.isIsomorphic(g2) ? 0 : -1);
    }

    /**
     * For each sample, matches the representatives of Configuration.getGraphs()
     * against the representatives of this sample.
     *
     * Prints detailed info on unmatched graphs.
     *
     * See data/configsamples/README.md for info on the sample files.
     *
     * @throws Exception
     */
    @Test
    public void testGetGraphs() throws Exception {
        List<Diff> diffList = new ArrayList<>();

        for (int sampleNo = 0; sampleNo < numSamples; sampleNo++) {
            Diff d = testGetGraphsConfigurationSample(sampleNo);
            if (!d.isMatch()) {
                diffList.add(d);
            }
        }

        evaluateDiffList(diffList);

        Assert.assertTrue(diffList.isEmpty());
    }

    /**
     * Matches Configuration.getAutomorphisms() against the automorphisms
     * of one sample.
     *
     * @param sampleNo the number of the sample to parse
     * @return a Diff object containing the permutations which could not be
     * matched
     */
    private Diff<String> testGetAutomorphismsConfigurationSample(int sampleNo) throws IOException {
        List<String> sampleAutomorphisms = new ArrayList<>(Arrays.asList(readSample(sampleNo)[3].trim().split("\n")));

        List<String> calculatedAutomorphisms = new ArrayList<>();
        for (Integer[] p : conf.getAutomorphisms()) {
            calculatedAutomorphisms.add(Arrays.toString(p));
        }

        return new Diff<>(sampleNo, sampleAutomorphisms, calculatedAutomorphisms);
    }

    /**
     * For each sample, matches the result of Configuration.getAutomorphisms()
     * against the automorphisms of this sample.
     *
     * Prints detailed info on unmatched permutations.
     *
     * See data/configsamples/README.md for info on the sample files.
     *
     * @throws Exception
     */
    @Test
    public void testGetAutomorphisms() throws Exception {
        List<Diff> diffList = new ArrayList<>();

        for (int sampleNo = 0; sampleNo < numSamples; sampleNo++) {
            Diff d = testGetAutomorphismsConfigurationSample(sampleNo);
            if (!d.isMatch()) {
                diffList.add(d);
            }
        }

        evaluateDiffList(diffList);

        Assert.assertTrue(diffList.isEmpty());
    }
}