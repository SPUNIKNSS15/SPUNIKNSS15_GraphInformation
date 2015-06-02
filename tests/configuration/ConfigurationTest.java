package tests.configuration;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import teo.isgci.smallgraph.Configuration;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Test class for teo.isgci.smallgraph.Configuration.
 *
 *
 * Created by momits on 02.06.15.
 */
public class ConfigurationTest {

    // number of available configuration samples
    private int numSamples = 54;

    private String normalEdgeSep = "-";
    private String optionalEdgeSep = "=";

    private Pattern noNodesPattern;
    private Pattern namePattern;

    private Configuration conf;

    public ConfigurationTest() {
        // patterns for parsing the Configuration spec
        noNodesPattern = Pattern.compile("\\{[0-9]*\\}");
        namePattern = Pattern.compile("\\[.*\\]");
    }

    @Before
    public void setUp() throws Exception {
        conf = new Configuration();
    }

    @After
    public void tearDown() throws Exception {}

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
        return s.substring(1, s.length()-1);
    }

    /**
     * Adds an an edge to the internal Configuration object
     * using a string representation. If a, b are nodes then
     * "a - b" specifies a normal edge and
     * "a = b" an optional edge.
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

        Integer node1 = new Integer(nodes[0]);
        Integer node2 = new Integer(nodes[1]);

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

            System.out.println("noNodes: " + noNodes);
            conf.addNodesCount(noNodes);

            System.out.println("name: " + name);
            conf.addName(name);

            for (String edge : edges) {
                System.out.println("edge: " + edge);
                addEdgeFromString(edge);
            }

            System.out.println("how it looks:");
            System.out.println(conf);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Reads in a sample from "tests/configuration/configurations" and
     * sets up the internal Configuration object according to the sample spec.
     * Then Configuration.getGraphs() is matched against the graphs in the
     * sample.
     *
     * @param sampleNo the number of the sample to parse
     */
    private void testConfigurationSample(int sampleNo) {
        String file = String.format("tests/configuration/configurations/sample-configuration%02d", sampleNo);

        // Parse the sample configuration
        try {
            String confSample = new String(Files.readAllBytes(Paths.get(file)), StandardCharsets.UTF_8);

            String[] parts = confSample.split("All contained graphs:");
            setupConfigurationFromString(parts[0]);

            ArrayList<String> confGraphs = new ArrayList<String>(Arrays.asList(parts[1].split("\\n")));
            Collections.sort(confGraphs);

        } catch (IOException e) {
            e.printStackTrace();
        }


    }

    @Test
    public void testGetGraphs() throws Exception {
        for (int sampleNo = 0; sampleNo < numSamples; sampleNo++) {
            testConfigurationSample(sampleNo);
        }
    }

    @Test
    public void testGetGraphs1() throws Exception {

    }

    @Test
    public void testIsInducedSubgraph() throws Exception {

    }
}