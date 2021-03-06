/*
 * A convenience form of ..sax.Attributes with just qname and value
 *
 * $Id$
 *
 * This file is part of the Information System on Graph Classes and their
 * Inclusions (ISGCI) at http://www.graphclasses.org.
 * Email: isgci@graphclasses.org
 */

package teo.isgci.xml;

import org.xml.sax.helpers.*;

public class SimpleAttributes extends AttributesImpl {
    public void addAttribute(String locName, String value) {
        addAttribute("", locName, "", "", value);
    }
}

/* EOF */
