package org.nmra.net.can;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision: 23 $
 */
public class CanTest extends TestCase {
    public void testStart() {
    }
    
    // from here down is testing infrastructure
    
    public CanTest(String s) {
        super(s);
    }

    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {CanTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(CanTest.class);

        suite.addTest(NmraNetCanFrameTest.suite());
        suite.addTest(NIDaTest.suite());
        suite.addTest(NIDaAlgorithmTest.suite());

        return suite;
    }
}
