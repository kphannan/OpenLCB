package org.openlcb.implementations.throttle;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author  Bob Jacobsen   Copyright 2012
 * @version $Revision$
 */
public class PackageTest extends TestCase {
    public void testStart() {
    }
    
    // from here down is testing infrastructure
    
    public PackageTest(String s) {
        super(s);
    }

    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {PackageTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(PackageTest.class);

        suite.addTest(Float16Test.suite());
        suite.addTest(ThrottleSpeedDatagramTest.suite());
        suite.addTest(ThrottleImplementationTest.suite());
        suite.addTest(RemoteTrainNodeTest.suite());
        suite.addTest(RemoteTrainNodeCacheTest.suite());

        suite.addTest(org.openlcb.implementations.throttle.dcc.PackageTest.suite());

        return suite;
    }
}
