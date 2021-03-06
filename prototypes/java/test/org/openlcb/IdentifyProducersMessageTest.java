package org.openlcb;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision$
 */
public class IdentifyProducersMessageTest extends TestCase {
    boolean result;
    
    public void testEqualsSame() {
        Message m1 = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
        Message m2 = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
    
        Assert.assertTrue(m1.equals(m2));
    }

    public void testNotEqualsDifferentNode() {
        Message m1 = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{99,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
        Message m2 = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
    
        Assert.assertTrue( ! m1.equals(m2));
    }

    public void testNotEqualsDifferentEvent() {
        Message m1 = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
        Message m2 = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{99,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
    
        Assert.assertTrue( ! m1.equals(m2));
    }

    public void testHandling() {
        result = false;
        Node n = new Node(){
            @Override
            public void handleIdentifyProducers(IdentifyProducersMessage msg, Connection sender){
                result = true;
            }
        };
        Message m = new IdentifyProducersMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}),
                                            new EventID(new byte[]{1,2,3,4,5,6,7,8}));
        
        n.put(m, null);
        
        Assert.assertTrue(result);
    }
    
    // from here down is testing infrastructure
    
    public IdentifyProducersMessageTest(String s) {
        super(s);
    }

    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {IdentifyProducersMessageTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(IdentifyProducersMessageTest.class);
        return suite;
    }
}
