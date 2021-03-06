package org.openlcb;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision$
 */
public class VerifyNodeIDNumberMessageTest extends TestCase {
    boolean result;
    
    public void testEqualsSame() {
        Message m1 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}) );
        Message m2 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}) );
    
        Assert.assertTrue(m1.equals(m2));
    }

    public void testEqualsSameWithContent() {
        Message m1 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}), new NodeID(new byte[]{1,2,3,4,5,6}));
        Message m2 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}), new NodeID(new byte[]{1,2,3,4,5,6}) );
    
        Assert.assertTrue(m1.equals(m2));
    }

    public void testNotEqualsDifferent() {
        Message m1 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}) );
        Message m2 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,3,3,4,5,6}) );
    
        Assert.assertTrue( ! m1.equals(m2));
    }

    public void testEqualsContentMatters() {
        Message m1 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}), new NodeID(new byte[]{1,2,3,4,5,6}) );
        Message m2 = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}), new NodeID(new byte[]{1,2,3,4,5,0}) );
    
        Assert.assertTrue( ! m1.equals(m2));
    }

    public void testHandling() {
        result = false;
        Node n = new Node(){
            @Override
            public void handleVerifyNodeIDNumber(VerifyNodeIDNumberMessage msg, Connection sender){
                result = true;
            }
        };
        Message m = new VerifyNodeIDNumberMessage(
                                            new NodeID(new byte[]{1,2,3,4,5,6}) );
        
        n.put(m, null);
        
        Assert.assertTrue(result);
    }
    
    // from here down is testing infrastructure
    
    public VerifyNodeIDNumberMessageTest(String s) {
        super(s);
    }

    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {VerifyNodeIDNumberMessageTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(VerifyNodeIDNumberMessageTest.class);
        return suite;
    }
}
