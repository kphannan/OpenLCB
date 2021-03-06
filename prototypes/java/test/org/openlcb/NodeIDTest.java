package org.openlcb;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision$
 */
public class NodeIDTest extends TestCase {
    public void testNullArg() {
        try {
            new NodeID((byte[])null);
        } catch (IllegalArgumentException e) { return; }
        Assert.fail("Should have thrown exception");
    }

    public void testTooLongArg() {
        // shouldn't throw, just takes 1st part
        new NodeID(new byte[]{1,2,3,4,5,6,7});
    }

    public void testTooShortArg() {
        try {
            new NodeID(new byte[]{1,2,3,4,5});
        } catch (IllegalArgumentException e) { return; }
        Assert.fail("Should have thrown exception");
    }
    
    public void testOKArg() {
        new NodeID(new byte[]{1,2,3,4,5,6});
    }
    
    public void testNullStringArg() {
        try {
            new NodeID((String)null);
        } catch (IllegalArgumentException e) { return; }
        Assert.fail("Should have thrown exception");
    }

    public void testTooLongStringArg() {
        // shouldn't throw, just takes 1st part
        new NodeID("1.2.3.4.5.6.7");
    }

    public void testTooShortStringArg() {
        try {
            new NodeID("1.2.3.4.5");
        } catch (IllegalArgumentException e) { return; }
        Assert.fail("Should have thrown exception");
    }
    
    public void testOKStringArg() {
        new NodeID("1.2.3.4.5.6");
    }
    
    public void testEqualsSame() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        NodeID e2 = new NodeID(new byte[]{1,2,3,4,5,6});
        Assert.assertTrue(e1.equals(e2));
    }
    
    public void testEqualsSameString() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        NodeID e2 = new NodeID("1.2.3.4.5.6");
        Assert.assertTrue(e1.equals(e2));
    }
    
    public void testEqualsCastSame() {
        Object e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        NodeID e2 = new NodeID(new byte[]{1,2,3,4,5,6});
        Assert.assertTrue(e1.equals(e2));
    }
    
    public void testEqualsSelf() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        Assert.assertTrue(e1.equals(e1));
    }
    
    public void testEqualsCastSelf() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        Object e2 = e1;
        Assert.assertTrue(e1.equals(e2));
    }
    
    public void testNotEquals() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        NodeID e2 = new NodeID(new byte[]{1,3,3,4,5,6});
        Assert.assertTrue(!e1.equals(e2));
    }
    
    public void testNotEqualsOtherType() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        Object e2 = new Object();
        Assert.assertTrue(!e1.equals(e2));
    }

    public void testNodesAreNotEvents() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        EventID e2 = new EventID(new byte[]{1,0,0,0,0,0,1,0});
        Assert.assertTrue(!e1.equals(e2));
    }

    public void testGetContents() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        byte[] contents;
        
        contents = e1.getContents();
        
        Assert.assertTrue(contents.length==6);
        Assert.assertTrue(contents[0] == 1);
        Assert.assertTrue(contents[1] == 2);
        Assert.assertTrue(contents[2] == 3);
        Assert.assertTrue(contents[3] == 4);
        Assert.assertTrue(contents[4] == 5);
        Assert.assertTrue(contents[5] == 6);
    }

    public void testImmutable() {
        NodeID e1 = new NodeID(new byte[]{1,2,3,4,5,6});
        byte[] contents;
        
        contents = e1.getContents();
        contents[5] = 100;
        contents = e1.getContents();
        
        Assert.assertTrue(contents.length==6);
        Assert.assertTrue(contents[0] == 1);
        Assert.assertTrue(contents[1] == 2);
        Assert.assertTrue(contents[2] == 3);
        Assert.assertTrue(contents[3] == 4);
        Assert.assertTrue(contents[4] == 5);
        Assert.assertTrue(contents[5] == 6);
    }

    public void testOutputFormat() {
        NodeID e1 = new NodeID(new byte[]{1,0x10,0x13,0x0D,(byte)0xD0,(byte)0xAB});
        Assert.assertEquals("01.10.13.0D.D0.AB", e1.toString());
    }

   // from here down is testing infrastructure
    
    public NodeIDTest(String s) {
        super(s);
    }

    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {NodeIDTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(NodeIDTest.class);
        return suite;
    }
}
