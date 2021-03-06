package org.openlcb.implementations;

import org.openlcb.*;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision$
 */
public class StreamTransmitterTest extends TestCase {
    
    NodeID hereID = new NodeID(new byte[]{1,2,3,4,5,6});
    NodeID farID  = new NodeID(new byte[]{1,1,1,1,1,1});
    
    byte[] data;

    java.util.ArrayList<Message> messagesReceived;
    
    public void testInitialization() {
        messagesReceived = new java.util.ArrayList<Message>();
        Connection testConnection = new AbstractConnection(){
            public void put(Message msg, Connection sender) {
                messagesReceived.add(msg);
            }
        };
        StreamTransmitter xmt = new StreamTransmitter(
                                            hereID,farID,
                                            64, data,
                                            testConnection);
                                                    
        Assert.assertTrue(messagesReceived.size() == 1); // startup message
        Assert.assertEquals(new StreamInitiateRequestMessage(hereID, farID, 64, (byte)4, (byte)0), messagesReceived.get(0));
    }
    
    public void testShortStream() {
        messagesReceived = new java.util.ArrayList<Message>();
        Connection testConnection = new AbstractConnection(){
            public void put(Message msg, Connection sender) {
                messagesReceived.add(msg);
            }
        };
        
        data = new byte[256];
        
        StreamTransmitter xmt = new StreamTransmitter(
                                            hereID,farID,
                                            256, data,
                                            testConnection);
                                                    
        Assert.assertEquals("init messages", 1, messagesReceived.size());
        Assert.assertTrue(messagesReceived.get(0)
                           .equals(new StreamInitiateRequestMessage(hereID, farID, 256, (byte)4, (byte)0)));
                           
        // OK 256 byte buffers
        Message m = new StreamInitiateReplyMessage(farID, hereID, 256, (byte)0, (byte)0);
        messagesReceived = new java.util.ArrayList<Message>();

        xmt.put(m, null);

        Assert.assertEquals("1st messages", 2, messagesReceived.size());
        Assert.assertTrue(messagesReceived.get(0)
                           .equals(new StreamDataSendMessage(hereID, farID, data, (byte)0)));
        Assert.assertTrue(messagesReceived.get(1)
                           .equals(new StreamDataCompleteMessage(hereID, farID, (byte)0, (byte)0)));
    }
    
    public void testTwoMsgStream() {
        messagesReceived = new java.util.ArrayList<Message>();
        Connection testConnection = new AbstractConnection(){
            public void put(Message msg, Connection sender) {
                messagesReceived.add(msg);
            }
        };
        
        data = new byte[512];
        
        StreamTransmitter xmt = new StreamTransmitter(
                                            hereID,farID,
                                            256, data,
                                            testConnection);
                                                    
        Assert.assertEquals("init messages", 1, messagesReceived.size());
        Assert.assertTrue(messagesReceived.get(0)
                           .equals(new StreamInitiateRequestMessage(hereID, farID, 256, (byte)4, (byte)0)));
                           
        // OK 256 byte buffers
        Message m = new StreamInitiateReplyMessage(farID, hereID, 256, (byte)0, (byte)0);
        messagesReceived = new java.util.ArrayList<Message>();

        xmt.put(m, null);

        // should get a data message
        Assert.assertEquals("1st messages", 1, messagesReceived.size());
        Assert.assertTrue(messagesReceived.get(0)
                           .equals(new StreamDataSendMessage(hereID, farID, new byte[256], (byte)0)));

        // reply to proceed
        m = new StreamDataProceedMessage(farID, hereID, (byte)0, (byte)0);
        messagesReceived = new java.util.ArrayList<Message>();

        xmt.put(m, null);

        // 2nd message should be followed by a Stream Data Complete message
        Assert.assertEquals("2nd messages", 2, messagesReceived.size());
        Assert.assertTrue(messagesReceived.get(0)
                           .equals(new StreamDataSendMessage(hereID, farID, new byte[256], (byte)0)));
        Assert.assertTrue(messagesReceived.get(1)
                           .equals(new StreamDataCompleteMessage(hereID, farID, (byte)0, (byte)0)));

    }
    
    // from here down is testing infrastructure
    
    public StreamTransmitterTest(String s) {
        super(s);
    }

    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {StreamTransmitterTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(StreamTransmitterTest.class);
        return suite;
    }
}
