package org.nmra.net.implementations;

import org.nmra.net.*;

/**
 * Example of receiving a NMRAnet datagram.
 *<p>
 *
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision: 69 $
 */
public class DatagramReceiver extends MessageDecoder {
    public DatagramReceiver(NodeID here, NodeID far, Connection c) {
        this.here = here;
        this.far = far;
        this.connection = c;
    }
    
    NodeID here;
    NodeID far;
    Connection connection;

    /**
     * Handle "Datagram" message
     */
    public void handleDatagram(DatagramMessage msg, Connection sender){
        // accept
        Message m = new DatagramAcknowledgedMessage(here, far);
        connection.put(m, this);
    }

}
