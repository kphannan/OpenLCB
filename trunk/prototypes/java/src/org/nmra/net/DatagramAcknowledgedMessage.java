package org.nmra.net;

/**
 * Datagram Acknowledged message implementation
 *
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision: 69 $
 */
public class DatagramAcknowledgedMessage extends Message {
    
    public DatagramAcknowledgedMessage(NodeID source, NodeID dest) {
        super(source);
    }
        
    /**
     * Implement message-type-specific
     * processing when this message
     * is received by a node.
     *<p>
     * Default is to do nothing.
     */
     @Override
     public void applyTo(MessageDecoder decoder, Connection sender) {
        decoder.handleDatagramAcknowledged(this, sender);
     }
}
