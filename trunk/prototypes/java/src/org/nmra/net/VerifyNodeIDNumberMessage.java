package org.nmra.net;

/**
 * Verify Node ID Number message implementation
 *
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision: 37 $
 */
public class VerifyNodeIDNumberMessage extends Message {
    
    public VerifyNodeIDNumberMessage(NodeID source) {
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
        decoder.handleVerifyNodeIDNumber(this, sender);
     }

    public String toString() {
        return getSourceNodeID().toString()
                +" PVerify Node ID number";     
    }
}
