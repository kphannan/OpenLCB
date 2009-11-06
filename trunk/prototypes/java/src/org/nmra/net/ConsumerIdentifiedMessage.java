package org.nmra.net;

/**
 * Consumer Identified message implementation
 *
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision: 36 $
 */
public class ConsumerIdentifiedMessage extends Message {
    
    public ConsumerIdentifiedMessage(NodeID source, EventID eventID) {
        super(source);
        if (eventID == null)
            throw new IllegalArgumentException("EventID cannot be null");
        this.eventID = eventID;
    }
        
    EventID eventID;

    // because EventID is immutable, can directly return object
    public EventID getEventID() {
        return eventID;
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
        decoder.handleConsumerIdentified(this, sender);
     }

    public boolean equals(Object o) {
        if (!super.equals(o)) return false;
        ConsumerIdentifiedMessage p = (ConsumerIdentifiedMessage) o;
        return eventID.equals(p.eventID);
    } 
    public String toString() {
        return getSourceNodeID().toString()
                +" Consumer identified for "+eventID.toString();     
    }
}
