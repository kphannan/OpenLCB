package org.openlcb;

// For annotations
import net.jcip.annotations.*; 
import edu.umd.cs.findbugs.annotations.*; 

/**
 * Identify Consumers message implementation
 *
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision$
 */
@Immutable
@ThreadSafe
public class IdentifyConsumersMessage extends Message {
    
    public IdentifyConsumersMessage(NodeID source, EventID event) {
        super(source);
        this.eventID = event;
    }
    
    EventID eventID = null;
    public EventID getEventID() { return eventID; }
    
    /**
     * Implement message-type-specific
     * processing when this message
     * is received by a node.
     *<p>
     * Default is to do nothing.
     */
     @Override
     public void applyTo(MessageDecoder decoder, Connection sender) {
        decoder.handleIdentifyConsumers(this, sender);
     }

     /**
      * To be equal, messages have to have the
      * same type and content
      */
     @Override
     public boolean equals(Object o) {
        if (!super.equals(o)) return false; // also checks type
        IdentifyConsumersMessage msg = (IdentifyConsumersMessage) o;
        if (! this.eventID.equals(msg.eventID))
            return false;
        return true;
     }

    public String toString() {
        return super.toString()
                +" Identify Consumers with "+eventID.toString();     
    }

    public int getMTI() { return MTI_IDENTIFY_CONSUMERS; }
}
