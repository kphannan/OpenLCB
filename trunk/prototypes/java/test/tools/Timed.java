package tools;

/**
 * The Timed interface allows an object to take
 * part in a Timer's discreet-time simulation
 *
 * @author  Bob Jacobsen   Copyright 2009
 * @version $Revision: 23 $
 */ 
public interface Timed {

    /**
     * Take actions at the next discrete time
     */
    public void tick(long time);
}
