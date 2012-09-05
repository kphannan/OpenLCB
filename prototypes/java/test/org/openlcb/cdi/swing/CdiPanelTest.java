package org.openlcb.cdi.swing;

import javax.swing.JFrame;
import org.openlcb.*;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import javax.swing.*;

/**
 * @author  Bob Jacobsen   Copyright 2012
 * @version $Revision: 2175 $
 */
public class CdiPanelTest extends TestCase {
    
    // from here down is testing infrastructure
    
    public CdiPanelTest(String s) {
        super(s);
    }

    public void testDisplay() {
        JFrame f = new JFrame();
        f.setTitle("Configuration Demonstration");
        CdiPanel m = new CdiPanel();
                
        m.initComponents(new CdiPanel.ReadWriteAccess(){
            @Override
            public void doWrite(long address, int space, byte[] data) {
                    System.out.println(data.length);
                    System.out.println("write "+address+" "+space+": "+org.openlcb.Utilities.toHexDotsString(data));
                }
            @Override
            public void doRead(long address, int space, int length, CdiPanel.ReadReturn handler) {
                    handler.returnData(new byte[]{1,2,3,4,5,6,7,8});
                    System.out.println("read "+address+" "+space);
                }            
        },
        new CdiPanel.GuiItemFactory() {
            public JButton handleReadButton(JButton button) {
                System.out.println("process button");
                button.setBorder(BorderFactory.createLineBorder(java.awt.Color.yellow));
                return button;
        }
    }
);
        m.loadCDI(
            new org.openlcb.cdi.jdom.JdomCdiRep(
                org.openlcb.cdi.jdom.SampleFactory.getBasicSample()
            )
        );
        
        f.add( m );

        // show
        f.pack();
        f.setVisible(true);        
    }
    
    // Main entry point
    static public void main(String[] args) {
        String[] testCaseName = {CdiPanelTest.class.getName()};
        junit.swingui.TestRunner.main(testCaseName);
    }

    // test suite from all defined tests
    public static Test suite() {
        TestSuite suite = new TestSuite(CdiPanelTest.class);
        return suite;
    }
}
