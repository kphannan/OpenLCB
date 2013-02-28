#!/olcbtest python
'''OpenLCB over CAN
Front end for python classes for creating, manipulating, and parsing CAN frames as OpenLCB
messages.

:author: Timothy C. Hatch
'''
# Copyright 2012 Timothy C. Hatch
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

from olcbtests import util
from openlcb import communication
from identify_events import identify_events_global
from identify_events import identify_events_addressed
from identify_consumers import identify_consumers_unknown
from identify_consumers import identify_consumers_valid
from identifyProducers import identify_producers
from identifyProducers import identify_producers_unknown
from testConsumerOutput import test_consumer_output
from nodeverification import verify_node_global
from nodeverificationaddressed import verify_node_addressed
from test_p_c_loopback import test_p_c_loopback
from testpcNotification import test_pc_notification
from resetDatagram import reset_datagram
from testAliasConflict import alias_conflict
from testStartup import test_start_up
from SNII import simple_node_ident_info
from protocolIdentProtocol import protocol_ident_protocol
from testDatagram import test_datagram
from unknownDatagramType import unknown_datagram_type
from event_id_reset import mfr_clear
from serial_connect import Serial_Connect
from serial_connect import Serial_Discover_Nodes
from serial_connect import Serial_Reconnection
from ethernet_connect import Ethernet_Connect

from serial_connect import Serial_Reconnection
import serial 
import threading
import logging
import gtk
#import time
#import sys
import pkg_resources
#import os
from openlcb.can import messages

root_logger = logging.getLogger()
logger = logging.getLogger()
logger = logging.getLogger(__name__)

class OpenLCB_tools_tests:

    def Show_Output_Dialog(self,w):
        self.output_box_dialog.show_all()
    def Hide_Output_Dialog(self, w):
        self.output_box_dialog.hide()

    def Show_Test_Failed(self, w):
        self.test_failed.show_all()
    def Hide_Test_Failed(self, w):
        self.test_failed.hide()

    def Error_Show(self, w):
        self.error.show_all()
    def Error_Close(self, w):
        self.error.hide()

    def Hide_Text_Box(self, w):
        self.output_box_viewport.hide()
        self.tch_logo.show_all()
    def Show_Text_Box(self, w):
        self.output_box_viewport.show_all()
        self.tch_logo.hide()

    def Show_consumer_out_test(self, w):
        for box in self.test_boxes:
            box.hide()
        self.test_con_out_box.show_all()
        self.test_pc_loop_box.hide()

    def Hide_consumer_out_test(self, w):
        self.test_con_out_box.hide()

    def Show_Test_pc_Loop(self, w):
        for box in self.test_boxes:
            box.hide()
        self.test_pc_loop_box.show_all()

    def Hide_Test_pc_Loop(self, w):
        self.test_pc_loop_box.hide()
        
    def Show_PC_Notify(self, w):
        for box in self.test_boxes:
            box.hide()
        self.test_pc_notify.show_all()

    def Hide_PC_Notify(self, w):
        self.test_pc_notify.hide()

    def Show_Datagram_Test(self, w):
        for box in self.test_boxes:
            box.hide()
        self.datagram_test_box.show_all()

    def Hide_Datagram_Test(self, w):
        self.datagram_test_box.hide()

    def show_startup(self, w):
        for box in self.test_boxes:
            box.hide()
        self.startup_box.show_all()

    def hide_startup(self, w):
        self.startup_box.hide()

    def Show_Unknown_Datagram(self, w):
        for box in self.test_boxes:
            box.hide()
        self.unknown_datagram_box.show_all()

    def Hide_Unknown_Datagram(self, w):
        self.unknown_datagram_box.hide()

    def show_alias_conflict(self, w):
        for box in self.test_boxes:
            box.hide()
        self.test_alias_conflict_box.show_all()
        
    def hide_alias_conflict(self, w):
        self.test_alias_conflict_box.hide()
        


    def Show_Connected_OK(self, w):
        self.connected_ok.show_all()
    def Hide_Connected_OK(self, w):
        self.connected_ok.hide()
        
    def Show_Serial_Error(self, w):
        self.error_serial_dialog.show_all()

    '''
    Connections for Serial and Ethernet
    '''

    def serial_ports(self):
        '''
        scan for available ports. return a list of tuples (num, name)
        '''
        serial_ports = self.builder.get_object("serial_entry_2")
        model = serial_ports.get_model()
        model.clear()
        for p in xrange(64):
            try:
                ports = serial.Serial(p)
                model.append([str(p+1)])
                ports.close() # explicit close
            except serial.SerialException:
                pass


    def baud_rates(self, w):
        baud_r = self.builder.get_object("baud_entry")
        model = baud_r.get_model()
        model.clear()
        items_to_add = [
            '230400',
            '333333',
            '500000'
        ]
        for item in items_to_add:
            model.append([item])
        #text = baud_r.child.get_text()


    def Find_Nodes(self, w):
        serial_discover_nodes=Serial_Discover_Nodes(self, w)

    def Serial_Connect(self, w):
        serial_connect=Serial_Connect(self, w)

    def Ethernet_connect(self, ip_data):
        ethernet_connect=Ethernet_Connect(self, ip_data)

    def Reconnect(self, w):
        serial_reconnect=Serial_Reconnection(self,w)



    def Discover_Nodes(self, ip_data):
        ip_address = self.builder.get_object("ip_address_entry").get_text()
        port_data = self.builder.get_object("port_entry_2").get_text()
        src_alias = self.builder.get_object("eth_src_alias_entry").get_text().strip()
        conn = communication.EthernetConnection(ip_address, int(port_data))
        msg = messages.VerifyNodeIDNumberSimple(src_alias=int(src_alias, 16))
        logger.info('Sending VerifyNode message from {src}'.format(
        src=msg.src_alias
        ))
        with conn:
            conn.send(msg)
            cbo_ = self.builder.get_object('eth_dst_alias_combo_box')
            model = cbo_.get_model()
            model.clear()
            responses = conn.receive_multi()
            logger.info('Received response from {0} node(s)'.format(len(responses)))
            for response in responses:
                response = messages.parse_frame(response)
                logger.info('Received {0} message from Node alias -- {1}'.format(
                    response.__class__.__name__, response.src_alias))
                model.append([response.src_alias])

    def serial_connection(self, w):
        self.serial_ports()
        self.baud_rates(w)
        self.comport_dialog.show_all()

    def hide_serial_connection(self, w):
        self.comport_dialog.hide()

    def Show_Ethernet_Connection(self, w):
        self.ethernet_dialog.show_all()
    def Hide_Ethernet_Connection(self, w):
        self.ethernet_dialog.hide()

    #def Show_Test_Passed(self, w):
       #self.test_passed.show_all()
    #def Hide_Test_Passed(self, w):
        #self.test_passed.hide()

    def Show_Save_Dialog(self, w):
        self.save_dialog.show_all()
    def Hide_Save_Dialog(self, w):
        self.save_dialog.hide()

    def connect(self, w):
        self.connection_dialog.show_all()
    def hide_connect_dialog(self,w):
        self.connection_dialog.hide()

    def Node_Under_Test(self, w):
        self.config['dst_alias'], self.config['dst_id'] = util.discover_node(self.conn, self.config['src_alias'])

    def close(self, w):

        logger.debug('Closing connection to serial port {port}'.format(
            port=self.conn.com_port
        ))
        self.conn.close()
        self.status_connect_button.show_all()
        self.connected_no.show_all()
        self.status_dst_alias_combo_box.show_all()
        self.Clear_Text(w)
        self.connect_status.set_text("Disconnected")
        self.connected_ok.hide()
        self.close_connection.hide()
        
    def close_eth(self, w):

        logger.debug('Closing connection to {hostname}:{port}'.format(
            hostname=self.conn.hostname,
            port=self.conn.port
        ))
        self.conn._socket.close()
        self.connected_no.show_all()
        self.Clear_Text(w)
        self.connect_status.set_text("Disconnected")
        self.connected_ok.hide()
        self.eth_disconnect.hide()


    '''
          Messages to Identify Events Global and Addressed
          Consumers and Producer Events Global and Addressed
          '''
    def Identify_Events_Global(self, w):
        #self.pbar.show_all()
        events_global = identify_events_global(self.conn, self.config)


    def Identify_Events_Addressed(self, wiget):
        events_addressed = identify_events_addressed(self.conn, self.config)

    def Identify_Consumers_Unknown(self, w):
        id_consumers = identify_consumers_unknown(self.conn, self.config)

    def Identify_Consumers_Valid(self, w):
        id_consumers = identify_consumers_valid(self.conn, self.config)

    def Identify_Producers(self, w):
        id_producers = identify_producers(self.conn, self.config)

    def Identify_Producers_Unknown(self, w):
        id_producers_unknown = identify_producers_unknown(self.conn, self.config)

    def Node_Global(self, w):
        ver_node_global = verify_node_global(self.conn, self.config)

    def Node_Addressed(self, w):
        ver_node_addressed = verify_node_addressed(self.conn, self.config)

    def MFR_Clear(self, w):
        mfr_clear_all = mfr_clear(self.conn, self.config)
    '''
    Tests
    '''
    def Test_p_c_loopback(self, w):
        loopback_test = test_p_c_loopback(self.conn, self.config)

    def Test_p_c_Notification(self, w):
        pc_notify_test = test_pc_notification(self.conn, self.config)
        #self.Show_Test_Passed(self)

    def Test_Consumer_Output(self, w):
        test_output = test_consumer_output(self.conn, self.config)

    def Alias_Conflict(self, w):
        test_alias_conflict = alias_conflict(self.conn, self.config)

    def Test_Startup(self, w):
        startup = test_start_up(self.conn, self.config)

    def Check_Simple_Node_Info(self, w):
        snii = simple_node_ident_info(self.conn, self.config)
        
    def Check_PIP(self, w):
        pip = protocol_ident_protocol(self.conn, self.config)

    def Reset_Datagram(self, w):
        send_reset = reset_datagram(self.conn, self.config)

    def Test_Datagram(self, w):
        send_test_datagram = test_datagram(self.conn, self.config)
        
    def Test_Unknown_Datagram_Type(self, w):
        test_unknow_datagram_type = unknown_datagram_type(self.conn, self.config)

    def Clear_Text_2(self, w):
        buf_2 = self.output_box_2.get_buffer()
        start_2 = buf_2.get_start_iter()
        end_2 = buf_2.get_end_iter()
        buf_2.delete(start_2, end_2)

    def Clear_Text(self, w):
        buf = self.output_box.get_buffer()
        start = buf.get_start_iter()
        end = buf.get_end_iter()
        buf.delete(start, end)

    def Save_Text_File(self, w):
        file_name =self.builder.get_object("file_name_entry_a").get_text()
        outfile = open(file_name, "w")
        buf = self.output_box.get_buffer()
        start = buf.get_start_iter()
        end = buf.get_end_iter()
        text = start.get_text(end)
        outfile.write(text)
        outfile.close()
        self.save_dialog.hide()
        buf.delete(start, end)
    
    def All_Test(self, w):
        ver_node_global = verify_node_global(self.conn, self.config)
        ver_node_addressed = verify_node_addressed(self.conn, self.config)
        events_global = identify_events_global(self.conn, self.config)
        events_addressed = identify_events_addressed(self.conn, self.config)
        id_consumers = identify_consumers_valid(self.conn, self.config)
        id_producers = identify_producers(self.conn, self.config)
        id_producers_unknown = identify_producers_unknown(self.conn, self.config)
        test_output = test_consumer_output(self.conn, self.config)
        loopback_test = test_p_c_loopback(self.conn, self.config)
        pc_notify_test = test_pc_notification(self.conn, self.config)
        snii = simple_node_ident_info(self.conn, self.config)
        pip = protocol_ident_protocol(self.conn, self.config)
        startup = test_start_up(self.conn, self.config)
        test_alias_conflict = alias_conflict(self.conn, self.config)

    def auto_scroll(self, w, event):
        adj = w.get_vadjustment()
        adj.set_value(adj.upper - adj.page_size)

    def gtk_main_quit(self, w):
        gtk.main_quit()


    def __init__(self):
        ui_def = pkg_resources.resource_string('olcbtests', 'main.ui')
        self.builder = gtk.Builder()
        self.builder.add_from_string(ui_def)
        self.builder.connect_signals(self)

        window = self.builder.get_object('window1')
        window.show_all()

        #test boxes
        self.test_boxes = []

        self.startup_box = self.builder.get_object("startup_box")
        self.startup_box.hide()
        self.test_boxes.append(self.startup_box)

        self.test_con_out_box = self.builder.get_object("test_con_out_box")
        self.test_con_out_box.hide()
        self.test_boxes.append(self.test_con_out_box)

        self.test_pc_loop_box = self.builder.get_object("test_pc_loop_box")
        self.test_pc_loop_box.hide()
        self.test_boxes.append(self.test_pc_loop_box)

        self.test_pc_notify = self.builder.get_object("test_pc_notify")
        self.test_pc_notify.hide()
        self.test_boxes.append(self.test_pc_notify)

        self.datagram_test_box = self.builder.get_object("datagram_test_box")
        self.datagram_test_box.hide()
        self.test_boxes.append(self.datagram_test_box)

        self.unknown_datagram_box =self.builder.get_object("unknown_datagram_box")
        self.unknown_datagram_box.hide()
        self.test_boxes.append(self.unknown_datagram_box)

        self.test_pc_loop_box = self.builder.get_object("test_pc_loop_box")
        self.test_pc_loop_box.hide()
        self.test_boxes.append(self.test_pc_loop_box)
        
        self.test_alias_conflict_box = self.builder.get_object("test_alias_conflict_box")
        self.test_alias_conflict_box.hide()
        self.test_boxes.append(self.test_alias_conflict_box)
        #end test boxes

        self.comport_dialog = self.builder.get_object("comport_dialog")
        self.comport_dialog.hide()

        self.error = self.builder.get_object("error_serial_dialog")
        self.error.hide()

        self.output_box_viewport = self.builder.get_object("output_box_viewport")
        self.output_box_viewport.show_all()



        self.ethernet_dialog = self.builder.get_object("ethernet_dialog")
        self.ethernet_dialog.hide()

        #self.test_passed = self.builder.get_object("test_passed")
        #self.test_passed.hide()

        self.tch_logo = self.builder.get_object("tch_logo")
        self.tch_logo.hide()

        self.output_box_dialog = self.builder.get_object("output_box_dialog")
        self.output_box_dialog.hide()

        self.save_dialog = self.builder.get_object("file_chooser_dialog")
        self.save_dialog.hide()

        self.test_failed = self.builder.get_object("test_failed_message")
        self.test_failed.hide()


        
        self.connected_ok = self.builder.get_object("connected_ok")
        self.connected_ok.hide()
        self.connected_no = self.builder.get_object("connected_no")
        self.connected_no.show_all()

        self.reconnect_button =self.builder.get_object("reconnect_button")
        self.reconnect_button.hide()
        
        self.status_connect_button = self.builder.get_object("status_connect_button")
        self.status_connect_button.hide()

        self.close_connection = self.builder.get_object("close_connection")
        self.close_connection.hide()
        
        self.eth_disconnect = self.builder.get_object("eth_disconnect")
        self.eth_disconnect.hide()
        
        self.status_dst_alias_combo_box = self.builder.get_object("status_dst_alias_combo_box")
        self.status_dst_alias_combo_box.hide()

        self.connect_status = self.builder.get_object("connect_status")
        self.connect_status.show_all()
        self.connect_status.set_text("Disconnected")
        
        self.error_serial_dialog = self.builder.get_object("error_serial_dialog")
        self.error_serial_dialog.hide()

        self.pbar = self.builder.get_object("progress_bar")
        self.pbar.pulse()
        self.pbar.hide()

        self.output_box = self.builder.get_object('output_box')
        self.output_box_2 = self.builder.get_object('output_box_2')
        output_handler = GuiHandler(self.output_box)
        output_handler_2 = GuiHandler(self.output_box_2)
        default_formatter = logging.Formatter(
            #'%(message)s'
            '[%(levelname)s] %(name)s (%(funcName)s): %(message)s'
            #'(%(funcName)s): %(message)s'
            #'[%(asctime)s] [%(levelname)s] %(name)s (%(funcName)s): %(message)s'
        )
        default_formatter_2 = logging.Formatter()
        output_handler.setLevel(logging.DEBUG)
        output_handler_2.setLevel(logging.DEBUG)
        output_handler.setFormatter(default_formatter)
        output_handler_2.setFormatter(default_formatter_2)
        root_logger.addHandler(output_handler)
        communications_logger = logging.getLogger("openlcb.communication")
        communications_logger.addHandler(output_handler_2)
        communications_logger.setLevel(logging.DEBUG)
        root_logger.setLevel(logging.DEBUG)

    def Output_Box_One():

        logging.error('starting')


    output_thread_1 = threading.Thread(target=Output_Box_One)

    output_thread_1.start()


class GuiHandler(logging.Handler):

    def __init__(self, textbox):
        super(GuiHandler, self).__init__()
        self.buf = textbox.get_buffer()
        colors = {
            logging.DEBUG: 'black',
            logging.INFO: 'blue',
            logging.WARNING: 'orange',
            logging.ERROR: 'red',
            logging.CRITICAL: 'red',
        }
        for level, color in colors.items():
            tag_name = 'loglevel_{0}'.format(level)
            self.buf.create_tag(tag_name, foreground=color)

    def emit(self, record):
        msg = self.format(record) + "\n"
        end = self.buf.get_end_iter()
        tag_name = 'loglevel_{0}'.format(record.levelno)
        self.buf.insert_with_tags_by_name(end, msg, tag_name)

def main():
    gtk.main()
    return 0

if __name__ == "__main__":
    OpenLCB_tools_tests()
    main()
