'''
Sends serial connection

@author: Tim Hatch
'''
# Copyright 2013 Timothy C. Hatch
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
#from openlcb.can import messages

from openlcb.can import messages
from openlcb import communication
import logging

logger = logging.getLogger(__name__)

def Serial_Connect(self, w):
    connect_status = self.builder.get_object("connect_status")
    comport_active = self.builder.get_object("serial_entry_2").get_active()
    comport_model = self.builder.get_object("serial_entry_2").get_model()
    comport = comport_model[comport_active][0]
    baud = self.builder.get_object("baud_entry").get_active_text()
    src_alias = self.builder.get_object("src_alias_entry").get_text().strip()
    dst_alias = self.builder.get_object("dst_alias_combo_box").get_active_text().strip()
    node_id = self.builder.get_object("node_id_entry").get_text().strip()
    self.conn = communication.SerialConnection(comport, baud)
    msg = messages.VerifiedNodeAddressed(src_alias=int(src_alias, 16),
        dst_alias=int(dst_alias, 16))
    logger.info('Sending Verify Node Addressed message from {src} to {dst} '.format(
    src=msg.src_alias,
    dst=msg.dst_alias
    ))
    self.conn.connect()
    self.config = {'src_alias':int(src_alias, 16),
        'dst_alias':int(dst_alias, 16),'node_id':int(node_id, 16)}
    self.conn.send(msg)
    responses = self.conn.receive_multi()
    for response in responses:
        msg = messages.parse_frame(response)
    logger.info('Received Node alias {0} with Node ID {1}'.format(msg.src_alias, msg.node_id))
    connect_status.set_text("Connected to Alias {0} via Comport {1} with Node ID {2} ".format(
        msg.src_alias, comport, msg.node_id))

    self.Show_Connected_OK(w)
    self.close_connection.show_all()
    self.connected_no.hide()
    self.status_connect_button.hide()
    self.comport_dialog.hide()
    self.status_dst_alias_combo_box.hide()
    
def Serial_Discover_Nodes(self, w):
    comport_active = self.builder.get_object("serial_entry_2").get_active()
    comport_model = self.builder.get_object("serial_entry_2").get_model()
    comport = comport_model[comport_active][0]
    baud = self.builder.get_object("baud_entry").get_active_text()
    src_alias = self.builder.get_object("src_alias_entry").get_text().strip()
    conn = communication.SerialConnection(comport, baud)
    msg = messages.VerifyNodeIDNumberSimple(src_alias=int(src_alias, 16))
    logger.info('Sending VerifyNode message from {src}'.format(
    src=msg.src_alias
    ))
    with conn:
        #self.config = {'src_alias':int(src_alias, 16)}
        conn.send(msg)
        cbo = self.builder.get_object('dst_alias_combo_box')
        model = cbo.get_model()
        model.clear()
        responses = conn.receive_multi()
        logger.info('Received response from {0} node(s)'.format(len(responses)))
        for response in responses:
            response = messages.parse_frame(response)
            logger.info('Received {0} message from Node alias -- {1}'.format(
                response.__class__.__name__, response.src_alias))
            model.append([response.src_alias])
            
def Serial_Reconnection(self, w):
        comport_active = self.builder.get_object("serial_entry_2").get_active()
        comport_model = self.builder.get_object("serial_entry_2").get_model()
        comport = comport_model[comport_active][0]
        baud = self.builder.get_object("baud_entry").get_active_text()
        src_alias = self.builder.get_object("src_alias_entry").get_text().strip()
        conn = communication.SerialConnection(comport, baud)
        msg = messages.VerifyNodeIDNumberSimple(src_alias=int(src_alias, 16))
        logger.info('Sending VerifyNode message from {src}'.format(
        src=msg.src_alias
        ))
        with conn:
            conn.send(msg)
            status_cbo = self.builder.get_object('status_dst_alias_combo_box')
            model = status_cbo.get_model()
            model.clear()
            responses = conn.receive_multi()
            logger.info('Received response from {0} node(s)'.format(len(responses)))
            for response in responses:
                response = messages.parse_frame(response)
                logger.info('Received {0} message from Node alias -- {1}'.format(
                    response.__class__.__name__, response.src_alias))
                model.append([response.src_alias])
        self.status_connect_button.show_all()

    



