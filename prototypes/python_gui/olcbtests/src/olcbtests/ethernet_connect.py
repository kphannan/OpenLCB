'''
Sends Ethernet connection

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

def Ethernet_Connect(self, ip_data):
    ip_address = self.builder.get_object("ip_address_entry").get_text()
    port_data =self.builder.get_object("port_entry_2").get_text()
    src_alias = self.builder.get_object("eth_src_alias_entry").get_text().strip()
    dst_alias = self.builder.get_object("eth_dst_alias_combo_box").get_active_text().strip()
    node_id = self.builder.get_object("eth_node_id_entry").get_text().strip()
    self.conn = communication.EthernetConnection(ip_address, int(port_data))
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
    logger.info('Received Node alias {0} and Node ID {1}'.format(msg.src_alias, msg.node_id))
    self.connect_status.set_text("Connected to Alias {0} via IP Address {1} Port {2} with Node ID {3}".format(
        msg.src_alias, ip_address, port_data, msg.node_id))
    self.ethernet_dialog.hide()
    self.Show_Connected_OK(ip_data)
    self.eth_disconnect.show_all()
    self.connected_no.hide()
    self.status_connect_button.hide()
    self.status_dst_alias_combo_box.hide()