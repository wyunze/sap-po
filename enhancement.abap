/*----
cl_proxy_adapter_test=>inbound_test
add implicit Enhancement to the end of the method
----*/
  DATA(lv_new_msg_id) = zcl_po_utility=>get_msg_id( io_proxy = lr_server_context iv_interface_direction = if_proxy_framework=>co_interface_direction_in ).
  EXPORT new_msg_id FROM lv_new_msg_id TO MEMORY ID 'NEW_PO_MSG_ID'.
  
/*----
cl_proxy_adapter_test=>outbound_test
add implicit Enhancement to the end of the method
----*/
  DATA(lv_new_msg_id) = zcl_po_utility=>get_msg_id( io_proxy = lr_proxy iv_interface_direction = if_proxy_framework=>co_interface_direction_out ).
  EXPORT new_msg_id FROM lv_new_msg_id TO MEMORY ID 'NEW_PO_MSG_ID'.
