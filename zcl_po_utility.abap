*@startuml
*scale 0.60
*class ZRRE_CL_PO_UTILITY {
*~ADMIN_CHECK( )
*+{static}DETERMINE_IFR_OPERATION( )
*+{static}DETERMINE_MT_BY_MP( )
*+{static}GET_MSG_ID( )
*+{static}GET_PAYLOAD( )
*+{static}QUERY_ACK_STATUS( )
*+{static}REQUEST_OUTBOUND_ACK( )
*+{static}RESTART( )
*}
*ZRRE_CL_PO_UTILITY --> ZRRE_IF_XML_ASSERTION
*interface ZRRE_IF_XML_ASSERTION {
*+ASSERT_BY_CONTAIN_TEXT( )
*+ASSERT_BY_TAG_COUNT( )
*+ASSERT_BY_TAG_VALUE( )
*+ASSERT_BY_XPATH_COUNT( )
*+ASSERT_BY_XPATH_VALUE( )
*}
*hide <<FUGR>> circle
*@enduml
CLASS zrre_cl_po_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.

    INTERFACES: zrre_if_xml_assertion.
    ALIASES:
      assert_by_contain_text FOR zrre_if_xml_assertion~assert_by_contain_text,
      assert_by_tag_value FOR zrre_if_xml_assertion~assert_by_tag_value,
      assert_by_tag_count FOR zrre_if_xml_assertion~assert_by_tag_count,
      assert_by_xpath_value FOR zrre_if_xml_assertion~assert_by_xpath_value,
      assert_by_xpath_count FOR zrre_if_xml_assertion~assert_by_xpath_count.

    TYPES:
         BEGIN OF ty_s_ori_msg_header.
    TYPES:
      abap_class_name  TYPE seoclsname,
      abap_method_name TYPE seosconame,
      prx_si_name      TYPE rm_oifname,
      prx_si_namespace TYPE rm_oifns,
      prx_si_operation TYPE sxi_operation.
      INCLUDE TYPE sxmsmsglst.
      TYPES: END OF ty_s_ori_msg_header.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine original message header</p>
      "!
      "! @parameter iv_msg_id | <p class="shorttext synchronized" lang="en">message ID</p>
      "! @parameter iv_ori_req_payload | <p class="shorttext synchronized" lang="en">original request payload</p>
      "! @parameter rs_msg_header | <p class="shorttext synchronized" lang="en">message header</p>
      determine_ori_msg_header
        IMPORTING
                  iv_msg_id            TYPE sxmsmguid
                  iv_ori_req_payload   TYPE bnk_com_msg_payld OPTIONAL
        RETURNING VALUE(rs_msg_header) TYPE ty_s_ori_msg_header,
      "! <p class="shorttext synchronized" lang="en">request outbound acknowledgement</p>
      "!
      "! @parameter io_ob_asyn_proxy | <p class="shorttext synchronized" lang="en">outbound asynchronous proxy</p>
      "! @parameter is_ack_request | <p class="shorttext synchronized" lang="en">ack request detail</p>
      request_outbound_ack
        IMPORTING
          io_ob_asyn_proxy TYPE REF TO cl_proxy_client
          is_ack_request   TYPE prx_ack_request_details DEFAULT if_wsprotocol_async_messaging=>co_transport_acknowledgment,
      "! <p class="shorttext synchronized" lang="en">query ack status</p>
      "!
      "! @parameter iv_msg_id | <p class="shorttext synchronized" lang="en">message id</p>
      "! @parameter es_ack_simple | <p class="shorttext synchronized" lang="en">ack simple</p>
      "! @parameter et_ack_detail | <p class="shorttext synchronized" lang="en">ack detail list</p>
      query_ack_status
        IMPORTING
          iv_msg_id     TYPE sxmsmguid
        EXPORTING
          es_ack_simple TYPE prx_ack_status
          et_ack_detail TYPE prx_ack_status_detail_table,
      "! <p class="shorttext synchronized" lang="en">get message id</p>
      "!
      "! @parameter iv_interface_direction | <p class="shorttext synchronized" lang="en">Interface direction</p>
      "! @parameter io_proxy | <p class="shorttext synchronized" lang="en">OB proxy instance/IB server context</p>
      "! @parameter rv_msg_id | <p class="shorttext synchronized" lang="en">message id</p>
      get_msg_id
        IMPORTING
                  iv_interface_direction TYPE string
                  io_proxy               TYPE REF TO object
        RETURNING VALUE(rv_msg_id)       TYPE sxmsmguid,
      "! <p class="shorttext synchronized" lang="en">determine message type by main payload</p>
      "!
      "! @parameter iv_xstr_payload | <p class="shorttext synchronized" lang="en">payload in xstring</p>
      "! @parameter ev_msg_name | <p class="shorttext synchronized" lang="en">message type name</p>
      "! @parameter ev_msg_namespace | <p class="shorttext synchronized" lang="en">message type namespace</p>
      determine_mt_by_mp
        IMPORTING
          iv_xstr_payload  TYPE xstring
        EXPORTING
          ev_msg_name      TYPE prx_intfid
          ev_msg_namespace TYPE prx_nspce,
      "! <p class="shorttext synchronized" lang="en">determine interface operation(mainly for multi operation)</p>
      "!
      "! @parameter iv_pid_ext | <p class="shorttext synchronized" lang="en">external PID(SENDER/RECEIVER)</p>
      "! @parameter iv_proxy_ns | <p class="shorttext synchronized" lang="en">proxy namespace</p>
      "! @parameter iv_proxy_name | <p class="shorttext synchronized" lang="en">proxy name</p>
      "! @parameter iv_req_msg_name | <p class="shorttext synchronized" lang="en">Request MT name(Not response)</p>
      "! @parameter iv_req_msg_namespace | <p class="shorttext synchronized" lang="en">Request MT namespace</p>
      "! @parameter ev_proxy_method | <p class="shorttext synchronized" lang="en">proxy class method</p>
      "! @parameter ev_operation_name | <p class="shorttext synchronized" lang="en">proxy operation name</p>
      determine_ifr_operation
        IMPORTING
          iv_pid_ext           TYPE sxmspipe-pidext
          iv_proxy_ns          TYPE rm_iifns
          iv_proxy_name        TYPE rm_iifname
          iv_req_msg_name      TYPE prx_intfid
          iv_req_msg_namespace TYPE prx_nspce
        EXPORTING
          ev_proxy_method      TYPE seosconame
          ev_operation_name    TYPE prx_ifrnam,
      "! <p class="shorttext synchronized" lang="en">Get payload of message</p>
      "!
      "! @parameter iv_msgid | <p class="shorttext synchronized" lang="en">message ID</p>
      "! @parameter ev_payload | <p class="shorttext synchronized" lang="en">payload in string</p>
      "! @parameter ev_xpayload | <p class="shorttext synchronized" lang="en">payload in xstring</p>
      get_payload
        IMPORTING
          iv_msgid    TYPE sxmsmguid
        EXPORTING
          ev_payload  TYPE string
          ev_xpayload TYPE bnk_com_msg_payld,
      "! <p class="shorttext synchronized" lang="en">Restart message</p>
      "!
      "! @parameter iv_msg_id | <p class="shorttext synchronized" lang="en">message ID</p>
      "! @parameter is_input | <p class="shorttext synchronized" lang="en">restart input control</p>
      "! @parameter iv_runtime | <p class="shorttext synchronized" lang="en">runtime</p>
      "! @parameter rs_output | <p class="shorttext synchronized" lang="en">restart output</p>
      restart
        IMPORTING
                  iv_msg_id        TYPE sxmsmguid
                  is_input         TYPE zrre_s_po_restart_input OPTIONAL
                  iv_runtime       TYPE string DEFAULT if_proxy_framework=>runtime_xi
        RETURNING VALUE(rs_output) TYPE zrre_s_po_restart_output.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">administration check</p>
      "!
      "! @parameter p_action | <p class="shorttext synchronized" lang="en">action</p>
      "! @parameter p_master | <p class="shorttext synchronized" lang="en">master info</p>
      "! @parameter p_allowed | <p class="shorttext synchronized" lang="en">if allowed</p>
      admin_check IMPORTING p_action  TYPE c
                            p_master  TYPE sxmsmsgdsp
                  EXPORTING p_allowed TYPE c.
ENDCLASS.



CLASS zrre_cl_po_utility IMPLEMENTATION.


  METHOD admin_check.
    DATA: ls_address TYPE ssximoni_address.
    DATA: lv_allowed  TYPE sxmsflag,
          lv_activity TYPE activ_auth.

    IF p_action = 'C'.
      lv_activity = 'A3'.
    ELSEIF p_action = 'R'.
      lv_activity = '16'.
    ENDIF.

* first, test inbound permission
    ls_address-party = p_master-ib_party.
    ls_address-party_type = p_master-ib_party_type.
    ls_address-party_agency = p_master-ib_party_agency.
    ls_address-service = p_master-ib_system.
    ls_address-interface = p_master-ib_name.
    ls_address-interface_namespace = p_master-ib_ns.

    CLEAR p_allowed.

    CALL FUNCTION 'SXMB_MONI_CHECK_AUTHORITY'
      EXPORTING
        im_activity      = lv_activity
        im_address       = ls_address
      IMPORTING
        ex_authority     = lv_allowed
      EXCEPTIONS
        invalid_activity = 1
        OTHERS           = 2.
    IF sy-subrc <> 0 OR lv_allowed = if_xms_main_const=>co_false.
      p_allowed = space.
    ELSE.
      p_allowed = 'X'.
    ENDIF.

* if access is forbidden for inbound and outbound values are set, test outbound permission
    IF p_allowed IS INITIAL AND
    ( p_master-ob_party IS NOT INITIAL OR
      p_master-ob_party_type IS NOT INITIAL OR
      p_master-ob_party_agency IS NOT INITIAL OR
      p_master-ob_system IS NOT INITIAL OR
      p_master-ob_name IS NOT INITIAL OR
      p_master-ob_ns IS NOT INITIAL ).

      ls_address-party = p_master-ob_party.
      ls_address-party_type = p_master-ob_party_type.
      ls_address-party_agency = p_master-ob_party_agency.
      ls_address-service = p_master-ob_system.
      ls_address-interface = p_master-ob_name.
      ls_address-interface_namespace = p_master-ob_ns.

      CALL FUNCTION 'SXMB_MONI_CHECK_AUTHORITY'
        EXPORTING
          im_activity      = lv_activity
          im_address       = ls_address
        IMPORTING
          ex_authority     = lv_allowed
        EXCEPTIONS
          invalid_activity = 1
          OTHERS           = 2.
      IF sy-subrc <> 0 OR lv_allowed = if_xms_main_const=>co_false.
        p_allowed = space.
      ELSE.
        p_allowed = 'X'.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    " auth_check


  METHOD determine_ifr_operation.
*---mapping proxy to ABAP name
    SELECT SINGLE FROM sproxhdr
      FIELDS object,obj_name
      WHERE ifr_name = @iv_proxy_name
      AND   ifr_nspce = @iv_proxy_ns
      AND   inactive = @abap_false
      INTO @DATA(ls_sproxhdr).
    CHECK sy-subrc EQ 0.

    "CL_PROXY_QUERY/CL_PXN_QUERY to find operation
    DATA(lv_param) = SWITCH seocmpname( iv_pid_ext
          WHEN cl_xms_main=>co_pipeline_sender THEN 'OUTPUT'"OUTBOUND
          WHEN cl_xms_main=>co_pipeline_receiver THEN 'INPUT'  ). "INBOUND

    SELECT SINGLE FROM sproxsig
      FIELDS obj_name1
      WHERE object   = @ls_sproxhdr-object
      AND   obj_name = @ls_sproxhdr-obj_name
      AND   object1  = 'METH'
      AND   param    = @lv_param
      AND   msg_name = @iv_req_msg_name
      AND   msg_namespace = @iv_req_msg_namespace
      INTO  @ev_proxy_method.
    IF sy-subrc NE 0.
      CLEAR ev_proxy_method.

      "CL_PROXY_QUERY to find operation
    ELSE.
      SELECT SINGLE FROM sproxdat
         FIELDS ifr_name
         WHERE object   = @ls_sproxhdr-object
         AND   obj_name = @ls_sproxhdr-obj_name
         AND   object1  = 'METH'
         AND   obj_name1 = @ev_proxy_method
         AND   object2 = 'METH'
         AND   obj_name2 = @ev_proxy_method
         INTO @ev_operation_name.
      IF sy-subrc NE 0.
        CLEAR ev_operation_name.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD determine_mt_by_mp.
    TRY.
        DATA(lo_document) = cl_rsdd_cs_xml_reader=>create_document( iv_xstr_payload ).
      CATCH cx_rsdd_cs_xml_invalid.
        RETURN.
    ENDTRY.

    DATA(lo_root_element) = lo_document->get_root_element( ).

    ev_msg_name = lo_root_element->get_name( ).
    ev_msg_namespace = lo_root_element->get_namespace_uri( ).
  ENDMETHOD.


  METHOD get_msg_id.
* get message id of sent message
    "cl_proxy_access
    DATA(lv_method) = |{ SWITCH #(
    iv_interface_direction
    WHEN if_proxy_framework=>co_interface_direction_in
    THEN 'IF_WS_SERVER_CONTEXT'
    WHEN if_proxy_framework=>co_interface_direction_out
    THEN 'IF_PROXY_BASIS'
    ) }~GET_PROTOCOL|.

    DATA lo_msg_id_protocol TYPE REF TO  if_wsprotocol.

    TRY.
        CALL METHOD io_proxy->(lv_method)
          EXPORTING
            protocol_name = if_wsprotocol=>message_id
          RECEIVING
            protocol      = lo_msg_id_protocol.
      CATCH cx_ai_system_fault.
        RETURN.
    ENDTRY.

    rv_msg_id = CAST if_wsprotocol_message_id( lo_msg_id_protocol )->get_message_id( ).
  ENDMETHOD.


  METHOD get_payload.
    "CL_XMS_PERSIST_ADM
    TRY.
        DATA(lt_pid) = cl_xms_persist_helper=>read_pid_via_msgguid( iv_msgid ).

        cl_xms_persist_adm=>get_xi_payload(
        EXPORTING im_msgguid = iv_msgid im_pid = lt_pid[ 1 ]-pid im_version = 0
        IMPORTING ex_messagepayload = DATA(lt_payload) ).
      CATCH cx_root INTO DATA(lo_error).
        ev_payload = lo_error->get_text( ).
        RETURN.
    ENDTRY.

    ev_xpayload = lt_payload[ 1 ]-payload.
    ev_payload = cl_soap_xml_helper=>xstring_to_string( ev_xpayload ).
  ENDMETHOD.


  METHOD query_ack_status.
* read ack
    TRY.
        DATA(lo_ack) = cl_proxy_access=>get_acknowledgment( iv_msg_id ).
        es_ack_simple = lo_ack->get_status( ).
        lo_ack->get_status_detail( IMPORTING ack_status_detail_table =  et_ack_detail ).
      CATCH cx_ai_system_fault.
    ENDTRY.
  ENDMETHOD.


  METHOD request_outbound_ack.
* get protocol for asynchronous messaging
    TRY.
        DATA(lo_async_messaging) = CAST if_wsprotocol_async_messaging(
         io_ob_asyn_proxy->get_protocol( if_wsprotocol=>async_messaging ) ).

* Ask for transport acknowledgment
        lo_async_messaging->set_acknowledgment_requested( is_ack_request ).
      CATCH cx_ai_system_fault.
    ENDTRY.
  ENDMETHOD.


  METHOD restart.
    "read original message header
    DATA(ls_org_msg_header) = determine_ori_msg_header( iv_msg_id ).

    "read payload
    get_payload( EXPORTING iv_msgid = ls_org_msg_header-msgguid IMPORTING ev_payload = DATA(lv_req_payload) ev_xpayload = DATA(lv_req_xpayload) ).

    "set success
    rs_output-technical_status = /bobf/cm_frw=>co_severity_success.

*----restart begin
    TRY.
        CASE ls_org_msg_header-pid.
          WHEN cl_xms_main=>co_pipeline_receiver."INBOUND
            cl_proxy_adapter_test=>inbound_test(
               EXPORTING
                 interface             = ls_org_msg_header-abap_class_name
                 method                = ls_org_msg_header-abap_method_name
                 request_data          = lv_req_xpayload
                 runtime               = iv_runtime
                 dont_catch_appl_fault = is_input-dont_catch_appl_fault
               IMPORTING
                 response_data         = DATA(lv_resp_xpayload)
                 error_data            = DATA(lv_error_data)
                 exception_class_name  = DATA(lv_exception_class_name)
                 runtime_deserialize   = DATA(lv_runtime_deserialize)
                 runtime_execute       = DATA(lv_runtime_execute)
                 runtime_serialize     = DATA(lv_runtime_serialize) ).

          WHEN cl_xms_main=>co_pipeline_sender."OUTBOUND
            cl_proxy_adapter_test=>outbound_test(
              EXPORTING
                class_name            = ls_org_msg_header-abap_class_name
                method_name           = ls_org_msg_header-abap_method_name
                logical_port_name     = space
                request_data          = lv_req_xpayload
                extended_xml_handling = space
              IMPORTING
                org_request_data      = DATA(lv_org_req_data)
                response_data         = DATA(lv_response_data)
                org_response_data     = lv_resp_xpayload
                error_data            = lv_error_data
                exception_class_name  = lv_exception_class_name ).

        ENDCASE.

      CATCH cx_root INTO DATA(lo_error).
        rs_output-technical_status = /bobf/cm_frw=>co_severity_error.
        rs_output-technical_message = lo_error->get_text( ).
        RETURN.
    ENDTRY.

    IMPORT new_msg_id TO rs_output-new_req_message_id FROM MEMORY ID 'NEW_PO_MSG_ID'.
    FREE MEMORY ID 'NEW_PO_MSG_ID'.

    IF is_input-return_message_details EQ abap_true.
      rs_output-message_details = CORRESPONDING #( ls_org_msg_header ).
    ENDIF.

    IF is_input-update_to_parentmsg EQ abap_true.
      IF rs_output-new_req_message_id IS NOT INITIAL.
        "update new message id's parent message id
        "get the names of active & inactive container and status of switch
        cl_xms_persist=>get_current_table_container( IMPORTING ex_master = DATA(lv_master) ).
        UPDATE (lv_master) SET parentmsg = @ls_org_msg_header-msgguid
                           WHERE msgguid = @rs_output-new_req_message_id
                           AND   pid = @ls_org_msg_header-pid.
      ENDIF.
    ENDIF.

    IF is_input-return_req_new_msgid EQ abap_false.
      CLEAR rs_output-new_req_message_id.
    ENDIF.

    IF lv_exception_class_name IS NOT INITIAL.
      rs_output-technical_status =  /bobf/cm_frw=>co_severity_error.
      rs_output-technical_message = cl_soap_xml_helper=>xstring_to_string( lv_error_data ).
      RETURN.
    ENDIF.

    IF ls_org_msg_header-msgtype = 'A'.
      "triger commit
      IF is_input-commit_asynchronous EQ abap_true.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CHECK ls_org_msg_header-msgtype = 'S'."only synchronous has response

    rs_output-response_in_xml = COND #( WHEN lv_exception_class_name IS NOT INITIAL THEN lv_error_data ELSE lv_resp_xpayload ).

    IF is_input-return_resp_in_abap EQ abap_true.
      "get signature
      cl_proxy_signature=>get_signature( EXPORTING object = 'CLAS' obj_name = ls_org_msg_header-abap_class_name IMPORTING tab = DATA(lt_signature) ).

      CASE ls_org_msg_header-pid.
        WHEN cl_xms_main=>co_pipeline_sender."OUTBOUND
          DATA(lv_resp_abap_type) = lt_signature[ object1 = 'METH' param = 'INPUT' ]-typename.
        WHEN cl_xms_main=>co_pipeline_receiver."INBOUND
          lv_resp_abap_type = lt_signature[ object1 = 'METH' param = 'OUTPUT' ]-typename.
      ENDCASE.

      DATA(lo_xml_reader) = cl_sxml_string_reader=>create( rs_output-response_in_xml ).
      DATA lr_abap_struct TYPE REF TO data.

      CREATE DATA lr_abap_struct TYPE (lv_resp_abap_type).
      ASSIGN lr_abap_struct->* TO FIELD-SYMBOL(<ls_abap_data>).
      CALL TRANSFORMATION sai_proxy_rtuntime_struct SOURCE XML lo_xml_reader RESULT struct = <ls_abap_data>.

      IF is_input-display_resp_in_gui EQ abap_true.
        cl_demo_output=>display( <ls_abap_data> ).
        RETURN.
      ENDIF.
    ENDIF.

    IF is_input-display_resp_in_gui EQ abap_true.
      cl_abap_browser=>show_xml( dialog = abap_false xml_xstring = rs_output-response_in_xml size = cl_abap_browser=>large title = 'Message Payload' ).
*    cl_soap_xml_helper=>show_data_by_browser( ev_payload ).
*    cl_demo_output=>display_xml( ev_payload ).
*    cl_proxy_ui_utils=>show_xml( rs_output-response_in_xml ).
    ENDIF.

    IF is_input-return_resp_in_xml EQ abap_false.
      CLEAR rs_output-response_in_xml.
    ENDIF.
    "PACKAGE SAI_PROXY_RUNTIME_COMMON->CL_PROXY_RUNTIME_COMMON_UTILS
    "SPROX_SERVICE_INTERFACE_TEST_O/SPROX_SERVICE_INTERFACE_TEST_I
  ENDMETHOD.

  METHOD assert_by_contain_text.
    DATA(lv_payload) = cl_soap_xml_helper=>xstring_to_string( iv_xml_xpayload ).
    SEARCH iv_contain_text FOR lv_payload.
    IF sy-subrc EQ 0.
      rv_succeed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_by_tag_value.
    TRY.
        DATA(lo_document) = cl_rsdd_cs_xml_reader=>create_document( iv_xml_xpayload ).

        DATA(lo_lead_elem) = lo_document->find_from_name_ns( name = iv_tag_name uri = lo_document->get_namespace_uri( ) ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    CHECK lo_lead_elem IS BOUND.
    IF lo_lead_elem->get_value( ) EQ iv_elem_value.
      rv_succeed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_by_xpath_value.
    TRY.
        DATA(lo_document) = cl_rsdd_cs_xml_reader=>create_document( iv_xml_xpayload ).
      CATCH cx_rsdd_cs_xml_invalid.
        RETURN.
    ENDTRY.

    DATA(lo_elem) = lo_document->find_from_path_ns( path = iv_xml_xpath default_uri = lo_document->get_namespace_uri( ) ).

    CHECK lo_elem IS BOUND.
    IF lo_elem->get_value( ) EQ iv_elem_value.
      rv_succeed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_by_tag_count.
    TRY.
        DATA(lo_document) = cl_rsdd_cs_xml_reader=>create_document( iv_xml_xpayload ).
      CATCH cx_rsdd_cs_xml_invalid.
        RETURN.
    ENDTRY.

    DATA(lo_elem_collection) = lo_document->get_elements_by_tag_name_ns( name = iv_tag_name uri = lo_document->get_namespace_uri( ) ).

    CHECK lo_elem_collection IS BOUND.
    IF lo_elem_collection->get_length( ) EQ iv_count.
      rv_succeed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD assert_by_xpath_count.
    TRY.
        DATA(lo_document) = cl_rsdd_cs_xml_reader=>create_document( iv_xml_xpayload ).
      CATCH cx_rsdd_cs_xml_invalid.
        RETURN.
    ENDTRY.

    DATA(lo_elem) = lo_document->find_from_path_ns( path = iv_xml_xpath default_uri = lo_document->get_namespace_uri( ) ).

    CHECK lo_elem IS BOUND.
    IF lo_elem->get_line( ) EQ iv_count.
      rv_succeed = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD determine_ori_msg_header.
    cl_xms_persist=>get_current_table_container(
        IMPORTING
          ex_master       = DATA(lv_master) ).
    SELECT SINGLE FROM (lv_master)
       FIELDS ref_to_msg
       WHERE msgguid = @iv_msg_id
       AND   ref_to_msg NE '00000000000000000000000000000000'
       INTO @rs_msg_header-msgguid.
    IF sy-subrc NE 0.
      rs_msg_header-msgguid = iv_msg_id.
    ENDIF.

    DATA(lt_related_msg) = cl_xms_persist=>read_nested_msg( rs_msg_header-msgguid ).

    "find the original request msg id,if the input is an ref msg id(response or ack)
    LOOP AT lt_related_msg INTO DATA(ls_msg).
      CASE ls_msg-pid.
        WHEN cl_xms_main=>co_pipeline_sender."OUTBOUND
          IF ls_msg-ref_to_msg IS INITIAL.
            rs_msg_header = CORRESPONDING #( BASE ( rs_msg_header ) ls_msg ).

            IF ls_msg-adapt_typ EQ 'IENGINE' AND ls_msg-adapt_tp_i EQ 'PROXY'.
              rs_msg_header-msgguid = ls_msg-msgguid.
              "proxy info
              rs_msg_header-prx_si_name = ls_msg-ob_name.
              rs_msg_header-prx_si_namespace = ls_msg-ob_ns.
            ENDIF.
          ELSE.
            IF ls_msg-msgtype = 'S'."synchronous
              DATA(ls_response) = CORRESPONDING sxmspemas_sr( ls_msg ).
            ENDIF.
            IF ls_msg-adapt_typ EQ 'PROXY' AND ls_msg-adapt_tp_i EQ 'IENGINE'.
              rs_msg_header-msgguid = ls_msg-ref_to_msg.
              "proxy info
              rs_msg_header-prx_si_name = ls_msg-ib_name.
              rs_msg_header-prx_si_namespace = ls_msg-ib_ns.
            ENDIF.
          ENDIF.
        WHEN cl_xms_main=>co_pipeline_receiver."INBOUND
          IF ls_msg-ref_to_msg IS INITIAL.
            rs_msg_header = CORRESPONDING #( BASE ( rs_msg_header ) ls_msg ).

            IF ls_msg-adapt_typ EQ 'PROXY' AND ls_msg-adapt_tp_i EQ 'AENGINE'.
              rs_msg_header-msgguid = ls_msg-msgguid.
              "proxy info
              rs_msg_header-prx_si_name = ls_msg-ib_name.
              rs_msg_header-prx_si_namespace = ls_msg-ib_ns.
            ENDIF.
          ELSE.
            IF ls_msg-msgtype = 'S'."synchronous
              ls_response = CORRESPONDING sxmspemas_sr( ls_msg ).
            ENDIF.
            IF ls_msg-adapt_typ EQ 'IENGINE' AND ls_msg-adapt_tp_i EQ 'PROXY'.
              rs_msg_header-msgguid = ls_msg-ref_to_msg.
              "proxy info
              rs_msg_header-prx_si_name = ls_msg-ob_name.
              rs_msg_header-prx_si_namespace = ls_msg-ob_ns.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    CHECK sy-subrc EQ 0.

    DATA(lt_operations) = cl_proxy_query=>get_interface_proxies( name = rs_msg_header-prx_si_name namespace = rs_msg_header-prx_si_namespace ).
    CHECK lt_operations IS NOT INITIAL.

    "even if there are multi operation in this SI,the OBJ&OBJ_NAME are the same.
    DATA(ls_operation) = lt_operations[ 1 ].

    IF lines( lt_operations ) EQ 1.
      rs_msg_header-abap_class_name = ls_operation-obj_name.
      rs_msg_header-abap_method_name = ls_operation-method.
      rs_msg_header-prx_si_operation = ls_operation-operation.
    ELSE.

      "read payload
      IF iv_ori_req_payload IS NOT INITIAL.
        DATA(lv_req_xpayload) = iv_ori_req_payload.
      ELSE.
        get_payload( EXPORTING iv_msgid = rs_msg_header-msgguid IMPORTING ev_payload = DATA(lv_req_payload) ev_xpayload = lv_req_xpayload ).
      ENDIF.

      "determine operation by payload
      determine_mt_by_mp(
        EXPORTING
          iv_xstr_payload  = lv_req_xpayload
        IMPORTING
          ev_msg_name      = DATA(lv_msg_name)
          ev_msg_namespace = DATA(lv_msg_namespace) ).

      rs_msg_header-abap_class_name = ls_operation-obj_name.

      determine_ifr_operation(
        EXPORTING
          iv_pid_ext           = ls_msg-pid
          iv_proxy_ns          = rs_msg_header-prx_si_namespace
          iv_proxy_name        = rs_msg_header-prx_si_name
          iv_req_msg_name      = lv_msg_name
          iv_req_msg_namespace = lv_msg_namespace
        IMPORTING
          ev_proxy_method = rs_msg_header-abap_method_name
          ev_operation_name = rs_msg_header-prx_si_operation ).
      "OR READ FROM lt_operations
    ENDIF.

    "write operation to the list info
    CASE rs_msg_header-pid.
      WHEN cl_xms_main=>co_pipeline_sender."OUTBOUND
        rs_msg_header-ob_operation = rs_msg_header-prx_si_operation.
        DATA(lv_receiver_prefix) = 'IB'.
      WHEN cl_xms_main=>co_pipeline_receiver.
        rs_msg_header-ib_operation = rs_msg_header-prx_si_operation.
        lv_receiver_prefix = 'OB'.
    ENDCASE.

    "filling receiver infomation
*    IF rs_msg_header-msgtype = 'S'."synchronous
*      LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( ls_response ) )->components INTO DATA(ls_component).
*        CHECK ls_component-name+0(2) EQ lv_receiver_prefix.
*
*        CHECK find( val = ls_component-name sub = 'OPERATION' occ = -1 len = 9 ) EQ -1.
*
*        ASSIGN COMPONENT ls_component-name OF STRUCTURE ls_response TO FIELD-SYMBOL(<lv_receiver_value>).
*        ASSIGN COMPONENT ls_component-name OF STRUCTURE rs_msg_header TO FIELD-SYMBOL(<lv_sender_value>).
*        <lv_sender_value> = <lv_receiver_value>.
*      ENDLOOP.
*    ENDIF.
  ENDMETHOD.

ENDCLASS.
