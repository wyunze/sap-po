*&---------------------------------------------------------------------*
*& Report zpo_restart_ehanced
*&---------------------------------------------------------------------*
*&
*001 Restart Selection
*002 Assertion by tag value
*003 Assertion by tag count
*004 Assertion by xpath value
*005 Assertion by xpath count
*006 Assertion by contain text
*007 System Status
*008 Application Status
*009 Original message ID
*010 New message ID
*011 Technical Error Message
*012 Message Payload
*013 Edit Original Request
*014 Restart Message
*015 Display New Response
*016 Response Message has been mapped to the ReftoMsg
*017 Press Ctrl+S to save after edit!
*018 You did not save the document

*P_ASSERT If Assertion
*P_CTV1   Contained text value1
*P_CTV2   Contained text value2
*P_TCTN   Tag name
*P_TCTV   Count value
*P_TVTN   Tag name
*P_TVTV   Tag value
*P_UP2PRT Update parent message,Caution!
*P_XCXN   Xpath name
*P_XCXV   Count value
*P_XVXN   Xpath name
*P_XVXV   Xpath value
*S_MSGID . "ddic
*&---------------------------------------------------------------------*
REPORT zrre_po_restart_ehanced.

*-----CLASS definition
CLASS lcl_si_restart DEFINITION CREATE PUBLIC FINAL.

  PUBLIC SECTION.
*    INTERFACES: zif_xml_assertion.

    TYPES:
      BEGIN OF ENUM tv_assert_type,
        assert_by_contain_text,
        assert_by_tag_value,
        assert_by_tag_count,
        assert_by_xpath_value,
        assert_by_xpath_count,
      END OF ENUM tv_assert_type,

      BEGIN OF ts_process_log,
        msg_id           TYPE sxmsmguid,
        system_status    TYPE msgty,
        sys_status_icon  TYPE icon_d,
        appl_status      TYPE msgty, "Assertion
        appl_status_icon TYPE icon_d,
        new_msg_id       TYPE sxmsmguid,

        "message details
        pid              TYPE sxmspid,
        msgtype          TYPE sxmspmtype,
        ob_system        TYPE ait_sndr,
        ob_ns            TYPE rm_oifns,
        ob_name          TYPE rm_oifname,
        ob_operation     TYPE sxi_operation,
        ib_system        TYPE ait_sndr,
        ib_ns            TYPE rm_oifns,
        ib_name          TYPE rm_oifname,
        ib_operation     TYPE sxi_operation,

        system_message   TYPE string, "system message
        req_payload      TYPE bnk_com_msg_payld,
        resp_payload     TYPE bnk_com_msg_payld,

      END OF ts_process_log,
      tt_process_log TYPE STANDARD TABLE OF ts_process_log.

    METHODS:
      start,
      edit_request
        IMPORTING
          iv_selected_row TYPE salv_de_row,
      restart_message,
      display_new_response
        IMPORTING
          iv_selected_row TYPE salv_de_row,
      display_log.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_log TYPE tt_process_log,
          mo_alv TYPE REF TO cl_salv_table.

    METHODS:
      assertion
        IMPORTING
                  iv_assert_type    TYPE tv_assert_type
                  iv_assert_name    TYPE string
                  iv_assert_value   TYPE string
                  iv_xml_payload    TYPE xstring
        RETURNING VALUE(rv_succeed) TYPE abap_bool,
      on_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.
ENDCLASS.


*----global definition
TABLES sscrfields.
DATA: gv_msg_id_name TYPE text30 VALUE 'SXMSPMAST-MSGGUID',
      go_si_restart  TYPE REF TO lcl_si_restart.
DATA: go_container TYPE REF TO cl_gui_custom_container.


*---header
SELECTION-SCREEN BEGIN OF BLOCK b_head WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_msgid FOR (gv_msg_id_name) VISIBLE LENGTH 40 NO INTERVALS.
PARAMETERS: p_up2prt TYPE abap_bool AS CHECKBOX,
            p_assert TYPE abap_bool AS CHECKBOX DEFAULT abap_false USER-COMMAND assert.
SELECTION-SCREEN END OF BLOCK b_head.

*---assertion info
SELECTION-SCREEN BEGIN OF TABBED BLOCK tabstrip FOR 24 LINES.
SELECTION-SCREEN TAB (30) TEXT-006 USER-COMMAND abct DEFAULT SCREEN 1005 MODIF ID m1.
SELECTION-SCREEN TAB (30) TEXT-002 USER-COMMAND abtv DEFAULT SCREEN 1001 MODIF ID m1.
SELECTION-SCREEN TAB (30) TEXT-003 USER-COMMAND abtc DEFAULT SCREEN 1002 MODIF ID m1.
SELECTION-SCREEN TAB (50) TEXT-004 USER-COMMAND abxv DEFAULT SCREEN 1003 MODIF ID m1.
SELECTION-SCREEN TAB (50) TEXT-005 USER-COMMAND abxc DEFAULT SCREEN 1004 MODIF ID m1.
SELECTION-SCREEN END OF BLOCK tabstrip.


*----assertion by tag value
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b_abtv WITH FRAME TITLE TEXT-002.
PARAMETERS: p_tvtn TYPE text256,
            p_tvtv TYPE text256.
SELECTION-SCREEN END OF BLOCK b_abtv.
SELECTION-SCREEN END OF SCREEN 1001.

*----assertion by tag count
SELECTION-SCREEN BEGIN OF SCREEN 1002 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b_abtc WITH FRAME TITLE TEXT-003.
PARAMETERS: p_tctn TYPE text256,
            p_tctv TYPE text256.
SELECTION-SCREEN END OF BLOCK b_abtc.
SELECTION-SCREEN END OF SCREEN 1002.

*----assertion by xpath value
SELECTION-SCREEN BEGIN OF SCREEN 1003 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b_abxv WITH FRAME TITLE TEXT-004.
PARAMETERS: p_xvxn TYPE text256,
            p_xvxv TYPE text256.
SELECTION-SCREEN END OF BLOCK b_abxv.
SELECTION-SCREEN END OF SCREEN 1003.

*----assertion by xpath count
SELECTION-SCREEN BEGIN OF SCREEN 1004 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b_abxc WITH FRAME TITLE TEXT-005.
PARAMETERS: p_xcxn TYPE text256,
            p_xcxv TYPE text256.
SELECTION-SCREEN END OF BLOCK b_abxc.
SELECTION-SCREEN END OF SCREEN 1004.

*----assertion by contain text
SELECTION-SCREEN BEGIN OF SCREEN 1005 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b_abct WITH FRAME TITLE TEXT-005.
PARAMETERS: p_ctv1 TYPE text256,
            p_ctv2 TYPE text256.
SELECTION-SCREEN END OF BLOCK b_abct.
SELECTION-SCREEN END OF SCREEN 1005.

INITIALIZATION.
  go_si_restart = NEW lcl_si_restart( ).

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      CASE sscrfields-ucomm.
        WHEN 'ABTV'.
          tabstrip-dynnr = 1001.
        WHEN 'ABTC'.
          tabstrip-dynnr = 1002.
        WHEN 'ABXV'.
          tabstrip-dynnr = 1003.
        WHEN 'ABXC'.
          tabstrip-dynnr = 1004.
        WHEN 'ABCT'.
          tabstrip-dynnr = 1005.
      ENDCASE.
  ENDCASE.


* selection screen
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN INTO DATA(ls_screen).
    IF ls_screen-group1 EQ 'M1'.
      IF p_assert EQ abap_false.
        ls_screen-active = 0.
        ls_screen-invisible = 1.
      ELSE.
        ls_screen-active = 1.
        ls_screen-invisible = 0.
      ENDIF.
      MODIFY SCREEN FROM ls_screen.
    ENDIF.
  ENDLOOP.


START-OF-SELECTION.
  go_si_restart->start( ).

END-OF-SELECTION.
  go_si_restart->display_log( ).


CLASS lcl_si_restart IMPLEMENTATION.

  METHOD start.

    LOOP AT s_msgid[] INTO DATA(ls_msgid).
      CHECK ls_msgid-low IS NOT INITIAL.

      APPEND INITIAL LINE TO mt_log ASSIGNING FIELD-SYMBOL(<fs_ls_log>).
      <fs_ls_log>-msg_id = ls_msgid-low.

      DATA(ls_org_msg_header) = zrre_cl_po_utility=>determine_ori_msg_header( <fs_ls_log>-msg_id ).

      "MAP to the original message id
      <fs_ls_log> = CORRESPONDING #( BASE ( <fs_ls_log> ) ls_org_msg_header MAPPING msg_id = msgguid ).

      IF <fs_ls_log>-msg_id NE ls_msgid-low.
        <fs_ls_log>-system_message = |{ 'Response Message has been mapped to the ReftoMsg'(016)
      } { ls_msgid-low }|."just for information
      ENDIF.

      zcl_po_utility=>get_payload(
       EXPORTING
        iv_msgid = <fs_ls_log>-msg_id
       IMPORTING
        ev_xpayload = <fs_ls_log>-req_payload  ).
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM mt_log COMPARING msg_id.
  ENDMETHOD.

  METHOD display_log.
    TRY.
        cl_salv_table=>factory(
         EXPORTING
            r_container    = cl_gui_container=>default_screen
          IMPORTING
            r_salv_table = mo_alv
          CHANGING
            t_table      = mt_log ).

        DATA(lo_columns) = mo_alv->get_columns( ).

        lo_columns->set_optimize( abap_true ).

        DATA(lo_column) = lo_columns->get_column( 'REQ_PAYLOAD' ).
        lo_column->set_visible( abap_false ).

        lo_column = lo_columns->get_column( 'RESP_PAYLOAD' ).
        lo_column->set_visible( abap_false ).

        "message
        lo_column = lo_columns->get_column( 'SYSTEM_MESSAGE' ).
        lo_column->set_short_text( CONV #( 'Technical Error Message'(011) ) ).

        "SYSTEM STATUS
        lo_column = lo_columns->get_column( 'SYSTEM_STATUS' ).
        lo_column->set_visible( abap_false ).

        lo_column = lo_columns->get_column( 'SYS_STATUS_ICON' ).
        lo_column->set_long_text( CONV #( 'System Status'(007) ) ).
        CAST cl_salv_column_table( lo_column )->set_icon( if_salv_c_bool_sap=>true ).

        "APPL STATUS
        lo_column = lo_columns->get_column( 'APPL_STATUS' ).
        lo_column->set_visible( abap_false ).

        lo_column = lo_columns->get_column( 'APPL_STATUS_ICON' ).
        lo_column->set_long_text( CONV #( 'Application Status'(008) ) ).
        CAST cl_salv_column_table( lo_column )->set_icon( if_salv_c_bool_sap=>true ).
*        CAST cl_salv_column_table( lo_column )->set_cell_type( if_salv_c_cell_type=>dropdown ).

        "MESSAGE ID
        lo_column = lo_columns->get_column( 'MSG_ID' ).
        lo_column->set_short_text( CONV #( 'Original message ID'(009) ) ).
        CAST cl_salv_column_table( lo_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).

        "NEW MESSAGE ID
        lo_column = lo_columns->get_column( 'NEW_MSG_ID' ).
        lo_column->set_short_text( CONV #( 'New message ID'(010) ) ).
        CAST cl_salv_column_table( lo_column )->set_cell_type( if_salv_c_cell_type=>hotspot ).

      CATCH cx_root INTO DATA(lo_error).
        MESSAGE s398(00) WITH lo_error->get_text( ) DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    "default toolbar
    DATA(lo_functions) = mo_alv->get_functions( ).
    lo_functions->set_default( if_salv_c_bool_sap=>true ).

*----add new toolbar
    "edit original request
    TRY.
        lo_functions->add_function(
          name     = 'EDIT_REQUEST'
          icon     = CONV #( icon_wd_text_edit )
          text     = CONV #( 'Edit Original Request'(013) )
          tooltip  = CONV #( 'Edit Original Request'(013) )
          position = if_salv_c_function_position=>left_of_salv_functions ).
        "restart
        lo_functions->add_function(
          name     = 'RESTART_MESSAGE'
          icon     = CONV #( icon_execute_object )
          text     = CONV #( 'Restart Message'(014) )
          tooltip  = CONV #( 'Restart Message'(014) )
          position = if_salv_c_function_position=>left_of_salv_functions ).
        "display new response
        lo_functions->add_function(
          name     = 'DISPLAY_NEW_RESPONSE'
          icon     = CONV #( icon_display_note )
          text     = CONV #( 'Display New Response'(015) )
          tooltip  = CONV #( 'Display New Response'(015) )
          position = if_salv_c_function_position=>left_of_salv_functions ).
        "Display acknowledgement

        "Display new acknowledgement

        "separate line
      CATCH cx_salv_existing cx_salv_wrong_call.
    ENDTRY.

    "double click app status to Display payload
    DATA(lo_event) = mo_alv->get_event( ).
    SET HANDLER on_double_click FOR lo_event.
    SET HANDLER on_link_click FOR lo_event.
    SET HANDLER on_user_command FOR lo_event.

*set selection mode
    DATA(lo_selections) = mo_alv->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    mo_alv->display( ).
    WRITE space."force to list output
  ENDMETHOD.

  METHOD assertion.
    DATA(lo_po_utility) = NEW zcl_po_utility( ).

    CASE iv_assert_type.
      WHEN assert_by_contain_text.
        IF lo_po_utility->assert_by_contain_text(
                                               iv_xml_xpayload = iv_xml_payload
                                               iv_contain_text = iv_assert_name )

        AND lo_po_utility->assert_by_contain_text(
                                               iv_xml_xpayload = iv_xml_payload
                                               iv_contain_text = iv_assert_value ).

          rv_succeed = abap_true.
        ENDIF.
      WHEN assert_by_tag_value.
        rv_succeed = lo_po_utility->assert_by_tag_value(
                                                       iv_xml_xpayload = iv_xml_payload
                                                       iv_tag_name = iv_assert_name
                                                       iv_elem_value = iv_assert_value ).
      WHEN assert_by_tag_count.
        rv_succeed = lo_po_utility->assert_by_tag_count(
                                                       iv_xml_xpayload = iv_xml_payload
                                                       iv_tag_name = iv_assert_name
                                                       iv_count = iv_assert_value ).
      WHEN assert_by_xpath_value.
        rv_succeed = lo_po_utility->assert_by_xpath_value(
                                                       iv_xml_xpayload = iv_xml_payload
                                                       iv_xml_xpath = iv_assert_name
                                                       iv_elem_value = iv_assert_value ).
      WHEN assert_by_xpath_count.
        rv_succeed = lo_po_utility->assert_by_xpath_count(
                                                       iv_xml_xpayload = iv_xml_payload
                                                       iv_xml_xpath = iv_assert_name
                                                       iv_count = iv_assert_value ).
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    CHECK column EQ 'APPL_STATUS_ICON'.
    CHECK row IS NOT INITIAL.
    go_si_restart->display_new_response( row ).
  ENDMETHOD.

  METHOD on_link_click.
    CHECK row IS NOT INITIAL.
    DATA(ls_clicked_row) = mt_log[ row ].

    CASE column.
      WHEN 'MSG_ID'.
        DATA(lv_msg_id) = ls_clicked_row-msg_id.
      WHEN 'NEW_MSG_ID'.
        lv_msg_id = ls_clicked_row-new_msg_id.
    ENDCASE.

    CHECK lv_msg_id IS NOT INITIAL.

    CALL FUNCTION 'SXMB_DISPLAY_MESSAGE_MONITOR'
      EXPORTING
        im_message_id = lv_msg_id.
  ENDMETHOD.

  METHOD on_user_command.
    DATA(lo_selections) = mo_alv->get_selections( ).
    DATA(lt_selected_rows) = lo_selections->get_selected_rows( ).

    CASE e_salv_function.
      WHEN 'EDIT_REQUEST'.
        CHECK lines( lt_selected_rows ) EQ 1.
        go_si_restart->edit_request( lt_selected_rows[ 1 ]  ).
      WHEN 'RESTART_MESSAGE'.
        go_si_restart->restart_message( ).
      WHEN 'DISPLAY_NEW_RESPONSE'.
        CHECK lines( lt_selected_rows ) EQ 1.
        go_si_restart->display_new_response( lt_selected_rows[ 1 ] ).
    ENDCASE.
  ENDMETHOD.

  METHOD display_new_response.
    CHECK iv_selected_row IS NOT INITIAL.
    DATA(ls_clicked_row) = mt_log[ iv_selected_row ].

    CHECK ls_clicked_row-system_status EQ /bobf/cm_frw=>co_severity_success
    AND ls_clicked_row-resp_payload IS NOT INITIAL.

    cl_abap_browser=>show_xml( xml_xstring = ls_clicked_row-resp_payload
    size = cl_abap_browser=>large title = 'Message Payload'(012)  context_menu = abap_true ).
  ENDMETHOD.

  METHOD edit_request.
    CHECK iv_selected_row IS NOT INITIAL.
    READ TABLE mt_log ASSIGNING FIELD-SYMBOL(<fs_ls_log>) INDEX iv_selected_row.

    MESSAGE s398(00) WITH 'Press Ctrl+S to save after edit!'(017).
    DATA(lv_saved_docu) = VALUE boolean( ).
    CALL FUNCTION 'SPRX_DOC_DISPLAY'
      EXPORTING
        p_document          = <fs_ls_log>-req_payload
        p_title             = 'Message Payload'(012)
        screen_start_column = 13
        screen_start_line   = 8
        screen_end_column   = 160
        screen_end_line     = 28
      IMPORTING
        p_new_document      = <fs_ls_log>-req_payload
        p_document_saved    = lv_saved_docu.

    IF lv_saved_docu IS INITIAL.
      MESSAGE s398(00) WITH 'You did not save the document'(018) DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

  METHOD restart_message.
    DATA(lo_selections) = mo_alv->get_selections( ).
    DATA(lt_selected_rows) = lo_selections->get_selected_rows( ).

    LOOP AT lt_selected_rows INTO DATA(ls_selected_row).
      READ TABLE mt_log ASSIGNING FIELD-SYMBOL(<fs_ls_log>) INDEX ls_selected_row.

      "Restart
      DATA(rs_output) = zcl_po_utility=>restart(
         EXPORTING
           iv_msg_id                = <fs_ls_log>-msg_id
           is_input = VALUE #(
           commit_asynchronous  = abap_true
           return_req_new_msgid = abap_true
           return_resp_in_xml   = abap_true
           update_to_parentmsg  = p_up2prt ) ).

      <fs_ls_log>-resp_payload = rs_output-response_in_xml.
      <fs_ls_log>-new_msg_id = rs_output-new_req_message_id.
      <fs_ls_log>-system_status = rs_output-technical_status.

      <fs_ls_log>-sys_status_icon = SWITCH #( <fs_ls_log>-system_status
       WHEN /bobf/cm_frw=>co_severity_success THEN icon_okay
       WHEN /bobf/cm_frw=>co_severity_error THEN icon_cancel ).

      <fs_ls_log>-system_message = rs_output-technical_message.

      CHECK <fs_ls_log>-resp_payload IS NOT INITIAL.

      IF p_assert EQ abap_true.
        CASE tabstrip-activetab.
          WHEN 'ABCT'.
            DATA(lv_succeed) = assertion(
                 iv_assert_type  = assert_by_contain_text
                 iv_assert_name  = CONV #( p_ctv1 )
                 iv_assert_value = CONV #( p_ctv2 )
                 iv_xml_payload  = <fs_ls_log>-resp_payload ).
          WHEN 'ABTV'.
            lv_succeed = assertion(
                 iv_assert_type  = assert_by_tag_value
                 iv_assert_name  = CONV #( p_tvtn )
                 iv_assert_value = CONV #( p_tvtv )
                 iv_xml_payload  = <fs_ls_log>-resp_payload ).
          WHEN 'ABTC'.
            lv_succeed = assertion(
                 iv_assert_type  = assert_by_tag_count
                 iv_assert_name  = CONV #( p_tctn )
                 iv_assert_value = CONV #( p_tctv )
                 iv_xml_payload  = <fs_ls_log>-resp_payload ).
          WHEN 'ABXV'.
            lv_succeed = assertion(
                 iv_assert_type  = assert_by_xpath_value
                 iv_assert_name  = CONV #( p_xvxn )
                 iv_assert_value = CONV #( p_xvxv )
                 iv_xml_payload  = <fs_ls_log>-resp_payload ).
          WHEN 'ABXC'.
            lv_succeed = assertion(
                 iv_assert_type  = assert_by_xpath_count
                 iv_assert_name  = CONV #( p_xcxn )
                 iv_assert_value = CONV #( p_xcxv )
                 iv_xml_payload  = <fs_ls_log>-resp_payload ).
        ENDCASE.

        IF lv_succeed EQ abap_true.
          <fs_ls_log>-appl_status = /bobf/cm_frw=>co_severity_success.
          <fs_ls_log>-appl_status_icon = icon_okay.
        ELSE.
          <fs_ls_log>-appl_status = /bobf/cm_frw=>co_severity_error.
          <fs_ls_log>-appl_status_icon = icon_cancel.
        ENDIF.
      ENDIF.
    ENDLOOP.

    mo_alv->refresh( ).
  ENDMETHOD.

ENDCLASS.
