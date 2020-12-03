*&---------------------------------------------------------------------*
*& Report zpo_uds_ehanced
*&---------------------------------------------------------------------*
*&
*001  Search By User Defined Keys(multi keys combine together)
*002  Message Selection
*003  Search Conditions
*004  Sender
*005  Receiver
*006  Key Values required
*007  No suitable message found,recheck your input
*008  Message ID Required
*009  Match By Contain Text
*010  Match By Tag Value
*011  Tag From Response
*012  Tag From Request
*
*P_ENDT	End Date
*P_ETDT	End Time
*P_FIRST  If Only Find The First
*P_HTDT	Start Time
*P_IIFNAM	Inbound Interface Name(PO)
*P_IIFNS  Inbound Interface Namespace
*P_MSGID  Message ID
*P_OIFNAM	Outbound Interface Name(PO)
*P_OIFNS  Outbound Interface Namespace
*P_RB1  Search By User Defined Keys
*P_RB2  Search By Message Id
*P_RB3   Search By Message Status Group
*P_REQ  Tag From Request
*P_RESP	Tag From Response
*P_RSERV  Receiver Component
*P_SSERV  Sender Component
*P_STATYP    Processing Status DDIC
*P_STDT	Start Date
*P_TVTN	Tag Value Tag Name
*P_TVTV	Tag Value Tag Value
*P_VALUE1	Key Value1
*P_VALUE2	Key Value2
*P_VALUE3	Key Value3
*P_VALUE4	Key Value4
*P_XVALUE	Key Value
*&---------------------------------------------------------------------*
REPORT zpo_uds_ehanced.

*----global definition
TABLES sscrfields.

*&---------------------------------------------------------------------*
*& selection screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_rb1 RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u2,
            p_rb2 RADIOBUTTON GROUP g1,
            p_rb3 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_stdt TYPE sy-datum OBLIGATORY DEFAULT sy-datum MODIF ID m1.
PARAMETERS: p_htdt TYPE sy-uzeit MODIF ID m1.
PARAMETERS: p_endt TYPE sy-datum OBLIGATORY DEFAULT sy-datum MODIF ID m1.
PARAMETERS: p_etdt TYPE sy-uzeit MODIF ID m1.

PARAMETERS: p_first TYPE xfeld AS CHECKBOX DEFAULT abap_true MODIF ID m4.
PARAMETERS: p_int TYPE int4 NO-DISPLAY MODIF ID m1 DEFAULT 1000.

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_sserv TYPE sximoni_service MODIF ID m1.
PARAMETERS: p_oifnam LIKE sxmsitf-itfnameo MODIF ID m1.
PARAMETERS: p_oifns LIKE sxmsitf-itfnso MODIF ID m1.
SELECTION-SCREEN END OF BLOCK 0.
SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-005.
PARAMETERS: p_rserv TYPE sximoni_service MODIF ID m1.
PARAMETERS: p_iifnam LIKE sxmsitf-itfnamei MODIF ID m1.
PARAMETERS: p_iifns LIKE sxmsitf-itfnsi MODIF ID m1.

*----MESSAGE ID
SELECTION-SCREEN END OF BLOCK 1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_msgid TYPE sxmsmguid MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b3.

*----status Group
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_statyp TYPE sxmspmstat_type
                     AS LISTBOX VISIBLE LENGTH 40
                     USER-COMMAND stattype_sel
                     DEFAULT if_xms_persist_const=>co_mstat_type_error
                     MODIF ID m3.
SELECTION-SCREEN END OF BLOCK b6.

*---match info
SELECTION-SCREEN BEGIN OF TABBED BLOCK tabstrip FOR 24 LINES.
SELECTION-SCREEN TAB (30) TEXT-009 USER-COMMAND contain_text DEFAULT SCREEN 1001 MODIF ID m4.
SELECTION-SCREEN TAB (30) TEXT-010 USER-COMMAND tag_value DEFAULT SCREEN 1002 MODIF ID m4.
SELECTION-SCREEN END OF BLOCK tabstrip.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_xvalue TYPE text256.
PARAMETERS: p_value1 TYPE text256.
PARAMETERS: p_value2 TYPE text256.
PARAMETERS: p_value3 TYPE text256.
PARAMETERS: p_value4 TYPE text256.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN END OF SCREEN 1001.

SELECTION-SCREEN BEGIN OF SCREEN 1002 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-001.
* logical operator
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_resp TYPE c RADIOBUTTON GROUP logo DEFAULT 'X'.
SELECTION-SCREEN COMMENT (32) TEXT-011 FOR FIELD p_resp.
PARAMETERS: p_req TYPE c RADIOBUTTON GROUP logo.
SELECTION-SCREEN COMMENT (32) TEXT-012 FOR FIELD p_req.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  p_tvtn TYPE text256,
  p_tvtv TYPE text256.
SELECTION-SCREEN END OF BLOCK b5.
SELECTION-SCREEN END OF SCREEN 1002.


INITIALIZATION.
  tabstrip-prog = sy-repid.
  tabstrip-dynnr = 1001.
  tabstrip-activetab = 'CONTAIN_TEXT'.

AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN 1000.
      CASE sscrfields-ucomm.
        WHEN 'CONTAIN_TEXT'.
          tabstrip-dynnr = 1001.
        WHEN 'TAG_VALUE'.
          tabstrip-dynnr = 1002.
        WHEN 'STATUS_TYPE'.
          tabstrip-dynnr = 1003.
      ENDCASE.
  ENDCASE.

*----------------------------------------------------------------------*
*pbo
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'M1'.
        IF p_rb2 IS NOT INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
      WHEN 'M2'.
        IF p_rb2 IS INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
      WHEN 'M3'.
        IF p_rb3 IS INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
      WHEN 'M4'.
        IF p_rb1 IS INITIAL.
          screen-active = '0'.
        ELSE.
          screen-active = '1'.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

*----------------
INITIALIZATION.
  p_htdt = sy-uzeit - 900.
  p_etdt = sy-uzeit.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*Search Data
  PERFORM frm_search_data..

END-OF-SELECTION.
*-------------------------
*-----------------FORM---------------------------BEGIN--------_-------*
*---------------------------------------------------------------------*
*  FORM FRM_SEARCH_DATA
*---------------------------------------------------------------------*
FORM frm_search_data.
  DATA: lt_msgid_range TYPE RANGE OF sxmsmguid,
        lt_mstat_tab   TYPE sxmspmstat_tab.

*---------------------------------------------------------------------*
* Search By message ID
*---------------------------------------------------------------------*
  IF p_rb2 IS NOT INITIAL.
    IF p_msgid IS INITIAL.
      MESSAGE s398(00) WITH 'Message ID Required'(008) DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      CALL FUNCTION 'SXMB_DISPLAY_MESSAGE_MONITOR'
        EXPORTING
          im_message_id = p_msgid.
    ENDIF.
    RETURN.
  ENDIF.

*---------------------------------------------------------------------*
* Search By Key
*---------------------------------------------------------------------*
  IF p_rb1 IS NOT INITIAL.

    IF tabstrip-activetab = 'CONTAIN_TEXT'.
      IF p_xvalue IS INITIAL AND p_value1 IS INITIAL AND p_value2 IS INITIAL AND p_value3 IS INITIAL AND p_value4 IS INITIAL.
        MESSAGE s398(00) WITH 'Key Values required'(006) DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ELSE.
      IF p_tvtn IS INITIAL OR p_tvtv IS INITIAL OR ( p_iifnam IS INITIAL AND p_oifns IS INITIAL ).
        MESSAGE s398(00) WITH 'Key Values required'(006) DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

*---------------------------------------------------------------------*
* Search By Status Type Group
*---------------------------------------------------------------------*
  IF p_rb3 IS NOT INITIAL.
    IF p_statyp IS INITIAL.
      MESSAGE s398(00) WITH 'Key Values required'(006) DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
*       selection by status group
      CALL FUNCTION 'SXMB_GET_STATUS_LIST'
        EXPORTING
          im_stat_type = p_statyp
        IMPORTING
          ex_mstat_tab = lt_mstat_tab.
      "APPEND LOG VERSION
      IF p_statyp EQ if_xms_persist_const=>co_mstat_type_error
      OR p_statyp EQ if_xms_persist_const=>co_mstat_type_error_appl.
        APPEND 030 TO lt_mstat_tab.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA(lt_back_pids) = cl_xms_persist=>get_pid( cl_xms_persist=>co_back_pid ).
  lt_back_pids = VALUE #( BASE lt_back_pids (
  LINES OF cl_xms_persist=>get_pid( cl_xms_persist=>co_pe_pid ) ) ).

  DATA(lt_pids) = VALUE sxms_sel_options(
  FOR ls_back_pids IN cl_xms_persist=>get_pid( cl_xms_persist=>co_back_pid )
  ( sign = 'E'
    option = 'EQ'
    low = ls_back_pids ) ).

  DATA(lt_msg_tab) = VALUE sxmsmsgtab( ).

*************************SXMB_SELECT_MESSAGES_NEW
  CALL FUNCTION 'SXMB_SELECT_MESSAGES_NEW'
    EXPORTING
      im_filter           = VALUE sxi_msg_select(
                            exedate     = p_stdt
                            exetime     = p_htdt
                            exe2date    = p_endt
                            exe2time    = p_etdt
                            pids        = lt_pids
                            client      = sy-mandt
                            msgstate_tab = lt_mstat_tab
                            sender_receiver = VALUE sxmspemas_sr(
                                ob_system      = p_sserv
                                ob_ns          = p_oifns
                                ob_name        = p_oifnam
                                ib_system      = p_rserv
                                ib_ns          = p_iifns
                                ib_name        = p_iifnam
                            )

                            )
      im_number           = p_int
    IMPORTING
      ex_msgtab           = lt_msg_tab
    EXCEPTIONS
      persist_error       = 1
      missing_parameter   = 2
      negative_time_range = 3
      too_many_parameters = 4
      no_timezone         = 5
      OTHERS              = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*****************************************
  IF lt_msg_tab IS INITIAL.
    MESSAGE s398(00) WITH 'No suitable message found,recheck your input'(007) DISPLAY LIKE 'E'.
    EXIT.
*    ELSE.
*      SELECT
*        msgguid,pid,vers
*        FROM sxmspvers
*        INTO CORRESPONDING FIELDS OF TABLE @lt_msg_tab
*        FOR ALL ENTRIES IN @lt_msg_tab
*      WHERE msgguid = @lt_msg_tab-msgguid.
  ENDIF.
***********************************************************************

  CHECK NOT lt_msg_tab[] IS INITIAL.

  IF p_rb3 IS NOT INITIAL.
    "search by status
    lt_msgid_range = VALUE #( FOR ls_msg IN lt_msg_tab
    ( sign = 'I'
      option = 'EQ'
      low    = ls_msg-msgguid ) ).
  ELSE.

*----search by key
    DATA(lo_persist) = NEW cl_xms_persist( ).
    DATA(lo_utility) = NEW zcl_po_utility( ).

    LOOP AT lt_msg_tab ASSIGNING FIELD-SYMBOL(<fs_ls_msg>).
      IF <fs_ls_msg>-msgtype = 'A'.
        CLEAR <fs_ls_msg>-vers.
      ENDIF.

      TRY.
          lo_persist->read_msg_all(
            EXPORTING
              im_msgguid = <fs_ls_msg>-msgguid
              im_pid    = <fs_ls_msg>-pid
              im_version = <fs_ls_msg>-vers
            IMPORTING
              ex_message = DATA(lo_xms_msg) ).
        CATCH cx_xms_syserr_persist.
          RETURN.
      ENDTRY.

      lo_xms_msg->deleteheaderbyname(
             nsuri  = if_xms_run_time_env=>co_nsuri
             lcname = if_xms_run_time_env=>co_lcname ).
      DATA(lt_payload) = CAST cl_xms_message_xmb( lo_xms_msg )->if_xms_message_xmb~get_main_payloads( ).
      READ TABLE lt_payload INTO DATA(ls_payload) INDEX 1.
      CHECK sy-subrc EQ 0.

      DATA(lv_payload) = VALUE string( ).
      TRY.
          cl_abap_conv_in_ce=>create(
           input = ls_payload-mainpayload->getbinarycontent( )
           ignore_cerr = abap_true
           endian = 'B'
           encoding = 'UTF-8' )->read(
          IMPORTING
            data = lv_payload ).
        CATCH cx_xms_exception cx_xms_system_error.
          RETURN.
      ENDTRY.

      IF tabstrip-activetab = 'CONTAIN_TEXT'.
        IF  p_xvalue IS NOT INITIAL.
          SEARCH lv_payload FOR p_xvalue.
          IF sy-subrc NE 0.
            DELETE lt_msg_tab.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF p_value1 IS NOT INITIAL.
          SEARCH lv_payload FOR p_value1.
          IF sy-subrc NE 0.
            DELETE lt_msg_tab.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF p_value2 IS NOT INITIAL.
          SEARCH lv_payload FOR p_value2.
          IF sy-subrc NE 0.
            DELETE lt_msg_tab.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF p_value3 IS NOT INITIAL.
          SEARCH lv_payload FOR p_value3.
          IF sy-subrc NE 0.
            DELETE lt_msg_tab.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF p_value4 IS NOT INITIAL.
          SEARCH lv_payload FOR p_value4.
          IF sy-subrc NE 0.
            DELETE lt_msg_tab.
            CONTINUE.
          ENDIF.
        ENDIF.

      ELSE.
        "Tag Value
        IF <fs_ls_msg>-ref_to_msg IS NOT INITIAL.
          "response
          IF p_resp NE abap_true.
            CONTINUE.
          ENDIF.
        ELSE.
          "Request
          IF p_req NE abap_true.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF NOT lo_utility->assert_by_tag_value( iv_tag_name = CONV #( p_tvtn ) iv_elem_value = CONV #( p_tvtv )
         iv_xml_xpayload = ls_payload-mainpayload->getbinarycontent( ) ).
          CONTINUE.
        ENDIF.

      ENDIF.
      lt_msgid_range = VALUE #( BASE lt_msgid_range (
      sign = 'I'
      option = 'EQ'
      low    = <fs_ls_msg>-msgguid ) ).


      IF p_first EQ abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF lt_msgid_range[] IS INITIAL.
    MESSAGE s398(00) WITH 'No suitable message found,recheck your input'(007) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SUBMIT rsxmb_select_messages WITH msgguid IN lt_msgid_range AND RETURN.
ENDFORM.
