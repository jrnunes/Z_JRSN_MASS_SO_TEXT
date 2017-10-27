REPORT Z_JRSN_MASS_SO_TEXT.

*Text Symbols:
*B01	Edit Text
*T01	Please enter the new text
*T02	Please confirm your action
*T03	Are you sure you want to proceed with the mass edition of Sales Order?
*T04	Yes
*T05	No
*T06	Text data

*P_ID	Text ID
*P_OBJECT	Text Object
*P_SPRAS	Language Key
*S_ORDER	Sales Document
type-pools: icon.
tables: vbak.

class lcl_app definition.
    public section.
      methods: constructor importing order_list type range_vbeln_va_tab
                                     texts type tlinetab
                                     text_object type tdobject
                                     text_id type tdid
                                     text_language type spras,
               start.

    private section.
      types: begin of ty_vbak,
              vbeln type vbeln,
              spart type spart,
              vkorg type vkorg,
              vtweg type vtweg,
             end of ty_vbak,

             tty_vbak type hashed table of ty_vbak with unique key vbeln.
      data: t_orders type tty_vbak,
            r_order_list type range_vbeln_va_tab,
            t_log type bapiret2_tab,
            texts type tlinetab,
            text_object type tdobject,
            text_id type tdid,
            text_language type spras.

      methods: confirm returning value(is_confirmed) type abap_bool,
               fetch_sales_order,
               authority_check importing order type ty_vbak
                               returning value(is_authorized) type abap_bool,
               change_text importing order_number type vbeln.

  endclass.

  class lcl_app implementation.
    method constructor.
      me->r_order_list = order_list.
      me->text_object = text_object.
      me->text_id = text_id.
      me->text_language = text_language.
      me->texts = texts.
    endmethod.

    method start.
      check confirm( ) = abap_true.

      fetch_sales_order( ).

      loop at t_orders assigning field-symbol(<order>).
        if authority_check( <order> ) eq abap_true.
          change_text( <order>-vbeln ).
        else.
          "TODO: add_log( <order> ).
        endif.
      endloop.
    endmethod.

    method confirm.
      data: answer type c.

      call function 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = text-t02
          TEXT_QUESTION         = text-t03
          TEXT_BUTTON_1         = text-t04
          ICON_BUTTON_1         = 'ICON_OKAY'
          TEXT_BUTTON_2         = text-t05
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '2'
          DISPLAY_CANCEL_BUTTON = space
        IMPORTING
          ANSWER                = answer.

      is_confirmed = cond #( when answer = '1' then abap_true else abap_false ).
    endmethod.

    method fetch_sales_order.
      select vbeln spart vkorg vtweg
        from vbak
        into table t_orders
       where vbeln in r_order_list.
    endmethod.

    method authority_check.
      authority-check object 'V_VBAK_VKO'
        id 'ACTVT' field '2'
        id 'SPART' field order-spart
        id 'VKORG' field order-vkorg
        id 'VTWEG' field order-vtweg.
      is_authorized = cond #( when sy-subrc = 0 then abap_true else abap_false ).
    endmethod.

    method change_text.

      DATA: is_insert TYPE abap_bool,
            text_name TYPE tdobname,
            text_header TYPE THEAD,
            text_lines TYPE STANDARD TABLE OF tline.

      text_name = order_number.
      text_header-tdid = text_id.
      text_header-tdname = text_name.
      text_header-tdobject = text_object.
      text_header-tdspras = text_language.

      call function 'READ_TEXT'
        EXPORTING
          ID                      = text_id
          LANGUAGE                = text_language
          NAME                    = text_name
          OBJECT                  = text_object
        TABLES
          LINES                   = text_lines
        EXCEPTIONS
          ID                      = 1
          LANGUAGE                = 2
          NAME                    = 3
          NOT_FOUND               = 4
          OBJECT                  = 5
          REFERENCE_CHECK         = 6
          WRONG_ACCESS_TO_ARCHIVE = 7
          OTHERS                  = 8.

      is_insert = cond #( when sy-subrc eq 0 then abap_false else abap_true ).

*        CALL FUNCTION 'DELETE_TEXT'
*          EXPORTING
*            id        = text_id
*            language  = text_language
*            name      = text_name
*            object    = text_object
*          EXCEPTIONS
*            not_found = 1.

      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          HEADER          = text_header
          INSERT          = is_insert
          SAVEMODE_DIRECT = 'X'
        TABLES
          LINES           = texts
        EXCEPTIONS
          ID              = 1
          LANGUAGE        = 2
          NAME            = 3
          OBJECT          = 4
          OTHERS          = 5.

      if sy-subrc eq 0.
        "TODO: Add Log = success
      else.
        "TODO: Add Log = error
      endif.
    endmethod.
  endclass.

data: lv_title type SYTITLE,
      lt_text type CATSXT_LONGTEXT_ITAB,
      lt_text_final type tlinetab,
      ls_text_final type tline.

SELECT-OPTIONS: s_order FOR vbak-vbeln obligatory.
selection-screen skip.
selection-screen begin of block b1 with frame title text-t06.
PARAMETERS: p_object TYPE TDOBJECT obligatory,
            p_id     TYPE TDID obligatory,
            p_spras  TYPE SPRAS obligatory.
selection-screen skip.
SELECTION-SCREEN: begin of line.
SELECTION-SCREEN  pushbutton 2(40) BUT1 USER-COMMAND text .
SELECTION-SCREEN: end of line.
selection-screen end of block b1.

INITIALIZATION.
  lv_title = text-t01.
  CONCATENATE ICON_CREATE_TEXT text-b01 INTO but1.

AT SELECTION-SCREEN.

  case sy-ucomm.
    when 'TEXT'.
      CALL function 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          IM_TITLE        = lv_title
        CHANGING
          CH_TEXT         = lt_text.

    if lt_text[] is not initial.
      refresh lt_text_final.
      loop at lt_text assigning field-symbol(<text>).
        ls_text_final-tdformat = '*'.
        ls_text_final-tdline = <text>.
        append ls_text_final to lt_text_final.
      endloop.
    endif.
  endcase.

START-OF-SELECTION.

  DATA: lo_app TYPE REF TO lcl_app,
        lr_order_list TYPE range_vbeln_va_tab.

  append lines of s_order to lr_order_list.

  create object LO_APP
    EXPORTING
      ORDER_LIST    = lr_order_list
      TEXT_OBJECT   = p_object
      TEXT_ID       = p_id
      TEXT_LANGUAGE = p_spras
      TEXTS         = lt_text_final.

  lo_app->start( ).