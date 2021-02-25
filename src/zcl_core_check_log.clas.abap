CLASS zcl_core_check_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA:
      _message TYPE string.

    CONSTANTS:
      gc_object     TYPE balobj_d  VALUE 'ZBW',
      gc_sub_object TYPE balsubobj VALUE 'ZCHECK'.

    CLASS-METHODS
      push_msgtype
        IMPORTING
          it_exception TYPE zcl_core_check_type=>gtyt_check_exp OPTIONAL
          irx_check    TYPE REF TO zcx_core_check OPTIONAL
          iv_msgty     TYPE sy-msgty OPTIONAL
        RAISING
          zcx_core_check.
    CLASS-METHODS
      raise_syst_t
        IMPORTING
          it_exception TYPE zcl_core_check_type=>gtyt_check_exp
          iv_msgid     TYPE sy-msgid DEFAULT sy-msgid
          iv_msgty     TYPE sy-msgty DEFAULT sy-msgty
          iv_msgno     TYPE sy-msgno DEFAULT sy-msgno
          iv_msgv1     TYPE sy-msgv1 DEFAULT sy-msgv1
          iv_msgv2     TYPE sy-msgv2 DEFAULT sy-msgv2
          iv_msgv3     TYPE sy-msgv3 DEFAULT sy-msgv3
          iv_msgv4     TYPE sy-msgv4 DEFAULT sy-msgv4
          ir_object    TYPE REF TO zif_core_check
        RAISING
          zcx_core_check.
    CLASS-METHODS
      get_probclas
        IMPORTING
          iv_msgty           TYPE msgty
          ir_log             TYPE REF TO zcl_core_check_log OPTIONAL
        RETURNING
          VALUE(rv_probclas) TYPE balprobcl.
    CLASS-METHODS
      raise_syst
        IMPORTING
          ir_check TYPE REF TO zif_core_check
        RAISING
          zcx_core_check_msg.
    CLASS-METHODS
      get_msgty
        IMPORTING
          irx_check    TYPE REF TO zcx_core_check OPTIONAL
          it_exception TYPE zcl_core_check_type=>gtyt_check_exp OPTIONAL
        EXPORTING
          erx_check    TYPE REF TO zcx_core_check
          ev_msgty     TYPE msgty.

    CLASS-METHODS
      do_display_all_logs.

    DATA:
      ns_max_msg TYPE bal_s_msg.

    METHODS
      constructor
        IMPORTING
          iv_text      TYPE clike OPTIONAL
          iv_object    TYPE balobj_d DEFAULT gc_object
          iv_subobject TYPE balsubobj DEFAULT gc_sub_object .
    METHODS
      add_message
        IMPORTING
          iv_msgid TYPE sy-msgid DEFAULT sy-msgid
          iv_msgno TYPE sy-msgno DEFAULT sy-msgno
          iv_msgty TYPE sy-msgty DEFAULT sy-msgty
          iv_msgv1 TYPE sy-msgv1 DEFAULT sy-msgv1
          iv_msgv2 TYPE sy-msgv2 DEFAULT sy-msgv2
          iv_msgv3 TYPE sy-msgv3 DEFAULT sy-msgv3
          iv_msgv4 TYPE sy-msgv4 DEFAULT sy-msgv4
          is_msg   TYPE rs_s_msg OPTIONAL
          ir_check TYPE REF TO zif_core_check.
    METHODS add_exception
      IMPORTING
        ir_exc TYPE REF TO cx_root.
    METHODS
      do_display_log.
    class-METHODS
      do_display_exception
        IMPORTING
          ir_exc TYPE REF TO zcx_core_check.
    METHODS
      do_save_log.
    METHODS
      do_free_log.
    METHODS
      prepare_exception
        IMPORTING
          iv_iteration    TYPE i
          ir_exc          TYPE REF TO cx_root
        RETURNING
          VALUE(rv_msgty) TYPE msgty.
    METHODS
      get_overwrite
        IMPORTING
          irx_check       TYPE REF TO zcx_core_check
        RETURNING
          VALUE(rv_msgty) TYPE msgty.
    METHODS set_level
      IMPORTING
        iv_level TYPE ballevel OPTIONAL
        iv_add   TYPE rs_bool OPTIONAL
        iv_sub   TYPE rs_bool OPTIONAL.
    METHODS add_text
      IMPORTING
        iv_text TYPE clike.


  PROTECTED SECTION.

    CLASS-DATA:
      gt_all_log  TYPE bal_t_logh.

    DATA:
      nv_log_handle TYPE balloghndl,
      nv_level      TYPE ballevel,
      nv_i_level    TYPE i.

  PRIVATE SECTION.

    METHODS
      _get_msgty
        IMPORTING
          irx_check TYPE REF TO zcx_core_check
        EXPORTING
          ev_msgty  TYPE msgty
          ev_reset  TYPE rs_bool.

ENDCLASS.



CLASS zcl_core_check_log IMPLEMENTATION.


  METHOD set_level.
    IF NOT iv_level IS INITIAL.
      nv_level = iv_level.
      nv_i_level = iv_level.
    ENDIF.
    IF iv_add = rs_c_true.
      nv_i_level = nv_i_level + 1.
    ENDIF.
    IF iv_sub = rs_c_true.
      nv_i_level = nv_i_level - 1.
    ENDIF.
    IF nv_i_level > 9.
      nv_level = '9'.
    ELSEIF nv_i_level < 1.
      nv_level = '1'.
    ELSE.
      MOVE nv_i_level TO nv_level.
    ENDIF.
  ENDMETHOD.

  METHOD add_text.
    DATA lv_char(256) TYPE c.
    lv_char = iv_text.
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle = nv_log_handle
        i_msgty      = rs_c_info
        i_text       = lv_char.
  ENDMETHOD.

  METHOD push_msgtype.
    DATA:
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp,
      lr_exception TYPE zcl_core_check_type=>gtyr_check_exp,
      lrx_check    TYPE REF TO zcl_core_check,
      lv_msgty     TYPE msgty.
    IF iv_msgty IS INITIAL AND irx_check IS NOT BOUND.
      " Nothing have been delivered that can help find a
      " msgty to be propagated
      RETURN.
    ELSE.
      " Find the MSGTY from input
      " or exception
      lv_msgty = iv_msgty.
      IF lv_msgty IS INITIAL.
        " Do not overwrite the supplied one
        lv_msgty = irx_check->msgty.
      ENDIF.
      IF it_exception IS SUPPLIED.
        " If there is a table use this one
        " This can only come from an external call
        lt_exception = it_exception.
      ELSE.
        " This can be both
        lt_exception = irx_check->t_exception.
      ENDIF.
      IF lt_exception IS NOT INITIAL.
        LOOP AT lt_exception INTO lr_exception.
          CALL METHOD push_msgtype
            EXPORTING
              irx_check = lr_exception
              iv_msgty  = lv_msgty.
        ENDLOOP.
      ENDIF.
      IF irx_check IS BOUND.
        irx_check->msgty = iv_msgty.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD raise_syst_t.
    DATA:
    lv_msgty TYPE msgty.
    CALL METHOD zcl_core_check_log=>get_msgty
      EXPORTING
        it_exception = it_exception
      IMPORTING
        ev_msgty     = lv_msgty.
    RAISE EXCEPTION TYPE zcx_core_check
      EXPORTING
        msgid       = iv_msgid
        msgty       = lv_msgty
        msgno       = iv_msgno
        msgv1       = iv_msgv1
        msgv2       = iv_msgv2
        msgv3       = iv_msgv3
        msgv4       = iv_msgv4
        t_exception = it_exception
        r_object    = ir_object.
  ENDMETHOD.

  METHOD get_probclas.
** Return the problem class based on the message type
** If ir_log is suppled return the problem class for the log
** meaning the value of message with the lowest problem class
    IF ir_log IS SUPPLIED.
      rv_probclas = get_probclas( ir_log->ns_max_msg-msgty ).
    ELSE.
      CASE iv_msgty.
        WHEN 'A' OR 'E'.
          rv_probclas = '1'.
        WHEN 'W'.
          rv_probclas = '2'.
        WHEN 'I'.
          rv_probclas = '4'.
        WHEN OTHERS.
** If called empty - REturn the
          rv_probclas = '4'.
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD get_msgty.
** Do nothing for now
    DATA:
      lrx_max      TYPE REF TO zcx_core_check,
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp,
      lrx_loop     TYPE REF TO zcx_core_check.
    IF irx_check IS INITIAL.
      lt_exception = it_exception.
    ELSE.
      lt_exception = irx_check->t_exception.
      lrx_max = irx_check.
    ENDIF.
    IF lt_exception IS INITIAL.
      ev_msgty = rs_c_info.
      erx_check = irx_check.
    ELSE.
      READ TABLE lt_exception INTO lrx_max INDEX 1.
      LOOP AT lt_exception INTO lrx_loop
      WHERE table_line IS BOUND.
** Find out if the problem class of the exception is above or below
** we need the lowest number to be move up...
        CALL METHOD get_msgty
          EXPORTING
            irx_check = lrx_loop
          IMPORTING
            erx_check = lrx_loop.
        IF get_probclas( lrx_loop->msgty ) < get_probclas( lrx_max->msgty ).
          lrx_max = lrx_loop.
        ENDIF.
      ENDLOOP.
      ev_msgty = lrx_max->msgty.
      erx_check = lrx_max.
    ENDIF.
  ENDMETHOD.

  METHOD do_display_all_logs.
    DATA:
    ls_prof TYPE bal_s_prof.
* CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
* IMPORTING
* e_s_display_profile = ls_prof.
* CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
* IMPORTING
* e_s_display_profile = ls_prof.
    CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
      IMPORTING
        e_s_display_profile = ls_prof.
* ls_prof-use_grid = 'X'.
* ls_prof-tree_adjst = space.
* ls_prof-tree_ontop = 'X'.
* ls_prof-bydetlevel = 'X'.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_prof
        i_t_log_handle      = gt_all_log.
  ENDMETHOD.

  METHOD constructor.
    DATA: ls_log TYPE bal_s_log. "Log header data
* define some header data of this log
    IF ls_log-extnumber IS INITIAL.
      IF iv_text IS INITIAL.
        ls_log-extnumber = 'EDL compliance check'(001).
      ELSE.
        ls_log-extnumber = iv_text.
      ENDIF.
    ENDIF.
    ls_log-object = iv_object.
    ls_log-subobject = iv_subobject.
    ls_log-aldate = sy-datum.
    ls_log-altime = sy-uzeit.
    ls_log-aluser = sy-uname.
    ls_log-alprog = sy-repid.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = nv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    APPEND nv_log_handle TO gt_all_log.
    CALL METHOD set_level( iv_level = '1' ).
  ENDMETHOD.



  METHOD raise_syst.
    RAISE EXCEPTION TYPE zcx_core_check_msg
      EXPORTING
        msgid    = sy-msgid
        msgty    = sy-msgty
        msgno    = sy-msgno
        msgv1    = sy-msgv1
        msgv2    = sy-msgv2
        msgv3    = sy-msgv3
        msgv4    = sy-msgv4
        r_object = ir_check.
  ENDMETHOD.

  METHOD add_exception.
    DATA:
      lv_msgty    TYPE msgty,
      lr_exc      TYPE REF TO zcx_core_check,
      lr_exc_loop TYPE REF TO zcx_core_check,
      lv_reset    TYPE rs_bool,
      ls_exc      TYPE bal_s_exc,
      ls_msg      TYPE bal_s_msg.
    CHECK ir_exc IS BOUND.
    CALL METHOD prepare_exception
      EXPORTING
        iv_iteration = 1
        ir_exc       = ir_exc.
    TRY.
        lr_exc ?= ir_exc.

        ls_msg-detlevel = nv_level.
        ls_msg-msgty = lr_exc->msgty.
        ls_msg-msgid = lr_exc->msgid.
        ls_msg-msgno = lr_exc->msgno.

        ls_msg-msgv1 = lr_exc->msgv1.
        ls_msg-msgv2 = lr_exc->msgv2.
        ls_msg-msgv3 = lr_exc->msgv3.
        ls_msg-msgv4 = lr_exc->msgv4.
        ls_msg-probclass = get_probclas( lr_exc->msgty ).
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = nv_log_handle
            i_s_msg          = ls_msg
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
** Let's see if this message is worse than the existing bad if so save it
** we need to push the message in case of messages
          IF ns_max_msg IS INITIAL OR get_probclas( ls_msg-msgty ) < get_probclas( ns_max_msg-msgty ).
            ns_max_msg = ls_msg.
          ENDIF.
        ENDIF.
        CALL METHOD set_level( iv_add = rs_c_true ).
        LOOP AT lr_exc->t_exception INTO lr_exc_loop.
          CALL METHOD add_exception( lr_exc_loop ).
        ENDLOOP.
        CALL METHOD set_level( iv_sub = rs_c_true ).
      CATCH cx_sy_move_cast_error.
** It should be done so that the only first call will be as ZCX...
** subsequent calls is given the MSGTY from the last select
        ls_exc-probclass = get_probclas( rs_c_error ).
        ls_exc-detlevel = nv_level.
        ls_exc-msgty = rs_c_error.
        ls_exc-exception = ir_exc.
        CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
          EXPORTING
            i_log_handle     = nv_log_handle
            i_s_exc          = ls_exc
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
    ENDTRY.
    IF ir_exc->previous IS BOUND.
      CALL METHOD:
      set_level( iv_add = rs_c_true ),
      add_exception( ir_exc->previous ),
      set_level( iv_sub = rs_c_true ).
    ENDIF.
  ENDMETHOD.

  METHOD add_message.
    DATA:
    lr_exc TYPE REF TO zcx_core_check_msg.
    IF is_msg IS INITIAL.
      CREATE OBJECT lr_exc
        EXPORTING
          msgid    = iv_msgid
          msgty    = iv_msgty
          msgno    = iv_msgno
          msgv1    = iv_msgv1
          msgv2    = iv_msgv2
          msgv3    = iv_msgv3
          msgv4    = iv_msgv4
          r_object = ir_check.
    ELSE.
      CREATE OBJECT lr_exc
        EXPORTING
          msgid    = is_msg-msgid
          msgty    = is_msg-msgty
          msgno    = is_msg-msgno
          msgv1    = is_msg-msgv1
          msgv2    = is_msg-msgv2
          msgv3    = is_msg-msgv3
          msgv4    = is_msg-msgv4
          r_object = ir_check.
    ENDIF.
    CALL METHOD add_exception( lr_exc ).
  ENDMETHOD.

  METHOD do_display_log.
    DATA:
      ls_prof       TYPE bal_s_prof,
      lt_log_handle TYPE bal_t_logh.
    APPEND nv_log_handle TO lt_log_handle.
* CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
* IMPORTING
* e_s_display_profile = ls_prof.
* CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
* IMPORTING
* e_s_display_profile = ls_prof.
    CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
      IMPORTING
        e_s_display_profile = ls_prof.
* ls_prof-use_grid = 'X'.
* ls_prof-tree_adjst = space.
* ls_prof-tree_ontop = 'X'.
* ls_prof-bydetlevel = 'X'.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_prof
        i_t_log_handle      = lt_log_handle.
  ENDMETHOD.

  METHOD do_display_exception.
    DATA:
      ls_prof TYPE bal_s_prof,
      " ls_exit TYPE bal_s_excm,
      lr_log  TYPE REF TO zcl_core_check_log.
** Create a log
    CREATE OBJECT lr_log.
    CALL METHOD lr_log->add_exception( ir_exc ).
    CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
      IMPORTING
        e_s_display_profile = ls_prof.
    ls_prof-start_col = 5.
    ls_prof-start_row = 5.
    ls_prof-end_col = 87.
    ls_prof-end_row = 25.
    ls_prof-pop_adjst = rs_c_true.
* CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
* IMPORTING
* e_s_display_profile = ls_prof.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_prof.
    MESSAGE ID lr_log->ns_max_msg-msgid
    TYPE lr_log->ns_max_msg-msgty
    NUMBER lr_log->ns_max_msg-msgno
    WITH lr_log->ns_max_msg-msgv1
    lr_log->ns_max_msg-msgv2
    lr_log->ns_max_msg-msgv3
    lr_log->ns_max_msg-msgv4
    INTO _message.
    lr_log->do_free_log( ).
    FREE lr_log.
  ENDMETHOD.

  METHOD do_save_log.
    DATA:
    lt_log_handle TYPE bal_t_logh.
    APPEND nv_log_handle TO lt_log_handle.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = lt_log_handle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgty
      TYPE sy-msgty
      NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD do_free_log.
    DELETE gt_all_log
       WHERE table_line = nv_log_handle.

    CALL FUNCTION 'BAL_LOG_DELETE'
      EXPORTING
        i_log_handle = nv_log_handle.
  ENDMETHOD.

  METHOD prepare_exception.
  DATA:
    lv_msgty       TYPE msgty,
    lv_maxty       TYPE msgty,
    lv_iteration   TYPE i,
    lrx_check      TYPE REF TO zcx_core_check,
    lrx_check_loop TYPE REF TO zcx_core_check.
  lv_iteration = iv_iteration + 1.
  TRY.
      " This cast is for seperatio of SAP's excetions
      " and mine - zcx_core_check will have a table of excetions
      " below. The intention is to set the MSGTY of the ir_exc
      " to the value of the worst msgty in the table
      " If there is warning in the table. The IR_EXC get's warning and
      " sends the Warning of this level back
      lrx_check ?= ir_exc.

      IF lrx_check->previous IS BOUND.
        lrx_check->msgty = prepare_exception(
        iv_iteration = lv_iteration
        ir_exc = lrx_check->previous ).
      ENDIF.
      lrx_check->msgty = get_overwrite( lrx_check ).
      IF lrx_check->t_exception IS INITIAL.
        " Nothing more to do
        rv_msgty = lrx_check->msgty.
        RETURN.
      ELSE.
        rv_msgty = rs_c_info.
      ENDIF.
      LOOP AT lrx_check->t_exception INTO lrx_check_loop.
        lv_msgty = prepare_exception(
        iv_iteration = lv_iteration
        ir_exc = lrx_check_loop ).
        IF get_probclas( rv_msgty ) < get_probclas( lv_msgty ).
          rv_msgty = lv_msgty.
        ENDIF.
      ENDLOOP.
      lrx_check->msgty = rv_msgty.
    CATCH cx_sy_move_cast_error.
      " This is not a ZCX_CORE_CHECK exception - We must
      " think of it as an error
      rv_msgty = rs_c_error.
  ENDTRY.
ENDMETHOD.

METHOD get_overwrite.
  DATA:
    ls_msg_check TYPE zcore_check,
    lt_msg_check TYPE STANDARD TABLE OF zcore_check.

  " So - if nothing is found, make sure to return the original
  " message type
  rv_msgty = irx_check->msgty.

  SELECT *
      INTO TABLE @lt_msg_check
      FROM zcore_check
      WHERE msgid = @irx_check->msgid AND
            msgno = @irx_check->msgno.
    IF sy-subrc = 0.
** Basically this is a list of overwrites, so do not perform this check with this error
** for this object or object type - If it's a 'X'
      read table lt_msg_check into ls_msg_check
        with key tlogo = irx_check->r_object->get_tlogo( )
                 objnm = irx_check->r_object->get_objnm(  ).
      IF sy-subrc = 0.
        " So we have an exception directly on the object
        rv_msgty = ls_msg_check-msgty.
      ELSE.
        READ TABLE lt_msg_check INTO ls_msg_check
          WITH KEY tlogo = irx_check->r_object->get_tlogo(  )
                   objnm = ''.
        IF sy-subrc = 0.
          " We have an exception directly for the object type
          rv_msgty = ls_msg_check-msgty.
        ELSE.
          READ TABLE lt_msg_check INTO ls_msg_check
            WITH KEY tlogo = ''
                     objnm = ''.
          IF sy-subrc = 0 .
            " Bypass this check completely with a new message type
            rv_msgty = ls_msg_check-msgty.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD _get_msgty.
    DATA:
      ls_msg_check TYPE zcore_check,
      lv_max_msgty TYPE msgty,
      lt_msg_check TYPE STANDARD TABLE OF zcore_check.
    CALL METHOD get_msgty
      EXPORTING
        irx_check = irx_check
      IMPORTING
        ev_msgty  = lv_max_msgty.
    IF get_probclas( irx_check->msgty ) < get_probclas( lv_max_msgty ).
      ev_msgty = irx_check->msgty.
      ev_reset = rs_c_false.
    ELSE.
      ev_msgty = lv_max_msgty.
      ev_reset = rs_c_false.
    ENDIF.

    SELECT *
        INTO TABLE lt_msg_check
        FROM zcore_check
        WHERE msgid = irx_check->msgid AND
              msgno = irx_check->msgno.
      IF sy-subrc = 0.
** Basically this is a list of overwrites, so do not perform this check with this error
** for this object or object type - If it's a 'X'
        read table lt_msg_check into ls_msg_check
            with key tlogo = irx_check->r_object->get_tlogo(  ) "Logical transport object of the checking instance
                     objnm = irx_check->r_object->get_objnm( ).
        IF sy-subrc = 0.
          ev_msgty = ls_msg_check-msgty.
          ev_reset = rs_c_true.
        ELSE.
          READ TABLE lt_msg_check INTO ls_msg_check
              WITH KEY tlogo = irx_check->r_object->get_tlogo( )
                        objnm = ''.
          IF sy-subrc = 0.
            ev_msgty = ls_msg_check-msgty.
            ev_reset = rs_c_true.
          ELSE.
            READ TABLE lt_msg_check INTO ls_msg_check
                WITH KEY tlogo = ''
                         objnm = ''.
            IF sy-subrc = 0 .
              ev_msgty = ls_msg_check-msgty.
              ev_reset = rs_c_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDMETHOD.



ENDCLASS.
