CLASS zcl_core_check_iobj DEFINITION
  PUBLIC
  INHERITING FROM zcl_core_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      zif_core_check~check_before_release REDEFINITION,
      zif_core_check~check_during_create REDEFINITION,
      zif_core_check~do_load REDEFINITION,
      zif_core_check~get_block REDEFINITION,
      zif_core_check~get_layer REDEFINITION,
      zif_core_check~get_section REDEFINITION,
      zif_core_check~get_cluster REDEFINITION.

  PROTECTED SECTION.
    DATA:
      ns_iobjnm   TYPE rsd_s_viobj,
      nr_iobjnm   TYPE REF TO cl_rsd_iobj,
      nv_iobjnm   TYPE rsiobjnm,
      nr_infoarea TYPE REF TO zif_core_check.

  PRIVATE SECTION.

    METHODS:
      check_name
        RAISING
          zcx_core_check,
      check_inactive_dtp
        RAISING
          zcx_core_check,
      check_inactive_transformation
        RAISING
          zcx_core_check,
      check_active
        RAISING
          zcx_core_check.

ENDCLASS.

CLASS zcl_core_check_iobj IMPLEMENTATION.

  METHOD zif_core_check~do_load.
    DATA:
      lv_tlogo_objnm TYPE sobj_name.
    nv_iobjnm = nv_objnm.
    lv_tlogo_objnm = nv_objnm.

    IF cl_rsd_iobj=>if_rso_tlogo_general~exists_on_db(
                   i_objnm = lv_tlogo_objnm
                   i_objvers = rs_c_objvers-active ) = rs_c_true OR
       cl_rsd_iobj=>if_rso_tlogo_general~exists_on_db(
                   i_objnm = lv_tlogo_objnm
                   i_objvers = rs_c_objvers-modified ) = rs_c_true.

      nv_deleted = rs_c_false.
      CALL METHOD cl_rsd_iobj=>factory
        EXPORTING
          i_iobjnm      = nv_iobjnm
        RECEIVING
          r_r_iobj      = nr_iobjnm
        EXCEPTIONS
          input_invalid = 1
          OTHERS        = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_core_check
          EXPORTING
            msgid    = sy-msgid
            msgty    = sy-msgty
            msgno    = sy-msgno
            msgv1    = sy-msgv1
            msgv2    = sy-msgv2
            msgv3    = sy-msgv3
            msgv4    = sy-msgv4
            r_object = me.
      ENDIF.
      CALL METHOD nr_iobjnm->get_info
        IMPORTING
          e_s_viobj = ns_iobjnm
        EXCEPTIONS
          OTHERS    = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_core_check
          EXPORTING
            msgid    = sy-msgid
            msgty    = sy-msgty
            msgno    = sy-msgno
            msgv1    = sy-msgv1
            msgv2    = sy-msgv2
            msgv3    = sy-msgv3
            msgv4    = sy-msgv4
            r_object = me.
      ENDIF.
      IF ns_iobjnm-infoarea IS NOT INITIAL.
        nr_infoarea = zcl_core_check=>factory( iv_tlogo = rs_c_tlogo-infoarea
                                               iv_objnm = ns_iobjnm-infoarea ).
      ENDIF.
      nr_if_tlogo_general ?= nr_iobjnm.
      nr_maintain ?= nr_iobjnm.

    ELSE.
      nv_deleted = rs_c_true.
    ENDIF.

  ENDMETHOD. "do_load

  METHOD zif_core_check~check_during_create.

    TRY.
        CALL METHOD check_name.
      CATCH zcx_core_check INTO DATA(lrx_check).
        " rv_error_count = rv_error_count + 1.
        " We do allow this error for now... it's not really an error
        zcl_core_check_log=>do_display_exception( lrx_check ).
    ENDTRY.

  ENDMETHOD. "CHECK_DURING_CREATE

  METHOD zif_core_check~check_before_release.
    DATA:
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp,
      lrx_check    TYPE REF TO zcx_core_check.

    TRY.
        CALL METHOD check_name.
      CATCH zcx_core_check INTO lrx_check.
        APPEND lrx_check TO lt_exception.
    ENDTRY.
    TRY.
        CALL METHOD check_standard.
      CATCH zcx_core_check INTO lrx_check.
        APPEND lrx_check TO lt_exception.
    ENDTRY.
    TRY.
        CALL METHOD check_inactive_transformation.
      CATCH zcx_core_check INTO lrx_check.
        APPEND lrx_check TO lt_exception.
    ENDTRY.

    TRY.
        CALL METHOD check_inactive_dtp.
      CATCH zcx_core_check INTO lrx_check.
        APPEND lrx_check TO lt_exception.
    ENDTRY.


    IF lines( lt_exception ) > 0.
      MESSAGE e025 WITH zcl_core_check=>gc_text-check_release nv_objnm nv_tlogo INTO _message.
      CALL METHOD zcl_core_check_log=>raise_syst_t
        EXPORTING
          it_exception = lt_exception
          ir_object    = me.
    ENDIF.
  ENDMETHOD. "check_before_release

  METHOD check_active.

    CHECK nv_deleted = rs_c_false.
** Check iif 'A' = 'M'
    IF nr_iobjnm->if_rso_tlogo_maintain~is_active( ) = rs_c_false.
      MESSAGE e010 WITH nv_tlogo nv_objnm INTO _message.
      RAISE EXCEPTION TYPE zcx_core_check_msg
        EXPORTING
          msgid    = sy-msgid
          msgty    = sy-msgty
          msgno    = sy-msgno
          msgv1    = sy-msgv1
          msgv2    = sy-msgv2
          r_object = me.
    ENDIF.
  ENDMETHOD. "check_active

  METHOD check_inactive_transformation.
    DATA:
      lrx_root          TYPE REF TO cx_root,
      lrx_check         TYPE REF TO zcx_core_check,
      lt_exception      TYPE zcl_core_check_type=>gtyt_check_exp,
      lrx_check_msg     TYPE REF TO zcx_core_check_msg,
      lr_transformation TYPE REF TO zcl_core_check,
      lt_from           TYPE rso_t_tlogo_asc,
      lt_to             TYPE rso_t_tlogo_asc,
      ls_transformation TYPE rso_s_tlogo_asc.
    TRY.
        CALL METHOD nr_if_tlogo_general->get_related
          EXPORTING
            i_objvers             = rs_c_objvers-active
            i_tlogo               = rs_c_tlogo-transformation
          IMPORTING
            e_t_send_data_to      = lt_to
            e_t_receive_data_from = lt_from.
      CATCH cx_root INTO lrx_root.
        MESSAGE e008 WITH nv_tlogo nv_objnm INTO _message.
        RAISE EXCEPTION TYPE zcx_core_check
          EXPORTING
            previous = lrx_root
            msgid    = sy-msgid
            msgty    = sy-msgty
            msgno    = sy-msgno
            msgv1    = sy-msgv1
            msgv2    = sy-msgv2
            msgv3    = sy-msgv3
            msgv4    = sy-msgv4
            r_object = me.
    ENDTRY.
    APPEND LINES OF lt_to TO lt_from.
    LOOP AT lt_from INTO ls_transformation.
      TRY.
          CALL METHOD zcl_core_check=>factory(
              iv_tlogo        = rs_c_tlogo-dtp
              iv_objnm        = ls_transformation-objnm
              iv_in_transport = rs_c_false )->check_active( ).
        CATCH zcx_core_check INTO lrx_check.
          APPEND lrx_check TO lt_exception.
      ENDTRY.
    ENDLOOP.

    IF lt_exception IS NOT INITIAL.
      MESSAGE w024 WITH nv_objnm nv_tlogo INTO _message.
      CALL METHOD zcl_core_check_log=>raise_syst_t
        EXPORTING
          it_exception = lt_exception
          ir_object    = me.
    ENDIF.

  ENDMETHOD. "check_inactive_transformation

  METHOD check_inactive_dtp.
    DATA:
      lrx_root         TYPE REF TO cx_root,
      lrx_check        TYPE REF TO zcx_core_check,
      lt_exception     TYPE zcl_core_check_type=>gtyt_check_exp,
      lr_dtp           TYPE REF TO zcl_core_check,
      lt_from          TYPE rso_t_tlogo_asc,
      lt_to            TYPE rso_t_tlogo_asc,
      ls_dtp           TYPE rso_s_tlogo_asc,
      lr_tlogo_general TYPE REF TO if_rso_tlogo_general.

    TRY.
        CALL METHOD nr_if_tlogo_general->get_related
          EXPORTING
            i_objvers             = rs_c_objvers-active
            i_tlogo               = rs_c_tlogo-dtp
          IMPORTING
            e_t_send_data_to      = lt_to
            e_t_receive_data_from = lt_from.
      CATCH cx_root INTO lrx_root.
        MESSAGE e008 WITH nv_tlogo nv_objnm INTO _message.
        RAISE EXCEPTION TYPE zcx_core_check
          EXPORTING
            previous = lrx_root
            msgid    = sy-msgid
            msgty    = sy-msgty
            msgno    = sy-msgno
            msgv1    = sy-msgv1
            msgv2    = sy-msgv2
            msgv3    = sy-msgv3
            msgv4    = sy-msgv4
            r_object = me.
    ENDTRY.
    APPEND LINES OF lt_to TO lt_from.
    LOOP AT lt_from INTO ls_dtp.
      TRY.
          CALL METHOD zcl_core_check=>factory(
              iv_tlogo        = rs_c_tlogo-dtp
              iv_objnm        = ls_dtp-objnm
              iv_in_transport = rs_c_false )->check_active( ).
        CATCH zcx_core_check INTO lrx_check. "Expected.. not necesserily an error
          APPEND lrx_check TO lt_exception.
      ENDTRY.
    ENDLOOP.

    IF lt_exception IS NOT INITIAL.
      MESSAGE w023 WITH nv_objnm nv_tlogo INTO _message.
      CALL METHOD zcl_core_check_log=>raise_syst_t
        EXPORTING
          it_exception = lt_exception
          ir_object    = me.
    ENDIF.
  ENDMETHOD. "check_inactive_dtp

  METHOD check_name.
    DATA:
      lv_iobjnm TYPE rsiobjnm,
      lrx_check TYPE REF TO zcx_core_check.

    CHECK nv_no_name_check = rs_c_false.

    " I do think that '0' in the front means that no check
    " are to be carried out... but anyway

    DATA(lv_block) = zif_core_check~get_block( ).
    TRY.
        IF lv_block IS NOT INITIAL.
          CALL METHOD zcl_core_check_type=>check_block
            EXPORTING
              iv_block = zif_core_check~get_block( )
              ir_check = me.
          FREE lrx_check.
        ENDIF.
      CATCH zcx_core_check INTO lrx_check.
        " It falied the "start the naming with a block, test"
    ENDTRY.

    data lv_prefix(3) type c.
    select single iobjnm
        from rsdiobj
        where iobjnm  = @nv_iobjnm and
              objvers = @rs_c_objvers-active
        into @lv_iobjnm.
    if sy-subrc <> 0.
      " This is a new InfoObject
      " Make sure that the number is the next consequitive number within the
      " <cluster>CH
      " <cluster>KF
      lv_prefix = lv_iobjnm(3).
      lv_iobjnm = |{ lv_prefix }*|.
      Select max( iobjnm )
        from rsdiobj
        where iobjnm  like @lv_iobjnm and
              objvers = @rs_c_objvers-active
        into @lv_iobjnm.
      if sy-subrc = 0.
        shift lv_iobjnm left by 3 places.
        data lv_number(5) type n.
        lv_number = lv_iobjnm.
        lv_number = lv_number + 1.
        if nv_iobjnm <> |{ lv_prefix }{ lv_number }|.
          "- You have not chosen the next number in the line
          message e074 with lv_iobjnm nv_iobjnm into _message.
          RAISE EXCEPTION TYPE zcx_core_check
            EXPORTING
              msgid    = sy-msgid
              msgty    = sy-msgty
              msgno    = sy-msgno
              msgv1    = sy-msgv1
              msgv2    = sy-msgv2
              msgv3    = sy-msgv3
              msgv4    = sy-msgv4
              r_object = me.
        endif.
      endif.
    endif.

    IF lrx_check IS BOUND.
      " It failed both tests
      MESSAGE w052 WITH nv_objnm INTO _message.
      RAISE EXCEPTION TYPE zcx_core_check
        EXPORTING
          msgid    = sy-msgid
          msgty    = sy-msgty
          msgno    = sy-msgno
          msgv1    = sy-msgv1
          msgv2    = sy-msgv2
          msgv3    = sy-msgv3
          msgv4    = sy-msgv4
          r_object = me.
    ENDIF.
  ENDMETHOD. "check_name

  METHOD zif_core_check~get_block.
    rv_block = ''.
    IF nr_infoarea IS BOUND.
      rv_block = nr_infoarea->get_block( ).
    ENDIF.
  ENDMETHOD. "get_block

  METHOD zif_core_check~get_layer.
    rv_layer = zcl_core_check_type=>gc_layer-infoobject.
  ENDMETHOD. "GET_LAYER

  METHOD zif_core_check~get_section.
    rv_section = zcl_core_check_type=>gc_section-mda.
  ENDMETHOD. "GET_SECTION

  METHOD zif_core_check~get_cluster.
    if nv_iobjnm(1) = '0' or nv_iobjnm = '/'.  " delivered somehow.
      rv_cluster = 'X'.
    else.
      rv_cluster = nv_iobjnm(1).
    endif.
  ENDMETHOD.

ENDCLASS.
