CLASS zcl_core_check_transport DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gtys_check,
        tlogo           TYPE trobjtype,
        objnm           TYPE trobj_name,
        r_check_obj     TYPE REF TO zif_core_check,
        rx_check        TYPE REF TO zcx_core_check,
        rx_check_before TYPE REF TO zcx_core_check,
      END OF gtys_check,
      gtyth_check TYPE HASHED TABLE OF gtys_check WITH UNIQUE KEY tlogo objnm.


    CLASS-DATA:
      _message TYPE string.

    DATA:
      nv_add_all TYPE rs_bool VALUE rs_c_false,
      nv_request TYPE trkorr,
      nr_log     TYPE REF TO zcl_core_check_log,
      nth_check  TYPE gtyth_check.

    CLASS-METHODS:
      is_request_approved
        IMPORTING
          iv_request         TYPE trkorr
        RETURNING
          VALUE(rv_approved) TYPE rs_bool.

    METHODS tr_prepare
      IMPORTING
        request TYPE trkorr
        type    TYPE trfunction
        objects TYPE tr_objects.

    METHODS tr_check
      IMPORTING
        request TYPE trkorr
        type    TYPE trfunction
        objects TYPE tr_objects.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      nt_exception TYPE zcl_core_check_type=>gtyt_check_exp,
      nv_type      TYPE trfunction.
ENDCLASS.


CLASS zcl_core_check_transport IMPLEMENTATION.

  METHOD tr_check.
    TYPES:
      BEGIN OF ltys_blocks,
        objnm    TYPE rsobjnm,
        tlogo    TYPE rstlogo,
        block    TYPE zcore_block,
        r_object TYPE REF TO zif_core_check,
      END OF ltys_blocks,
      ltyth_blocks TYPE HASHED TABLE OF ltys_blocks WITH UNIQUE KEY block.
    DATA:
      ls_blocks    TYPE ltys_blocks,
      lth_blocks   TYPE ltyth_blocks,
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp,
      lrx_check    TYPE REF TO zcx_core_check,
      ls_check     TYPE gtys_check.
    LOOP AT me->nth_check INTO ls_check.

      REFRESH lt_exception.
      TRY.
          IF ls_check-rx_check IS BOUND.
            " Errors already found under DO_LOAD
            " and must be placed correctly under the right node
            " no further check to be performed
            APPEND ls_check-rx_check TO lt_exception.
          ELSE.
** Do not do any check if the object is deleted
            CALL METHOD ls_check-r_check_obj->is_object_deleted( 'ZCX_CORE_CHECK_MSG' ).
** This is part of the Transport Request/Task integrity test
            TRY.
** Check consistency of the TR with regards to blocks
                ls_blocks-objnm = ls_check-objnm.
                ls_blocks-tlogo = ls_check-tlogo.
                ls_blocks-block = ls_check-r_check_obj->get_block( ).
                ls_blocks-r_object = ls_check-r_check_obj.
                " This is to piggyback in the message exception framework
                " This way certain objects can be omitted from block
                " check
                MESSAGE e056(zcore_check) WITH ls_blocks-objnm ls_blocks-tlogo
                ls_blocks-block INTO _message.
                CREATE OBJECT lrx_check
                  EXPORTING
                    msgid    = sy-msgid
                    msgty    = sy-msgty
                    msgno    = sy-msgno
                    msgv1    = sy-msgv1
                    msgv2    = sy-msgv2
                    msgv3    = sy-msgv3
                    msgv4    = sy-msgv4
                    r_object = ls_blocks-r_object.
                IF nr_log->get_overwrite( lrx_check ) <> rs_c_info.
                  INSERT ls_blocks INTO TABLE lth_blocks.
                ENDIF.
                FREE lrx_check.
              CATCH zcx_core_check.
            ENDTRY.
** This is the two test of the object within the task/request
            TRY.
                CALL METHOD ls_check-r_check_obj->check_before_release.
              CATCH zcx_core_check INTO ls_check-rx_check_before.
                APPEND ls_check-rx_check_before TO lt_exception.
            ENDTRY.
          ENDIF.
          IF lines( lt_exception ) > 0 OR nv_add_all = rs_c_true.
** Depending on the number of exception encountered and the setting
** pick up the exception and post a suitiable messages with or without
** exception
            TRY.
                IF lt_exception IS INITIAL.
                  MESSAGE i030(zcore_check) WITH ls_check-objnm ls_check-tlogo INTO
                  _message.
                  CALL METHOD zcl_core_check_log=>raise_syst( ls_check-r_check_obj ).
                ELSE.
                  MESSAGE e030(zcore_check) WITH ls_check-objnm ls_check-tlogo INTO
                  _message.
                  CALL METHOD zcl_core_check_log=>raise_syst_t
                    EXPORTING
                      it_exception = lt_exception
                      ir_object    = ls_check-r_check_obj.
                ENDIF.
              CATCH zcx_core_check INTO lrx_check.
                APPEND lrx_check TO nt_exception.
            ENDTRY.
          ENDIF.
        CATCH zcx_core_check INTO lrx_check.
          " Catch the "Object deleted"
          IF nv_add_all = rs_c_true.
            APPEND lrx_check TO nt_exception.
          ENDIF.
      ENDTRY.
    ENDLOOP.
** Check if more than one Block is transported in the transport
    DELETE lth_blocks WHERE block IS INITIAL.
    DATA:
      lv_msgty    TYPE sy-msgty.
    lv_msgty = rs_c_warning.

    IF lines( lth_blocks ) > 1.
      REFRESH lt_exception.
      LOOP AT lth_blocks INTO ls_blocks.
        MESSAGE ID 'ZCORE_CHECK' TYPE lv_msgty NUMBER '056'
        WITH ls_blocks-objnm ls_blocks-tlogo ls_blocks-block INTO _message.
        CREATE OBJECT lrx_check
          EXPORTING
            msgid    = sy-msgid
            msgty    = sy-msgty
            msgno    = sy-msgno
            msgv1    = sy-msgv1
            msgv2    = sy-msgv2
            msgv3    = sy-msgv3
            msgv4    = sy-msgv4
            r_object = ls_blocks-r_object.
        APPEND lrx_check TO lt_exception.
      ENDLOOP.
      MESSAGE ID 'ZCORE_CHECK' TYPE lv_msgty NUMBER '029'
      WITH request INTO _message.
      CREATE OBJECT lrx_check
        EXPORTING
          msgid       = sy-msgid
          msgty       = sy-msgty
          msgno       = sy-msgno
          msgv1       = sy-msgv1
          msgv2       = sy-msgv2
          msgv3       = sy-msgv3
          msgv4       = sy-msgv4
          t_exception = lt_exception
          r_object    = zcl_core_check=>gr_void.
** I know insert sucks, but i rather have this in the begining
      INSERT lrx_check INTO nt_exception INDEX 1.
    ENDIF.
    IF nt_exception IS NOT INITIAL.
      CALL METHOD zcl_core_check_log=>get_msgty
        EXPORTING
          it_exception = nt_exception
        IMPORTING
          ev_msgty     = lv_msgty.
** Add the information that this has been checked
      MESSAGE ID 'ZCORE_CHECK' TYPE lv_msgty NUMBER '031'
      WITH request INTO _message.
      CREATE OBJECT lrx_check
        EXPORTING
          msgid       = sy-msgid
          msgty       = sy-msgty
          msgno       = sy-msgno
          msgv1       = sy-msgv1
          msgv2       = sy-msgv2
          msgv3       = sy-msgv3
          msgv4       = sy-msgv4
          t_exception = nt_exception
          r_object    = zcl_core_check=>gr_void.
    ELSE.
      MESSAGE i031(zcore_check) WITH request INTO _message.
      CREATE OBJECT lrx_check
        EXPORTING
          msgid    = sy-msgid
          msgty    = sy-msgty
          msgno    = sy-msgno
          msgv1    = sy-msgv1
          msgv2    = sy-msgv2
          msgv3    = sy-msgv3
          msgv4    = sy-msgv4
          r_object = zcl_core_check=>gr_void.
    ENDIF.
    IF lrx_check IS BOUND.
      CALL METHOD nr_log->add_exception( lrx_check ).
    ENDIF.
  ENDMETHOD.

  METHOD tr_prepare.

    DATA:
      ls_check   TYPE gtys_check,
      ls_objects TYPE e071.

    nv_request = request.
    nv_type = type.
    IF nr_log IS INITIAL.
      CREATE OBJECT nr_log
        EXPORTING
          iv_text = nv_request.
    ENDIF.

    LOOP AT objects INTO ls_objects.
      TRY.
          ls_check-tlogo = ls_objects-object.
          ls_check-objnm = ls_objects-obj_name.
          ls_check-r_check_obj = zcl_core_check=>factory( iv_objnm = ls_check-objnm
                                                          iv_tlogo = ls_check-tlogo ).
        CATCH zcx_core_check INTO ls_check-rx_check.
      ENDTRY.
      INSERT ls_check INTO TABLE nth_check.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_request_approved.

    DATA lv_request TYPE trkorr.

    rv_approved = rs_c_false.
    SELECT SINGLE request
        FROM ('ZCORE_APPROVAL')
        WHERE request = @iv_request
        INTO @lv_request.
    IF sy-subrc = 0.
      rv_approved = rs_c_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
