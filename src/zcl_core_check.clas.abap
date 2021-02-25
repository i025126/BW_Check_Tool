CLASS zcl_core_check DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_core_check .

    TYPES:
      BEGIN OF gtys_buffer,
        tlogo       TYPE rstlogo,
        objnm       TYPE trobj_name,
        r_check     TYPE REF TO zif_core_check,
        r_exception TYPE REF TO zcx_core_check,
      END OF gtys_buffer .
    TYPES:
      gtyth_buffer TYPE HASHED TABLE OF gtys_buffer WITH UNIQUE KEY tlogo objnm .
    TYPES:
      BEGIN OF gtys_buffer_type,
        tlogo    TYPE rstlogo,
        objnm    TYPE rsobjnm,
        obj_name TYPE trobj_name,
      END OF gtys_buffer_type .
    TYPES:
      gtyth_buffer_type TYPE HASHED TABLE OF gtys_buffer WITH UNIQUE KEY tlogo objnm .

    CLASS-DATA gr_void TYPE REF TO zif_core_check .
    CLASS-DATA gv_forward TYPE rfcdest .
    DATA nv_obj_name TYPE trobj_name .
    DATA nv_tlogo TYPE rstlogo .
    DATA nv_objnm TYPE rsobjnm .

    CONSTANTS:
      begin of gc_text,
        check_release TYPE string VALUE 'CHECK_BEFORE_RELEASE' ##NO_TEXT,
      end of gc_text.
    CLASS-DATA _message TYPE string .
    CLASS-DATA gv_logstatus TYPE rs_bool .
    DATA nr_maintain TYPE REF TO if_rso_tlogo_maintain .
    DATA nr_if_tlogo_maintain TYPE REF TO if_rso_tlogo_maintain .
    DATA nv_deleted TYPE rs_bool VALUE rs_c_true ##NO_TEXT.
    DATA nr_if_tlogo_general TYPE REF TO if_rso_tlogo_general .
    DATA nv_no_name_check TYPE rs_bool .

    CLASS-METHODS class_constructor .
    CLASS-METHODS factory
      IMPORTING
        !iv_tlogo        TYPE trobjtype
        !iv_objnm        TYPE clike
        !iv_in_transport TYPE rs_bool DEFAULT rs_c_true
      RETURNING
        VALUE(rr_check)  TYPE REF TO zif_core_check
      RAISING
        zcx_core_check.
    METHODS constructor
      IMPORTING
        !iv_tlogo TYPE rstlogo
        !iv_objnm TYPE trobj_name
      EXCEPTIONS
        zcx_core_check.

  protected section .
    METHODS check_name_general.
    METHODS check_standard
      RAISING
        zcx_core_check .

  PRIVATE SECTION.

    CLASS-DATA:
      gth_buffer       TYPE gtyth_buffer,
      gth_in_transport TYPE gtyth_buffer_type.

ENDCLASS.

CLASS zcl_core_check IMPLEMENTATION.

  METHOD class_constructor.
    TRY.
        CREATE OBJECT gr_void TYPE zcl_core_check
          EXPORTING
            iv_tlogo = 'VOID'
            iv_objnm = ''.
      CATCH zcx_core_check ##no_handler.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    nv_tlogo = iv_tlogo.
    nv_objnm = iv_objnm.
    nv_obj_name = iv_objnm.
  ENDMETHOD.

  METHOD factory.
    " Creates an instance of the object that must be checked
    " it also add it to a buffer table in order to avoid creating
    " multiple entries

    DATA:
      ls_buffer_type TYPE gtys_buffer_type,
      ls_buffer      TYPE gtys_buffer,
      lv_class_name  TYPE text72,
      lrx_create     TYPE REF TO cx_root,
      lr_check       TYPE REF TO object.

    ls_buffer-tlogo = iv_tlogo.
    ls_buffer-objnm = iv_objnm.
    READ TABLE gth_buffer ASSIGNING FIELD-SYMBOL(<ls_buffer>)
           FROM ls_buffer.
    IF sy-subrc <> 0.
      TRY.
          TRY.
              lv_class_name = |ZCL_CORE_CHECK_{ iv_tlogo }|.
              CREATE OBJECT lr_check TYPE (lv_class_name)
                EXPORTING
                  iv_tlogo = ls_buffer-tlogo
                  iv_objnm = ls_buffer-objnm.

              ls_buffer-r_check ?= lr_check.

              CALL METHOD ls_buffer-r_check->do_load.
              CALL METHOD ls_buffer-r_check->check_name_general.

              INSERT ls_buffer INTO TABLE gth_buffer ASSIGNING <ls_buffer>.

            CATCH zcx_core_check INTO ls_buffer-r_exception.
              CLEAR ls_buffer-r_check. "Reset to make the error visible
              INSERT ls_buffer INTO TABLE gth_buffer ASSIGNING <ls_buffer>.

            CATCH cx_sy_dyn_call_illegal_class
                  cx_sy_create_error.
              " Default to a class that does nothing
              " This is to handle what ever is not implemented
              lv_class_name = 'ZCL_CORE_CHECK_VOID'.
              CREATE OBJECT lr_check TYPE (lv_class_name)
                EXPORTING
                  iv_tlogo = ls_buffer-tlogo
                  iv_objnm = ls_buffer-objnm.

              ls_buffer-r_check ?= lr_check.
              INSERT ls_buffer INTO TABLE gth_buffer ASSIGNING <ls_buffer>.
          ENDTRY.
        CATCH cx_sy_dyn_call_error INTO lrx_create.
          MESSAGE e002 WITH iv_tlogo iv_objnm INTO _message.
          RAISE EXCEPTION TYPE zcx_core_check
            EXPORTING
              previous = lrx_create
              msgid    = sy-msgid
              msgty    = sy-msgty
              msgno    = sy-msgno
              msgv1    = sy-msgv1
              msgv2    = sy-msgv2
              r_object = gr_void.
      ENDTRY.
    ENDIF.

    IF <ls_buffer>-r_check IS NOT BOUND.
      IF <ls_buffer>-r_exception IS BOUND.
        RAISE EXCEPTION <ls_buffer>-r_exception.
      ENDIF.
    ELSE.
      rr_check = <ls_buffer>-r_check.
    ENDIF.
  ENDMETHOD.

  METHOD check_name_general.
    DATA:
    lv_namespace TYPE namespace.
    " do not include check of name of the name is initial
    " This is mainly for the object that have no class, but
    " also for deleted object etc...
    nv_no_name_check = rs_c_true.
    CHECK nv_objnm IS NOT INITIAL.
    CALL FUNCTION 'RSD_NSPACE_PAR_GET_FROM_NAME'
      EXPORTING
        i_objnm     = nv_objnm
      IMPORTING
        e_namespace = lv_namespace
      EXCEPTIONS
        name_error  = 1
        OTHERS      = 2.
    ASSERT sy-subrc = 0.
    IF lv_namespace IS NOT INITIAL OR
    " No name check for application component not starting
    " with 'Z'
    ( nv_tlogo = rs_c_tlogo-application AND
    nv_objnm(1) <> 'Z' ).
      nv_no_name_check = rs_c_true.
    ELSE.
      nv_no_name_check = rs_c_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_standard.
    DATA:
      lr_msg_log   TYPE REF TO cl_rso_msg,
      lrx_check    TYPE REF TO zcx_core_check,
      lv_msgty     TYPE msgty,
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp,
      lt_msg       TYPE rs_t_msg,
      ls_msg       TYPE rs_s_msg.
    CHECK nr_maintain IS BOUND.
** Run the standard check
    CALL METHOD nr_maintain->check
      EXPORTING
        i_objvers = rs_c_objvers-active
      IMPORTING
        e_r_msg   = lr_msg_log.
    CHECK lr_msg_log IS BOUND.
    lt_msg = lr_msg_log->get_all_msg( ).
    DELETE lt_msg
    WHERE NOT ( msgty = rs_c_error OR msgty = rs_c_warning ).
    IF lines( lt_msg ) > 0.
      lv_msgty = rs_c_warning.
      LOOP AT lt_msg INTO ls_msg.
        CREATE OBJECT lrx_check
          EXPORTING
            msgid    = ls_msg-msgid
            msgty    = ls_msg-msgty
            msgno    = ls_msg-msgno
            msgv1    = ls_msg-msgv1
            msgv2    = ls_msg-msgv2
            msgv3    = ls_msg-msgv3
            msgv4    = ls_msg-msgv4
            r_object = me.
        APPEND lrx_check TO lt_exception.
        IF lv_msgty = rs_c_warning AND ls_msg-msgty = rs_c_error.
          lv_msgty = rs_c_error.
        ENDIF.
      ENDLOOP.
      MESSAGE i028 WITH nv_objnm nv_tlogo INTO _message.
      RAISE EXCEPTION TYPE zcx_core_check
        EXPORTING
          msgid       = sy-msgid
          msgty       = sy-msgty
          msgno       = sy-msgno
          msgv1       = sy-msgv1
          msgv2       = sy-msgv2
          msgv3       = sy-msgv3
          msgv4       = sy-msgv4
          t_exception = lt_exception
          r_object    = me.
    ENDIF.
  ENDMETHOD.

  METHOD zif_core_check~is_object_deleted.
  DATA lrx_check TYPE REF TO ZCX_CORE_CHECK.

    IF nv_deleted = rs_c_true.
      MESSAGE i036 WITH nv_objnm nv_tlogo INTO _message.
      CREATE OBJECT lrx_check TYPE (iv_check_msg)
        EXPORTING
          msgid = sy-msgid
          msgty = sy-msgty
          msgno = sy-msgno
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4
          r_object = me.
      RAISE EXCEPTION lrx_check.
    ENDIF.
  ENDMETHOD.

  METHOD zif_core_check~check_active ##NEEDED.
  ENDMETHOD.

  METHOD zif_core_check~check_before_release ##NEEDED.
  ENDMETHOD.

  METHOD zif_core_check~check_during_create ##NEEDED.
  ENDMETHOD.

  METHOD zif_core_check~check_name_general.
    DATA:
    lv_namespace TYPE namespace.
    " do not include check of name of the name is initial
    " This is mainly for the object that have no class, but
    " also for deleted object etc...
    nv_no_name_check = rs_c_true.
    CHECK nv_objnm IS NOT INITIAL.
    CALL FUNCTION 'RSD_NSPACE_PAR_GET_FROM_NAME'
      EXPORTING
        i_objnm     = nv_objnm
      IMPORTING
        e_namespace = lv_namespace
      EXCEPTIONS
        name_error  = 1
        OTHERS      = 2.
    ASSERT sy-subrc = 0.
    IF lv_namespace IS NOT INITIAL OR
    " No name check for application component not starting
    " with 'Z'
    ( nv_tlogo = rs_c_tlogo-application AND
    nv_objnm(1) <> 'Z' ).
      nv_no_name_check = rs_c_true.
    ELSE.
      nv_no_name_check = rs_c_false.
    ENDIF.
  ENDMETHOD.

  METHOD zif_core_check~get_objnm.
    rv_objnm = nv_objnm.
  ENDMETHOD.

  METHOD zif_core_check~get_tlogo.
    rv_tlogo = nv_tlogo.
  ENDMETHOD.

  METHOD zif_core_check~do_load ##NEEDED.
  ENDMETHOD.

  METHOD zif_core_check~get_block.
    rv_block = nv_objnm(4).
  ENDMETHOD.

  METHOD zif_core_check~get_layer.
    rv_layer = nv_objnm+4(1).
  ENDMETHOD.

  METHOD zif_core_check~get_cluster.
    data(lv_block) = zif_core_check~get_block(  ).
    rv_cluster = lv_block(1).
  ENDMETHOD.

  METHOD zif_core_check~get_section ##NEEDED.
  ENDMETHOD.
ENDCLASS.
