CLASS zcl_core_check_void DEFINITION
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
      zif_core_check~get_section REDEFINITION,
      zif_core_check~get_cluster REDEFINITION,
      zif_core_check~check_active REDEFINITION,
      zif_core_check~get_layer REDEFINITION,
      zif_core_check~is_object_deleted REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_core_check_void IMPLEMENTATION.

  METHOD zif_core_check~check_before_release ##NEEDED.
  ENDMETHOD.
  METHOD zif_core_check~check_during_create ##NEEDED.
  ENDMETHOD.
  METHOD zif_core_check~do_load ##NEEDED.
  ENDMETHOD.
  METHOD zif_core_check~get_block ##NEEDED.
    rv_block = ''.
  ENDMETHOD.
  METHOD zif_core_check~get_section ##NEEDED.
    rv_section = ''.
  ENDMETHOD.
  METHOD zif_core_check~get_cluster ##NEEDED.
    rv_cluster = ''.
  ENDMETHOD.
  METHOD zif_core_check~check_active ##NEEDED.
  ENDMETHOD.
  METHOD zif_core_check~get_layer ##NEEDED.
    rv_layer = ''.
  ENDMETHOD.
  METHOD zif_core_check~is_object_deleted ##NEEDED.
    MESSAGE i058 WITH nv_obj_name nv_tlogo INTO _message.
    RAISE EXCEPTION TYPE zcx_core_check
      EXPORTING
        msgid = sy-msgid
        msgty = sy-msgty
        msgno = sy-msgno
        msgv1 = sy-msgv1
        msgv2 = sy-msgv2
        r_object = me.
ENDMETHOD.

ENDCLASS.
