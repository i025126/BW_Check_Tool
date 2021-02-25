INTERFACE zif_core_check
  PUBLIC .

  METHODS:
    is_object_deleted
      IMPORTING
        iv_check_msg TYPE string DEFAULT 'ZCX_CORE_CHECK_MSG'
      RAISING
        zcx_core_check,
    check_active
      RAISING
        zcx_core_check,
    get_cluster
      RETURNING
        VALUE(rv_cluster) type zcore_cluster,
    get_objnm
      RETURNING
        VALUE(rv_objnm) TYPE rsobjnm,
    get_tlogo
      RETURNING
        VALUE(rv_tlogo) TYPE rstlogo,
    get_block
      RETURNING
        VALUE(rv_block) TYPE zcore_block,
    get_layer
      RETURNING
        VALUE(rv_layer) TYPE zcore_layer,
    get_section
      RETURNING
        VALUE(rv_section) TYPE zcore_section
      RAISING
        zcx_core_check,
    check_during_create
      RETURNING
        VALUE(rv_error_count) TYPE i,
    check_before_release
      RAISING
        zcx_core_check,
    do_load
      RAISING
        zcx_core_check,
    check_name_general.

ENDINTERFACE.
