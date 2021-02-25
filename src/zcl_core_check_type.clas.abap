CLASS zcl_core_check_type DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      gtyv_value     TYPE char30,
      gtyv_param     TYPE char30,
      gtyr_check     type ref to zif_core_check,
      gtyr_check_exp TYPE REF TO zcx_core_check,
      gtyt_cluster   type STANDARD TABLE OF zcore_cluster with non-UNIQUE DEFAULT KEY,
      gtyt_section   type STANDARD TABLE OF zcore_section with non-UNIQUE DEFAULT KEY,
      gtyt_check_exp TYPE STANDARD TABLE OF gtyr_check_exp.

    CONSTANTS:
      BEGIN OF gc_layer,
        virtual        TYPE zcore_layer VALUE 'V',
        infoobject     TYPE zcore_layer VALUE 'I',
        reporting      TYPE zcore_layer VALUE 'R',
        transformation TYPE zcore_layer VALUE 'B',
        extraction     TYPE zcore_layer VALUE 'E',
        propagation    TYPE zcore_layer VALUE 'P',
        quality        TYPE zcore_layer VALUE 'Q',
        corporate      TYPE zcore_layer VALUE 'C',
        acquisition    TYPE zcore_layer VALUE 'A',
      END OF gc_layer.

    CONSTANTS:
      BEGIN OF gc_objecttype,
        wodso       TYPE zcore_objecttype VALUE 'W',
        directdso   TYPE zcore_objecttype VALUE 'U',
        infocube    TYPE zcore_objecttype VALUE 'C',
        infosource  TYPE zcore_objecttype VALUE 'I',
        destination TYPE zcore_objecttype VALUE 'O',
        dso         TYPE zcore_objecttype VALUE 'D',
        composite   TYPE zcore_objecttype VALUE 'M',
        openods     TYPE zcore_objecttype VALUE 'A',
        spo         TYPE zcore_objecttype VALUE 'X',
        datasource  TYPE zcore_objecttype VALUE 'S',
        infoobject  TYPE zcore_objecttype VALUE 'E',
      END OF gc_objecttype.

    CONSTANTS:
      BEGIN OF gc_section,
        mda TYPE zcore_section VALUE 'MDA',
        edw TYPE zcore_section VALUE 'EDW',
        iap TYPE zcore_section VALUE 'IAP',
        exz TYPE zcore_section VALUE 'EXZ',
        aav TYPE zcore_section VALUE 'AAV',
        sys TYPE zcore_section VALUE 'SYS',
      END OF gc_section.

    CONSTANTS:
      gv_root type rsinfoarea value 'ROOT',
      BEGIN OF gc_var_for_what,
        formula        TYPE c VALUE 'F',
        hierarchy      TYPE c VALUE 'H',
        text           TYPE c VALUE 'T',
        characteristic TYPE c VALUE 'C',
      END OF gc_var_for_what,
      BEGIN OF gc_var_type,
        authorization TYPE c VALUE 'A',
        customerexit  TYPE c VALUE 'E',
        replacement   TYPE c VALUE 'R',
        sapexit       TYPE c VALUE 'S',
        userentry     TYPE c VALUE 'U',
        brf           TYPE c VALUE 'B',
      END OF gc_var_type,
      BEGIN OF gc_var_option,
        single        TYPE c VALUE 'S',
        multiple      TYPE c VALUE 'M',
        interval      TYPE c VALUE 'I',
        option        TYPE c VALUE 'O',
        precalculated TYPE c VALUE 'P',
      END OF gc_var_option.

    class-data:
      _message type string.

    CLASS-METHODS:
      get_all_cluster
        RETURNING
          VALUE(rt_cluster) type gtyt_cluster,
      get_all_section  " and ROOT
        IMPORTING
          iv_cluster        type zcore_cluster
        RETURNING
          VALUE(rt_Section) type gtyt_section,
      check_cluster
        IMPORTING
          ir_check    TYPE REF TO zif_core_check
        RAISING
          zcx_core_check,
      check_constant
        IMPORTING
          iv_value    TYPE clike
          is_constant TYPE any
          ir_check    TYPE REF TO zif_core_check
        RAISING
          zcx_core_check,
      "!Description: Get a parameter value
      get_parameter_value
        IMPORTING
          iv_parameter    TYPE gtyv_param
        RETURNING
          VALUE(rv_value) TYPE gtyv_value,
      "! Description: Is the block correctly placed in an inforarea
      infoarea_to_block
        IMPORTING
          iv_infoarea     TYPE rsinfoarea
        RETURNING
          VALUE(rv_block) TYPE zcore_block,
      "!Description: Check values of layer
      check_layer
        IMPORTING
          iv_section TYPE zcore_section
          iv_layer   TYPE zcore_layer
          ir_check   TYPE REF TO zif_core_check
        RAISING
          zcx_core_check,
      "! Description: Check the object type and adress
      check_object_type
        IMPORTING
          iv_tlogo      TYPE rstlogo
          iv_objecttype TYPE zcore_objecttype
          ir_check      TYPE REF TO zif_core_check
        RAISING
          zcx_core_check,
      "!Description: Check the block
      check_block
        IMPORTING
          iv_block   TYPE zcore_block OPTIONAL
          ir_check   TYPE REF TO zif_core_check OPTIONAL
          PREFERRED PARAMETER ir_check
        RAISING
          zcx_core_check,
      "! Return the cluster of the block
      "! @parameter iv_block   | Block that is used to find the cluster for
      "! @parameter rv_cluster | Cluster of the block given
      get_cluster_from_block
        IMPORTING
          iv_block type zcore_block OPTIONAL
          ir_check TYPE REF TO zif_core_check OPTIONAL
          PREFERRED PARAMETER ir_check
        returning
          value(rv_cluster) type zcore_cluster
        RAISING
          zcx_core_check,
      get_void
        RETURNING
          VALUE(rr_void) type ref to zif_core_check.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_core_check_type IMPLEMENTATION.

  METHOD get_all_cluster.
    select doccluster
        from zi_core_cluster
        into table @rt_cluster.
  ENDMETHOD.

  METHOD get_all_section.
*===Get the data from the domain===
* In theory, this could be a helper function
* reusable for all these type of calls
  data:
    l_t_dom_val    TYPE STANDARD TABLE OF dd07v.

  CALL FUNCTION 'DDUT_DOMVALUES_GET'
    EXPORTING
      name          = 'ZCORE_SECTIO'
      langu         = sy-langu
    TABLES
      dd07v_tab     = l_t_dom_val
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  assert sy-subrc = 0.

*===Append the values in e_t_chavlinfo===
  LOOP AT l_t_dom_val INTO data(l_s_dom_val).
    append INITIAL LINE TO rt_section ASSIGNING FIELD-SYMBOL(<lv_section>).
    <lv_section> = |{ l_s_dom_val-domvalue_l }{ iv_cluster }|.
  ENDLOOP.

  ENDMETHOD.



  METHOD get_void.
    try.
        rr_void = zcl_core_check=>factory( iv_tlogo = 'VOID' iv_objnm = '' ).
      CATCH zcx_core_check.
    endtry.
  ENDMETHOD.

  METHOD check_constant.
    DATA lv_string TYPE string.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE is_constant TO FIELD-SYMBOL(<lv_value>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      IF iv_value = <lv_value>.
        CLEAR lv_string.
        EXIT.
      ENDIF.
      lv_string = |{ lv_string }, { <lv_value> }|.
    ENDDO.
    IF lv_string IS NOT INITIAL.
** No match where found - Issue an error... the next line is to get rid of the >, <
      SHIFT lv_string LEFT BY 2 PLACES.
      MESSAGE e042 WITH iv_value lv_string ir_check->get_objnm(  ) ir_check->get_tlogo(  ) into _message.
      CALL METHOD zcl_core_check_log=>raise_syst( ir_check ).
    ENDIF.
  ENDMETHOD. "CHECK_CONSTANT

  METHOD get_parameter_value.

    "" Need to find out how we can get a parameter value
    rv_value = ''.

  ENDMETHOD. "GET_PARAMETER_VALUE

  METHOD infoarea_to_block.

    select single docblock
        from zi_core_contentview
        where infoarea = @iv_infoarea
        into @rv_block.
    if sy-subrc <> 0.
      clear rv_block.
    endif.
    IF strlen( iv_infoarea ) = 11.
      MESSAGE i069(zcore_check) WITH iv_infoarea.
    ENDIF.

  ENDMETHOD. "INFOAREA_TO_BLOCK

  METHOD check_cluster.
    " Either the cluster is '0' or 'X' or the same as the block
    if ir_check->get_cluster(  ) <> 'X'.
      if ir_check->get_cluster( ) <> zcl_core_check_type=>get_cluster_from_block( ir_check ) .
        assert 1 = 0.
      endif.
    endif.

  ENDMETHOD.

  METHOD check_layer.
    DATA lv_allowed TYPE string.
    CASE iv_section.
      WHEN gc_section-iap.
        lv_allowed = gc_layer-transformation.
      WHEN gc_section-aav.
        lv_allowed = gc_layer-virtual.
      WHEN gc_section-exz.
        lv_allowed = gc_layer-extraction.
      WHEN gc_section-edw.
        lv_allowed = |{ gc_layer-propagation }{ gc_layer-quality }{ gc_layer-acquisition }{ gc_layer-corporate }|.
      WHEN gc_section-mda.
        lv_allowed = gc_layer-infoobject.
      WHEN OTHERS.
        MESSAGE e003 WITH iv_section INTO _message.
        CALL METHOD zcl_core_check_log=>raise_syst( ir_check ).
    ENDCASE.
    IF lv_allowed NA iv_layer.
      MESSAGE e004 WITH iv_section iv_layer lv_allowed INTO _message.
      CALL METHOD zcl_core_check_log=>raise_syst( ir_check ).
    ENDIF.
  ENDMETHOD. "CHECK_LAYER

  METHOD check_object_type.
    DATA lv_allowed TYPE string.
    " Concept is to check the that the object type given to the
    " TLOGO is allowed to be used... so in Composite Providers can only be used
    " for composite providers
*        wodso       TYPE zcore_objecttype VALUE 'W', ==> only for ADSO
*        directdso   TYPE zcore_objecttype VALUE 'U', ==> only for aDSO
*        infocube    TYPE zcore_objecttype VALUE 'C', ==> only for aDSO
*        infosource  TYPE zcore_objecttype VALUE 'I', ==> only for InfoSource
*        destination TYPE zcore_objecttype VALUE 'O', ==> only for Openhub
*        dso         TYPE zcore_objecttype VALUE 'D', ==> only for aDSO
*        composite   TYPE zcore_objecttype VALUE 'M', ==> only for Composite provider
*        openods     TYPE zcore_objecttype VALUE 'A', ==> only for OpenOds View
*        spo         TYPE zcore_objecttype VALUE 'X', ==> only for SPO's
*        datasource  TYPE zcore_objecttype VALUE 'S', ==> only for DataSource
*        infoobject  TYPE zcore_objecttype VALUE 'E', ==> ONly for aDSO
    CASE iv_tlogo.
      WHEN rs_c_tlogo-adso.
        lv_allowed = |{ gc_objecttype-wodso }{ gc_objecttype-directdso }{ gc_objecttype-infocube }{ gc_objecttype-dso }{ gc_objecttype-infoobject }|.
      when rs_c_tlogo-log_partitioned_obj.
        lv_allowed = gc_objecttype-spo.
      when rs_c_tlogo-datasource.
        lv_allowed = gc_objecttype-datasource.
      WHEN rs_c_tlogo-new_comm_structure.
        lv_allowed = gc_objecttype-infosource.
      WHEN rs_c_tlogo-destination.
        lv_allowed = gc_objecttype-destination.
      when rs_c_tlogo-copr.
        lv_allowed = gc_objecttype-composite.
      when rs_c_tlogo-hcpr.
        lv_allowed = gc_objecttype-composite.
      when rs_c_tlogo-field_based_provider.
        lv_allowed = gc_objecttype-openods.
      when OTHERS .
        clear lv_allowed.
    endcase.

    CHECK lv_allowed IS NOT INITIAL.
    IF lv_allowed NA iv_objecttype.
      MESSAGE e005 WITH iv_tlogo iv_objecttype lv_allowed INTO _message.
      CALL METHOD zcl_core_check_log=>raise_syst( ir_check ).
    ENDIF.

  ENDMETHOD. "CHECK_OBJECT_TYPE

  METHOD get_cluster_from_block.

    " This will ensure that the block is known in the system
    if iv_block is not SUPPLIED.
      data(lv_block) = ir_check->get_block(  ).
    else.
      lv_block = iv_block.
    endif.

    call METHOD check_block( iv_block = lv_block ir_check = ir_check ).

    " If it is know.. there is a cluster
    select single DocCluster
      from zi_core_contentview
      where docblock = @lv_block
      into @rv_cluster.

    if sy-subrc <> 0.
      " Or the block
      rv_cluster = ''.
    endif.

  ENDMETHOD.

  METHOD check_block.

    if iv_block is not SUPPLIED.
      data(lv_block) = ir_check->get_block(  ).
    else.
      lv_block = iv_block.
    endif.

    select single docblock
        from zi_core_contentview
        where DocBlock = @lv_block
        into @lv_block.

    if sy-subrc <> 0.
      MESSAGE e001 WITH iv_block INTO _message.
      RAISE EXCEPTION TYPE zcx_core_check
        EXPORTING
          msgid    = sy-msgid
          msgty    = sy-msgty
          msgno    = sy-msgno
          msgv1    = sy-msgv1
          msgv2    = sy-msgv2
          msgv3    = sy-msgv3
          msgv4    = sy-msgv4
          r_object = ir_check.
    ENDIF.

  ENDMETHOD. "CHECK_BLOCK

ENDCLASS.
