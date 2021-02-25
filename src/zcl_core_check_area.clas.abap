CLASS zcl_core_check_area DEFINITION
  PUBLIC
  INHERITING FROM zcl_core_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_rso_tlogo_general.

    METHODS:
      zif_core_check~check_before_release REDEFINITION,
      zif_core_check~check_during_create REDEFINITION,
      zif_core_check~do_load REDEFINITION,
      zif_core_check~get_block REDEFINITION,
      zif_core_check~get_section REDEFINITION,
      zif_core_check~get_cluster REDEFINITION.

    DATA:
      nv_infoarea TYPE rsinfoarea,
      ns_infoarea TYPE rsdarea.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS:
      check_structure_infoarea
        IMPORTING
          iv_infoarea TYPE rsobjnm
        RAISING
          cx_rs_fcst_univariate,
      check_name
        RAISING
          zcx_core_check,
      check_infoarea
        RAISING
          zcx_core_check.

ENDCLASS.



CLASS zcl_core_check_area IMPLEMENTATION.

  METHOD zif_core_check~check_during_create.
    TRY.
        CALL METHOD check_name.
      CATCH zcx_core_check INTO DATA(lrx_check).
        CALL METHOD zcl_core_check_log=>do_display_exception( lrx_check ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_core_check~check_before_release.

    DATA:
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp.

    TRY.
        CALL METHOD check_name.
      CATCH zcx_core_check INTO DATA(lrx_check).
        APPEND lrx_check TO lt_exception.
    ENDTRY.
    TRY.
        CALL METHOD check_infoarea.
      CATCH zcx_core_check INTO lrx_check.
        APPEND lrx_check TO lt_exception.
    ENDTRY.

    IF lines( lt_exception ) > 0.
      MESSAGE e025 WITH gc_text-check_release nv_objnm nv_tlogo INTO _message.
      RAISE EXCEPTION TYPE zcx_core_check
        EXPORTING
          msgid       = sy-msgid
          msgty       = sy-msgty
          msgno       = sy-msgno
          msgv1       = sy-msgv1
          msgv2       = sy-msgv2
          msgv3       = sy-msgv3
          t_exception = lt_exception
          r_object    = me.
    ENDIF.
  ENDMETHOD.

  METHOD check_infoarea.

    CHECK nv_no_name_check = rs_c_false.

    DATA:
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp.

    TRY.
        LOOP AT zcl_core_check_type=>get_all_cluster(  ) INTO DATA(lv_cluster).
          IF nv_objnm = lv_cluster.
            " Root infoArea
            IF ns_infoarea-infoarea_p IS NOT INITIAL.
              MESSAGE e071 WITH nv_infoarea ns_infoarea-infoarea_p INTO _message.
              RAISE RESUMABLE EXCEPTION TYPE zcx_core_check
                EXPORTING
                  msgid    = sy-msgid
                  msgty    = sy-msgty
                  msgno    = sy-msgno
                  msgv1    = sy-msgv1
                  msgv2    = sy-msgv1
                  r_object = me.
            ENDIF.
            " One error and you are gome
            RAISE EXCEPTION TYPE cx_rs_fcst_univariate.
          ENDIF.

          LOOP AT zcl_core_check_type=>get_all_section( lv_cluster ) INTO DATA(lv_section).
            IF nv_objnm = lv_section.
              " Technical InfoArea of a section in a cluster
              IF ns_infoarea-infoarea_p <> lv_cluster.
                MESSAGE e072 WITH nv_infoarea lv_cluster ns_infoarea-infoarea_p INTO _message.
                RAISE RESUMABLE EXCEPTION TYPE zcx_core_check
                  EXPORTING
                    msgid    = sy-msgid
                    msgty    = sy-msgty
                    msgno    = sy-msgno
                    msgv1    = sy-msgv1
                    msgv2    = sy-msgv1
                    r_object = me.
              ENDIF.
              " One error and you are gome
              RAISE EXCEPTION TYPE cx_rs_fcst_univariate.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        " If we got this far... this is a block outside of the structure
        " If we got this far... this is a block outside of the structure
        TRY.
            zcl_core_check_type=>check_block( me ).
          CATCH zcx_core_check INTO DATA(lrx_check).
            APPEND lrx_check TO lt_exception.
        ENDTRY.

        TRY.
            zcl_core_check_type=>check_cluster( me ).
          CATCH zcx_core_check INTO lrx_check.
            APPEND lrx_check TO lt_exception.
        ENDTRY.

        DATA(lr_parent) = zcl_core_check=>factory( iv_objnm = ns_infoarea-infoarea_p
                                                   iv_tlogo = rs_c_tlogo-infoarea ).

        IF lr_parent->get_cluster(  ) <> me->zif_core_check~get_cluster(  ).
          MESSAGE e073 WITH nv_objnm lr_parent->get_objnm( ) me->zif_core_check~get_cluster(  ) lr_parent->get_cluster(  ) INTO _message.
          RAISE RESUMABLE EXCEPTION TYPE zcx_core_check
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

        IF lr_parent->get_section(  ) <> me->zif_core_check~get_section(  ).
          MESSAGE e018 WITH nv_objnm nv_tlogo me->zif_core_check~get_section(  ) lr_parent->get_section(  ) INTO _message.
          RAISE RESUMABLE EXCEPTION TYPE zcx_core_check
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

        " if the parent is not a Section and Root... it must have the same block
        TRY.
            "" Skip check if the upper node is a structural node
            CALL METHOD check_structure_infoarea( lr_parent->get_objnm( ) ).

            IF lr_parent->get_block(  ) <> me->zif_core_check~get_block(  ).
              MESSAGE e017 WITH nv_objnm nv_tlogo me->zif_core_check~get_block(  ) lr_parent->get_block(  ) INTO _message.
              RAISE RESUMABLE EXCEPTION TYPE zcx_core_check
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
          CATCH cx_rs_fcst_univariate.
        ENDTRY.


      CATCH BEFORE UNWIND zcx_core_check INTO lrx_check.
        APPEND lrx_check TO lt_exception.
        RESUME.
      CATCH cx_rs_fcst_univariate.
    ENDTRY.

  ENDMETHOD.

  METHOD if_rso_tlogo_general~exists_on_db.

    SELECT SINGLE *
        FROM rsdarea
        WHERE infoarea = @i_objnm AND
              objvers  = @i_objvers
        INTO @DATA(lv_infoarea).
    IF sy-subrc = 0 .
      r_answer = rs_c_true.
    ELSE.
      r_answer = rs_c_false.
    ENDIF.

  ENDMETHOD.

  METHOD zif_core_check~get_block.
    SELECT SINGLE docblock
      FROM zi_core_contentview
      WHERE infoarea = @nv_infoarea
      INTO @rv_block.
  ENDMETHOD.

  METHOD zif_core_check~get_cluster.
    SELECT SINGLE doccluster
      FROM zi_core_contentview
      WHERE infoarea = @nv_infoarea
      INTO @rv_cluster.
  ENDMETHOD.

  METHOD zif_core_check~get_section.
    SELECT SINGLE docsection
    FROM zi_core_contentview
    WHERE infoarea = @nv_infoarea
    INTO @rv_section.
  ENDMETHOD.

  METHOD zif_core_check~do_load.
    DATA:
      lv_tlogo_objnm TYPE sobj_name.

    nv_infoarea = nv_objnm.
    lv_tlogo_objnm = nv_objnm.

    nr_if_tlogo_general ?= me. "No system/SAP implementation of the if_rso_tlogo_general

    "is found, hence we did it ourself
    IF nr_if_tlogo_general->exists_on_db(
                   i_objnm = lv_tlogo_objnm
                   i_objvers = rs_c_objvers-active ) = rs_c_true     OR
       nr_if_tlogo_general->exists_on_db(
                   i_objnm = lv_tlogo_objnm
                   i_objvers = rs_c_objvers-modified ) = rs_c_true.
      nv_deleted = rs_c_false.
      SELECT SINGLE *
          FROM rsdarea
          WHERE infoarea = @nv_infoarea AND
                objvers  = @rs_c_objvers-active
          INTO @DATA(lv_infoarea).
    ELSE.
      nv_deleted = rs_c_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_name.

    DATA:
      lt_exception TYPE zcl_core_check_type=>gtyt_check_exp.

    CHECK nv_no_name_check = rs_c_false.

    TRY.
        CALL METHOD check_structure_infoarea( me->nv_objnm ).
        " If we got this far... this is a block outside of the structure
        TRY.
            zcl_core_check_type=>check_block( me ).
          CATCH zcx_core_check INTO DATA(lrx_check).
            APPEND lrx_check TO lt_exception.
        ENDTRY.

        TRY.
            zcl_core_check_type=>check_cluster( me ).
          CATCH zcx_core_check INTO lrx_check.
            APPEND lrx_check TO lt_exception.
        ENDTRY.

        IF lt_exception IS NOT INITIAL.
          MESSAGE e016 WITH nv_objnm INTO _message.
          RAISE EXCEPTION TYPE zcx_core_check
            EXPORTING
              msgid       = sy-msgid
              msgty       = sy-msgty
              msgno       = sy-msgno
              msgv1       = sy-msgv1
              t_exception = lt_exception
              r_object    = me.
        ENDIF.

      CATCH cx_rs_fcst_univariate.
        " Do nothing... this is just to get out
    ENDTRY.

  ENDMETHOD.

  METHOD check_structure_infoarea.
    LOOP AT zcl_core_check_type=>get_all_cluster(  ) INTO DATA(lv_cluster).
      IF iv_infoarea = lv_cluster.
        RAISE EXCEPTION TYPE cx_rs_fcst_univariate.
      ENDIF.

      LOOP AT zcl_core_check_type=>get_all_section( lv_cluster ) INTO DATA(lv_section).
        IF iv_infoarea = lv_section.
          RAISE EXCEPTION TYPE cx_rs_fcst_univariate.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
