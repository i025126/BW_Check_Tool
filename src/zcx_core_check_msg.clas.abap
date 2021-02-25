CLASS zcx_core_check_msg DEFINITION
  PUBLIC
  INHERITING FROM zcx_core_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid      LIKE if_t100_message=>t100key OPTIONAL
        !previous    LIKE previous OPTIONAL
        !msgid       TYPE sy-msgid OPTIONAL
        !msgty       TYPE sy-msgty OPTIONAL
        !msgno       TYPE sy-msgno OPTIONAL
        !msgv1       TYPE sy-msgv1 OPTIONAL
        !msgv2       TYPE sy-msgv2 OPTIONAL
        !msgv3       TYPE sy-msgv3 OPTIONAL
        !msgv4       TYPE sy-msgv4 OPTIONAL
        !r_object    TYPE REF TO zif_core_check
        !t_exception TYPE zcl_core_check_type=>gtyt_check_exp OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_core_check_msg IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous    = previous
        msgid       = msgid
        msgty       = msgty
        msgno       = msgno
        msgv1       = msgv1
        msgv2       = msgv2
        msgv3       = msgv3
        msgv4       = msgv4
        r_object    = r_object
        t_exception = t_exception.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
