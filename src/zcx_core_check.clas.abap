CLASS zcx_core_check DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    data:
      MSGID TYPE SY-MSGID,
      MSGTY TYPE SY-MSGTY,
      MSGNO TYPE SY-MSGNO,
      MSGV1 TYPE SY-MSGV1,
      MSGV2 TYPE SY-MSGV2,
      MSGV3 TYPE SY-MSGV3,
      MSGV4 TYPE SY-MSGV4,
      R_OBJECT     TYPE ref to zif_core_check,
      T_EXCEPTION  type zcl_core_check_type=>gtyt_check_exp.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !MSGID TYPE SY-MSGID OPTIONAL
        !MSGTY TYPE SY-MSGTY OPTIONAL
        !MSGNO TYPE SY-MSGNO OPTIONAL
        !MSGV1 TYPE SY-MSGV1 OPTIONAL
        !MSGV2 TYPE SY-MSGV2 OPTIONAL
        !MSGV3 TYPE SY-MSGV3 OPTIONAL
        !MSGV4 TYPE SY-MSGV4 OPTIONAL
        !R_OBJECT TYPE REF TO Zif_CORE_CHECK
        !T_EXCEPTION TYPE zcl_core_check_type=>gtyt_check_exp OPTIONAL
.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_core_check IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->MSGID = MSGID .
    me->MSGTY = MSGTY .
    me->MSGNO = MSGNO .
    me->MSGV1 = MSGV1 .
    me->MSGV2 = MSGV2 .
    me->MSGV3 = MSGV3 .
    me->MSGV4 = MSGV4 .
    me->R_OBJECT = R_OBJECT .
    me->T_EXCEPTION = T_EXCEPTION .

  ENDMETHOD.
ENDCLASS.
