CLASS lhc_buffer DEFINITION.
* 1) define the data buffer
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_buffer.
             INCLUDE TYPE   ztbooking_adr AS data.
    TYPES:   flag TYPE c LENGTH 1,
           END OF ty_buffer.

    TYPES tt_bookings TYPE SORTED TABLE OF ty_buffer WITH UNIQUE KEY booking.

    CLASS-DATA mt_buffer TYPE tt_bookings.
ENDCLASS.


CLASS lsc_zi_booking_001 DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS save REDEFINITION.

ENDCLASS.

CLASS lsc_zi_booking_001 IMPLEMENTATION.

  METHOD save.
    DATA lt_data TYPE STANDARD TABLE OF ztbooking_adr.

    lt_data = VALUE #(  FOR row IN lhc_buffer=>mt_buffer WHERE  ( flag = 'C' ) (  row-data ) ).
    IF lt_data IS NOT INITIAL.
      INSERT ztbooking_adr FROM TABLE @lt_data.
    ENDIF.


    CLEAR lt_data.
    lt_data = VALUE #(  FOR row IN lhc_buffer=>mt_buffer WHERE  ( flag = 'D' ) (  row-data ) ).
    IF lt_data IS NOT INITIAL.
      DELETE ztbooking_adr FROM TABLE @lt_data.
    ENDIF.


    CLEAR lt_data.
    lt_data = VALUE #(  FOR row IN lhc_buffer=>mt_buffer WHERE  ( flag = 'U' ) (  row-data ) ).
    IF lt_data IS NOT INITIAL.
      UPDATE ztbooking_adr FROM TABLE @lt_data.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_booking_adr RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zi_booking_adr.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zi_booking_adr.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zi_booking_adr.

    METHODS read FOR READ
      IMPORTING keys FOR READ zi_booking_adr RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zi_booking_adr.

ENDCLASS.


CLASS lhc_booking IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.

    IF entities IS NOT INITIAL.
      SELECT SINGLE MAX( booking ) FROM ztbooking_001 INTO @DATA(lv_max_booking).
    ENDIF.

    LOOP AT entities INTO DATA(ls_create).
      ADD 1 TO lv_max_booking.
      ls_create-%data-booking = lv_max_booking.
      GET TIME STAMP FIELD DATA(zv_tsl).
      ls_create-%data-lastchangedat = zv_tsl.
      INSERT VALUE #( flag = 'C' data = CORRESPONDING #( ls_create-%data ) ) INTO TABLE lhc_buffer=>mt_buffer.

      IF ls_create-%cid IS NOT INITIAL.
        INSERT VALUE #( %cid = ls_create-%cid  booking = ls_create-booking ) INTO TABLE mapped-zi_booking_adr.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD update.


    LOOP AT entities INTO DATA(ls_update).

      IF ls_update-booking IS INITIAL.
        ls_update-booking = mapped-zi_booking_adr[ %cid = ls_update-%cid_ref ]-booking.
      ENDIF.


      READ TABLE lhc_buffer=>mt_buffer WITH KEY booking = ls_update-booking ASSIGNING FIELD-SYMBOL(<ls_buffer>).

      IF sy-subrc <> 0.

        SELECT SINGLE * FROM ztbooking_adr WHERE booking = @ls_update-booking INTO @DATA(ls_db).
        INSERT VALUE #( flag = 'U' data = ls_db ) INTO TABLE lhc_buffer=>mt_buffer ASSIGNING <ls_buffer>.

      ENDIF.

      IF ls_update-%control-customername IS NOT INITIAL.
        <ls_buffer>-customername = ls_update-customername.
      ENDIF.
      IF ls_update-%control-cost  IS NOT INITIAL.
        <ls_buffer>-cost = ls_update-cost.
      ENDIF.
      IF ls_update-%control-dateoftravel   IS NOT INITIAL.
        <ls_buffer>-dateoftravel  = ls_update-dateoftravel.
      ENDIF.
      IF ls_update-%control-currencycode  IS NOT INITIAL.
        <ls_buffer>-currencycode = ls_update-currencycode.
      ENDIF.
      GET TIME STAMP FIELD DATA(zv_tsl2).
      <ls_buffer>-lastchangedat = zv_tsl2.

    ENDLOOP.


  ENDMETHOD.

  METHOD delete.



    LOOP AT keys INTO DATA(ls_delete).

      IF ls_delete-booking IS INITIAL.
        ls_delete-booking = mapped-zi_booking_adr[ %cid = ls_delete-%cid_ref ]-booking.
      ENDIF.

      INSERT VALUE #( flag = 'D' booking = ls_delete-booking ) INTO TABLE lhc_buffer=>mt_buffer.
    ENDLOOP.

  ENDMETHOD.


  METHOD read.

    LOOP AT keys INTO DATA(ls_booking_key).
      " check if it is in buffer (and not deleted).
      READ TABLE lhc_buffer=>mt_buffer WITH KEY booking = ls_booking_key-booking INTO DATA(ls_booking).
      IF sy-subrc = 0 AND ls_booking-flag <> 'U'.
        INSERT CORRESPONDING #( ls_booking-data ) INTO TABLE result.
      ELSE.
        SELECT SINGLE * FROM ztbooking_adr WHERE booking = @ls_booking_key-booking INTO @DATA(ls_db).
        IF sy-subrc = 0.
          INSERT CORRESPONDING #( ls_db ) INTO TABLE result.
        ELSE.
          INSERT VALUE #( booking = ls_booking_key-booking ) INTO TABLE failed-zi_booking_adr.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

ENDCLASS.
