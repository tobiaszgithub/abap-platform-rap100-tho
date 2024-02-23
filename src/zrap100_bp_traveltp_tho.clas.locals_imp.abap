CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1 VALUE 'O', "Open
        accepted TYPE c LENGTH 1 VALUE 'A', "Accepted
        rejected TYPE c LENGTH 1 VALUE 'X', "Rejected
      END OF travel_status.
    METHODS:
      get_global_authorizations FOR GLOBAL AUTHORIZATION
        IMPORTING
        REQUEST requested_authorizations FOR Travel
        RESULT result,
      earlynumbering_create FOR NUMBERING
        IMPORTING entities FOR CREATE Travel,
      setStatusToOpen FOR DETERMINE ON MODIFY
        IMPORTING keys FOR Travel~setStatusToOpen,
      validateCustomer FOR VALIDATE ON SAVE
        IMPORTING keys FOR Travel~validateCustomer.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.
    METHODS deductDiscount FOR MODIFY
      IMPORTING keys FOR ACTION Travel~deductDiscount RESULT result.
    METHODS copyTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~copyTravel.
    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.

    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.
ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.
  METHOD earlynumbering_create.
    DATA entity TYPE STRUCTURE FOR CREATE zrap100_r_traveltp_tho\\Travel.
    DATA travel_id_max    TYPE /dmo/travel_id.
    " change to abap_false if you get the ABAP Runtime error 'BEHAVIOR_ILLEGAL_STATEMENT'
    DATA use_number_range TYPE abap_bool VALUE abap_true.

    "Ensure Travel ID is not set yet (idempotent)- must be checked when BO is draft-enabled
    LOOP AT entities INTO entity WHERE TravelID IS NOT INITIAL.
      APPEND CORRESPONDING #( entity ) TO mapped-travel.
    ENDLOOP.

    DATA(entities_wo_travelid) = entities.
    "Remove the entries with an existing Travel ID
    DELETE entities_wo_travelid WHERE TravelID IS NOT INITIAL.

    IF entities_wo_travelid IS INITIAL.
      RETURN.
    ENDIF.

    IF use_number_range = abap_true.

      TRY.
          cl_numberrange_runtime=>number_get(
            EXPORTING
*          ignore_buffer     =
              nr_range_nr       = '01'
              object            = '/DMO/TRV_M'
              quantity          = CONV #( lines( entities_wo_travelid ) )
*          subobject         =
*          toyear            =
            IMPORTING
              number            = DATA(number_range_key)
              returncode        = DATA(number_range_return_code)
              returned_quantity = DATA(number_range_returned_quantity)
          ).
        CATCH cx_number_ranges INTO DATA(lx_number_ranges).
          LOOP AT entities_wo_travelid INTO entity.
            APPEND VALUE #(  %cid      = entity-%cid
                             %key      = entity-%key
                             %is_draft = entity-%is_draft
                             %msg      = lx_number_ranges
                          ) TO reported-travel.
            APPEND VALUE #(  %cid      = entity-%cid
                             %key      = entity-%key
                             %is_draft = entity-%is_draft
                          ) TO failed-travel.
          ENDLOOP.
          EXIT.
      ENDTRY.
      "determine the first free travel ID from the number range
      travel_id_max = number_range_key - number_range_returned_quantity.
    ELSE.
      "determine the first free travel ID without number range
      "Get max travel ID from active table
      SELECT SINGLE FROM zrap100_atravtho FIELDS MAX( travel_id ) AS travelID INTO @travel_id_max.
      "Get max travel ID from draft table
      SELECT SINGLE FROM zrap100_dtravtho FIELDS MAX( travelid ) INTO @DATA(max_travelid_draft).
      IF max_travelid_draft > travel_id_max.
        travel_id_max = max_travelid_draft.
      ENDIF.
    ENDIF.

    LOOP AT entities_wo_travelid INTO entity.
      travel_id_max += 1.
      entity-TravelID = travel_id_max.

      APPEND VALUE #( %cid = entity-%cid
                      %key = entity-%key
                      %is_draft = entity-%is_draft
                       ) TO mapped-travel.

    ENDLOOP.

  ENDMETHOD.

  METHOD setStatusToOpen.
    "Read travel instances of the transferred keys
    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
      FIELDS ( OverallStatus )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      FAILED DATA(read_failed).

    "If overall travel status is already set, do nothing, i.e. remove such instances
    DELETE travels WHERE OverallStatus IS NOT INITIAL.
    CHECK travels IS NOT INITIAL.

    "else set overall travel status to open ('O')
    MODIFY ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
      UPDATE FIELDS ( OverallStatus )
      WITH  VALUE #( FOR travel IN travels ( %tky = travel-%tky
                                             OverallStatus = travel_status-open ) )
      REPORTED DATA(update_reported).

    "Set the changing parameter
    reported = CORRESPONDING #( DEEP update_reported ).

  ENDMETHOD.

  METHOD validateCustomer.
    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
      FIELDS ( CustomerID )
      WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.
    IF customers IS NOT INITIAL.
      SELECT FROM /dmo/customer FIELDS customer_id
                        FOR ALL ENTRIES IN @customers
                        WHERE customer_id = @customers-customer_id
      INTO TABLE @DATA(valid_customers).

      LOOP AT travels INTO DATA(travel).
        APPEND VALUE #( %tky = travel-%tky
                        %state_area = 'VALIDATE_CUSTOMER'
                         ) TO reported-travel.

        IF travel-CustomerID IS  INITIAL.
          APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

          APPEND VALUE #( %tky = travel-%tky
                          %state_area         = 'VALIDATE_CUSTOMER'
                          %msg = NEW /dmo/cm_flight_messages(
                                                textid = /dmo/cm_flight_messages=>enter_customer_id
                                                severity = if_abap_behv_message=>severity-error )
                          %element-CustomerID = if_abap_behv=>mk-on
           ) TO reported-travel.
        ELSEIF travel-CustomerID IS NOT INITIAL AND NOT line_exists( valid_customers[ customer_id = travel-CustomerID ] ).
          APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.

          APPEND VALUE #(  %tky                = travel-%tky
                           %state_area         = 'VALIDATE_CUSTOMER'
                           %msg                = NEW /dmo/cm_flight_messages(
                                                                  customer_id = travel-customerid
                                                                  textid      = /dmo/cm_flight_messages=>customer_unkown
                                                                  severity    = if_abap_behv_message=>severity-error )
                           %element-CustomerID = if_abap_behv=>mk-on
                        ) TO reported-travel.
        ENDIF.

      ENDLOOP.

    ENDIF.


  ENDMETHOD.

  METHOD validateDates.
    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
    ENTITY Travel
    FIELDS ( BeginDate EndDate TravelID )
    WITH CORRESPONDING #( keys )
    RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).

      APPEND VALUE #( %tky = travel-%tky
                      %state_area = 'VALIDATE_DATES'  ) TO reported-travel.

      IF travel-BeginDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg = NEW /dmo/cm_flight_messages(
                                      textid   = /dmo/cm_flight_messages=>enter_begin_date
                                      severity = if_abap_behv_message=>severity-error )
                       %element-BeginDate = if_abap_behv=>mk-on

           ) TO reported-travel.
      ENDIF.

      IF travel-BeginDate < cl_abap_context_info=>get_system_date( ) AND travel-BeginDate IS NOT INITIAL.
        APPEND VALUE #( %tky               = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg              = NEW /dmo/cm_flight_messages(
                                                                begin_date = travel-BeginDate
                                                                textid     = /dmo/cm_flight_messages=>begin_date_on_or_bef_sysdate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

      IF travel-EndDate IS INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                         %msg                = NEW /dmo/cm_flight_messages(
                                                                textid   = /dmo/cm_flight_messages=>enter_end_date
                                                                severity = if_abap_behv_message=>severity-error )
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

      IF travel-EndDate < travel-BeginDate AND travel-BeginDate IS NOT INITIAL
                                     AND travel-EndDate IS NOT INITIAL.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW /dmo/cm_flight_messages(
                                                                textid     = /dmo/cm_flight_messages=>begin_date_bef_end_date
                                                                begin_date = travel-BeginDate
                                                                end_date   = travel-EndDate
                                                                severity   = if_abap_behv_message=>severity-error )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD deductDiscount.
    DATA travels_for_update TYPE TABLE FOR UPDATE zrap100_r_traveltp_tho.
    DATA(keys_with_valid_discount) = keys.

    LOOP AT keys_with_valid_discount ASSIGNING FIELD-SYMBOL(<key_with_valid_discount>)
      WHERE %param-discount_percent IS INITIAL OR %param-discount_percent > 100 OR %param-discount_percent <= 0.

      " report invalid discount value appropriately
      APPEND VALUE #( %tky                       = <key_with_valid_discount>-%tky ) TO failed-travel.

      APPEND VALUE #( %tky                       = <key_with_valid_discount>-%tky
                      %msg                       = NEW /dmo/cm_flight_messages(
                                                       textid = /dmo/cm_flight_messages=>discount_invalid
                                                       severity = if_abap_behv_message=>severity-error )
                      %element-TotalPrice        = if_abap_behv=>mk-on
                      %op-%action-deductDiscount = if_abap_behv=>mk-on
                    ) TO reported-travel.

      " remove invalid discount value
      DELETE keys_with_valid_discount.
    ENDLOOP.

    CHECK keys_with_valid_discount IS NOT INITIAL.

    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
      FIELDS ( BookingFee )
      WITH CORRESPONDING #( keys_with_valid_discount )
      RESULT DATA(travels).

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      DATA percentage TYPE decfloat16.
      DATA(discount_percent) = keys_with_valid_discount[ KEY draft %tky = <travel>-%tky ]-%param-discount_percent.
      percentage = discount_percent / 100.

      DATA(reduced_fee) = <travel>-BookingFee * ( 1 - percentage ) .

      APPEND VALUE #( %tky = <travel>-%tky
                      BookingFee = reduced_fee ) TO travels_for_update.

    ENDLOOP.

    MODIFY ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
      UPDATE FIELDS ( BookingFee )
      WITH travels_for_update.

    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
    ENTITY Travel
    ALL FIELDS WITH
    CORRESPONDING #( travels )
    RESULT DATA(travels_with_discount).

    " set action result
    result = VALUE #( FOR travel IN travels_with_discount ( %tky   = travel-%tky
                                                              %param = travel ) ).

  ENDMETHOD.

  METHOD copyTravel.
    DATA: travels TYPE TABLE FOR CREATE zrap100_r_traveltp_tho\\travel.

    READ TABLE keys WITH KEY %cid = '' INTO DATA(key_with_initial_cid).
    ASSERT key_with_initial_cid IS INITIAL.

    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
    ENTITY Travel
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(travel_read_result)
    FAILED failed.

    LOOP AT travel_read_result ASSIGNING FIELD-SYMBOL(<travel>).
      APPEND VALUE #( %cid = keys[ KEY entity %key = <travel>-%key ]-%cid
                      %is_draft = keys[ KEY entity %key = <travel>-%key ]-%param-%is_draft
                      %data = CORRESPONDING #( <travel> EXCEPT TravelID )
                     )
      TO travels ASSIGNING FIELD-SYMBOL(<new_travel>).
      " adjust the copied travel instance data
      "" BeginDate must be on or after system date
      <new_travel>-BeginDate     = cl_abap_context_info=>get_system_date( ).
      "" EndDate must be after BeginDate
      <new_travel>-EndDate       = cl_abap_context_info=>get_system_date( ) + 30.
      "" OverallStatus of new instances must be set to open ('O')
      <new_travel>-OverallStatus = travel_status-open.
    ENDLOOP.

    MODIFY ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
      CREATE FIELDS ( AgencyID CustomerID BeginDate EndDate BookingFee
                      TotalPrice CurrencyCode OverallStatus Description )
      WITH travels
      MAPPED DATA(mapped_create).

    mapped-travel = mapped_create-travel.

  ENDMETHOD.

  METHOD acceptTravel.
    " modify travel instance
    MODIFY ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-accepted ) )  " 'A'
    FAILED failed
    REPORTED reported.

    " read changed data for action result
    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(travels).

    " set the action result parameter
    result = VALUE #( FOR travel IN travels ( %tky   = travel-%tky
                                              %param = travel ) ).
  ENDMETHOD.

  METHOD rejectTravel.
    " modify travel instance(s)
    MODIFY ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
        UPDATE FIELDS ( OverallStatus )
        WITH VALUE #( FOR key IN keys ( %tky          = key-%tky
                                        OverallStatus = travel_status-rejected ) )  " 'X'
    FAILED failed
    REPORTED reported.

    " read changed data for action result
    READ ENTITIES OF zrap100_r_traveltp_tho IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH
        CORRESPONDING #( keys )
      RESULT DATA(travels).

    " set the action result parameter
    result = VALUE #( FOR travel IN travels ( %tky   = travel-%tky
                                              %param = travel ) ).
  ENDMETHOD.

ENDCLASS.
