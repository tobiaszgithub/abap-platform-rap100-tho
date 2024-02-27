CLASS zrap100_cl_eml_tho DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      is_draft  TYPE if_abap_behv=>t_xflag VALUE if_abap_behv=>mk-on,  "draft: '01'
      is_active TYPE if_abap_behv=>t_xflag VALUE if_abap_behv=>mk-off. "active: '00'

    CLASS-DATA:
      travel_id      TYPE /dmo/travel_id,        "travel id
      instance_state TYPE if_abap_behv=>t_xflag, "instance state (draft or active)
      console_output TYPE REF TO if_oo_adt_classrun_out.

    METHODS:
      read_travel,
      update_travel,
      create_travel,
      delete_travel,
      activate_travel_draft,
      discard_travel_draft.

ENDCLASS.


CLASS zrap100_cl_eml_tho IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " [Legende for operation execution]
    "  READ: 1 | UPDATE: 2 | CREATE: 3 | DELETE: 4 |
    "  ACTIVATE draft: 5 | DISCARD draft: 6 | ALL: 55
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "set console output instance
    console_output = out.

    "specify the operation to be executed
    DATA(execute) = 5.

    "READ a Travel BO entity instance
    IF execute = 1 OR execute = 55.
      travel_id      = '00000005'.
      instance_state = is_active.
      read_travel( ).
    ENDIF.

    "UPDATE a Travel BO entity instance
    IF execute = 2 OR execute = 55.
      travel_id      = '00000005'.
      instance_state = is_active.
      update_travel( ).
    ENDIF.

    "CREATE a Travel BO entity instance
    IF execute = 3 OR execute = 55.
      instance_state = is_active.
      create_travel( ).
    ENDIF.

    "DELETE a Travel BO entity instance
    IF execute = 4 OR execute = 55.
      travel_id      = '00004170'.
      instance_state = is_active.
      delete_travel( ).
    ENDIF.

    "ACTIVATE a draft Travel BO entity instance
    IF execute = 5 OR execute = 55.
      travel_id      = '00000005'.
      activate_travel_draft( ).
    ENDIF.

    "DISCARD a draft Travel BO entity instance
    IF execute = 6 OR execute = 55.
      travel_id      = '00000000'.
      discard_travel_draft( ).
    ENDIF.

  ENDMETHOD.


  METHOD read_travel.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exercise 9.2: Read a travel BO entity instance
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "declare internal table using derived type
    DATA travels TYPE TABLE FOR READ IMPORT ZRAP100_R_TravelTP_tho.

    "fill in data for READ request
    travels = VALUE #( ( TravelID = travel_id  %is_draft = instance_state ) ).

    "read from transactional buffer
    READ ENTITIES OF ZRAP100_R_TravelTP_tho
      ENTITY Travel
        ALL FIELDS
*        FIELDS ( AgencyID CustomerID BeginDate Description )
        WITH travels
    RESULT DATA(lt_travels_read)
    FAILED DATA(failed)
    REPORTED DATA(reported).

    "console output
    console_output->write( |Exercise 9.2: READ a Travel BO entity instance | ).
    console_output->write( lt_travels_read ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed read = { failed-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD update_travel.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exercise 9.3: Update a travel BO entity instance
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "update the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_tho
      ENTITY Travel
        UPDATE FIELDS ( Description )
          WITH VALUE #( ( %is_draft   = instance_state
                          TravelID    = travel_id
                          Description = | Vacation { cl_abap_context_info=>get_system_time( ) } |
                      ) )
      FAILED DATA(failed)
      REPORTED DATA(reported).

    "persist the transactional buffer
    COMMIT ENTITIES
      RESPONSE OF zrap100_r_traveltp_tho
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.3: UPDATE a Travel BO entity instance| ).
    console_output->write( |- TravelID = { travel_id } / Description was updated | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed update = { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit = { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD create_travel.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exercise 9.4: Create a travel BO entity instance
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "create in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_tho
      ENTITY travel
        CREATE FIELDS ( CustomerID AgencyID BeginDate EndDate Description )
          WITH VALUE #( ( %cid        = 'create_travel'
                          %is_draft   = instance_state
                          CustomerID  = '15'
                          AgencyID    = '070042'
                          BeginDate   = cl_abap_context_info=>get_system_date( )
                          EndDate     = cl_abap_context_info=>get_system_date( ) + 10
                          Description = | ABAP DevDays { cl_abap_context_info=>get_system_time(  ) } |
                      ) )

      "execute action `rejectTravel`
        ENTITY travel
        EXECUTE rejectTravel
        FROM VALUE #( ( %cid_ref = 'create_travel' %is_draft = instance_state ) )

      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_tho
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.4: CREATE a new Travel BO entity instance| ).
    console_output->write( mapped-travel ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed create: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD delete_travel.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exercise 9.5: Delete a travel BO entity instance
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "delete in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_tho
      ENTITY travel
        DELETE FROM
          VALUE
             #( ( TravelID = travel_id  %is_draft   = instance_state ) )
     FAILED DATA(failed)
     REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_tho
      FAILED     DATA(failed_commit)
      REPORTED   DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.5: DELETE a Travel BO entity instance | ).
    console_output->write( |- TravelID = { travel_id } | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed delete: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD activate_travel_draft.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exercise 9.6: Activate a draft travel BO entity instance
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "activate travel instance in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_tho
      ENTITY Travel
        EXECUTE Activate FROM
        VALUE #( ( %cid = 'activate_draft_travel'  %key-TravelID = travel_id ) )
     MAPPED DATA(mapped)
     FAILED DATA(failed)
     REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_tho
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.6: ACTIVATE a draft Travel BO entity instance | ).
    console_output->write( |- TravelID = { travel_id } | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed activate: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.


  METHOD discard_travel_draft.
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Exercise 9.7: Discard a draft travel BO entity instance
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "activate travel instance in the transactional buffer
    MODIFY ENTITIES OF ZRAP100_R_TravelTP_tho
      ENTITY Travel
        EXECUTE Discard FROM
        VALUE #( ( %key-TravelID = travel_id ) )
     MAPPED DATA(mapped)
     FAILED DATA(failed)
     REPORTED DATA(reported).

    "persist changes
    COMMIT ENTITIES
      RESPONSE OF ZRAP100_R_TravelTP_tho
      FAILED DATA(failed_commit)
      REPORTED DATA(reported_commit).

    "console output
    console_output->write( |Exercise 9.7: DISCARD a draft Travel BO entity instance | ).
    console_output->write( |- TravelID = { travel_id } | ).
    IF failed IS NOT INITIAL.
      console_output->write( |- Cause for failed discard: { failed-travel[ 1 ]-%fail-cause } | ).
    ELSEIF failed_commit IS NOT INITIAL.
      console_output->write( |- Cause for failed commit: { failed_commit-travel[ 1 ]-%fail-cause } | ).
    ENDIF.
    console_output->write( |--------------------------------------------- | ).
  ENDMETHOD.

ENDCLASS.
