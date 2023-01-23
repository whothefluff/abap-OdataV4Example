class zcl_ov4_orders_dpc definition
                         public
                         inheriting from /iwbep/cl_v4_abs_data_provider
                         create public.

  public section.

    interfaces: /iwbep/if_v4_dp_basic.

    METHODS read_list_salesorder
      IMPORTING
        io_request        TYPE REF TO /iwbep/if_v4_requ_basic_list
        io_response       TYPE REF TO /iwbep/if_v4_resp_basic_list
        iv_orderby_string TYPE any
        iv_select_string  TYPE any
        iv_where_clause   TYPE any
        iv_skip           TYPE any
        iv_top            TYPE any
        is_done_list      TYPE any.

    METHODS read_entity_orders
      IMPORTING
        io_request  TYPE REF TO /iwbep/if_v4_requ_basic_read
        io_response TYPE REF TO /iwbep/if_v4_resp_basic_read.

    METHODS read_ref_key_list_salesorder
      IMPORTING
        io_request  TYPE REF TO /iwbep/if_v4_requ_basic_ref_l
        io_response TYPE REF TO /iwbep/if_v4_resp_basic_ref_l.

    methods /iwbep/if_v4_dp_basic~read_entity redefinition.

    methods /iwbep/if_v4_dp_basic~read_entity_list redefinition.

    methods /iwbep/if_v4_dp_basic~read_ref_target_key_data_list redefinition.

  protected section.

endclass.



class zcl_ov4_orders_dpc implementation.

  method /iwbep/if_v4_dp_basic~read_entity.
"GET ….<service root>/orders(‘50000000’).
    io_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

    case entityset_name.

      when zcl_ov4_orders_mpc=>entity_set_names-internal-orders.

        read_entity_orders( io_request = io_request
                            io_response = io_response ).

    endcase.

  endmethod.
  method /iwbep/if_v4_dp_basic~read_entity_list.
"GET ….<service root>/orders
    data(done_list) = value /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list( ).

    io_request->get_todos( importing es_todo_list = data(todo_list) ).

    if todo_list-process-skip eq abap_true.

      done_list-skip = abap_true.

      io_request->get_skip( importing ev_skip = data(skip) ).

    endif.

    if todo_list-process-top = abap_true.

      done_list-top = abap_true.

      io_request->get_top( importing ev_top = data(top) ).

    endif.

    io_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

    case entityset_name.

      when zcl_ov4_orders_mpc=>entity_set_names-internal-orders.

        read_list_salesorder( io_request = io_request
                              io_response = io_response
                              iv_orderby_string = ''
                              iv_select_string = ''
                              iv_where_clause = ''
                              iv_skip = skip
                              iv_top = top
                              is_done_list = done_list ).

    endcase.

  endmethod.
  method /iwbep/if_v4_dp_basic~read_ref_target_key_data_list.
*SalesOrder(‘500000000’)/_Item or
*SalesOrder(‘500000000’)?$expand=_Item

    data: lv_source_entity_name type /iwbep/if_v4_med_element=>ty_e_med_internal_name.


    io_request->get_source_entity_type( importing ev_source_entity_type_name = lv_source_entity_name ).

    case lv_source_entity_name.

      when zcl_ov4_orders_mpc=>entity_type_names-internal-orders.

        read_ref_key_list_salesorder( io_request = io_request
                                      io_response = io_response ).

      when others.

        super->/iwbep/if_v4_dp_basic~read_ref_target_key_data_list(
          exporting
            io_request  = io_request
            io_response = io_response ).

    endcase.


  endmethod.
  method read_list_salesorder.

    data orders type standard table of ZI_OV4_Orders with empty key.

      select (iv_select_string)
        from ZI_OV4_Orders
        where (iv_where_clause)
        order by (iv_orderby_string)
        into table @orders
        up to @iv_top rows
        offset @iv_skip.

      io_response->set_busi_data( orders ).


  endmethod.
  method read_entity_orders.

    data: ls_salesorder type zcl_ov4_orders_mpc=>cds_views-orders,
          ls_key_salesorder type zcl_ov4_orders_mpc=>cds_views-orders,
          lv_salesorder_key_edm type string,
          lv_helper_int type i.

    data: ls_todo_list type /iwbep/if_v4_requ_basic_read=>ty_s_todo_list,
          ls_done_list type /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list.

    io_request->get_todos( importing es_todo_list = ls_todo_list ).

    " read the key data
    io_request->get_key_data( importing es_key_data = ls_key_salesorder ).

    ls_done_list-key_data = abap_true.

    select single *
      from ZI_OV4_Orders
      into corresponding fields of @ls_salesorder
      where Id = @ls_key_salesorder-Id.

    if ls_salesorder is not initial.
      io_response->set_busi_data( is_busi_data = ls_salesorder ).
    else.
      "Move data first to an integer to remove leading zeros from the response
      lv_salesorder_key_edm = lv_helper_int = ls_key_salesorder-Id.

      raise exception type /iwbep/cx_gateway
        exporting
          textid              = value #( ) "/iwbep/cx_gateway=>entity_not_found
          http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-not_found.
*          edm_entity_set_name = zcl_ov4_orders_mpc=>entity_set_names-edm-orders
*          entity_key          = lv_salesorder_key_edm.

    endif.

    " Report list of request options handled by application
    io_response->set_is_done( ls_done_list ).

  endmethod.
  method read_ref_key_list_salesorder.

    data: ls_salesorder_key_data type zcl_ov4_orders_mpc=>cds_views-orders,
          lt_salesorderitem_key_data type standard table of zcl_ov4_orders_mpc=>cds_views-items,
          ls_todo_list type /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_list.

    data: ls_done_list type /iwbep/if_v4_requ_basic_ref_l=>ty_s_todo_process_list,
          lv_nav_property_name type /iwbep/if_v4_med_element=>ty_e_med_internal_name.

    " Get the request options the application should/must handle
    io_request->get_todos( importing es_todo_list = ls_todo_list ).

    if ls_todo_list-process-source_key_data = abap_true.
      io_request->get_source_key_data( importing es_source_key_data =  ls_salesorder_key_data ).
      ls_done_list-source_key_data = abap_true.
    endif.

    io_request->get_navigation_prop( importing ev_navigation_prop_name = lv_nav_property_name ).

    case lv_nav_property_name.

      when zcl_ov4_orders_mpc=>nav_prop_names-internal-orders_to_items.

        select UpId,
               Id
          from ZI_OV4_OrderItems
          into corresponding fields of table @lt_salesorderitem_key_data
          where Id = @ls_salesorder_key_data-Id.

        io_response->set_target_key_data( lt_salesorderitem_key_data ).

      when others.

        raise exception type /iwbep/cx_gateway
          exporting
            http_status_code = /iwbep/cx_gateway=>gcs_http_status_codes-sv_not_implemented.

    endcase.

    io_response->set_is_done( ls_done_list ).

  endmethod.

endclass.
