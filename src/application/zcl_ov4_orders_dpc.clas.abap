class zcl_ov4_orders_dpc definition
                         public
                         inheriting from /iwbep/cl_v4_abs_data_provider
                         create public.

public section.

  "! <p class="shorttext synchronized" lang="EN"></p>
  "!
  "! @parameter i_request | <p class="shorttext synchronized" lang="EN"></p>
  "! @parameter i_response | <p class="shorttext synchronized" lang="EN"></p>
  "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized" lang="EN"></p>
  methods read_entity
            importing
              !i_request type ref to /iwbep/if_v4_requ_basic_read
              !i_response type ref to /iwbep/if_v4_resp_basic_read
            raising
              /iwbep/cx_gateway.

  "! <p class="shorttext synchronized" lang="EN"></p>
  "! Paging in general is probably a bad idea until 7.51 and offset
  "!
  "! @parameter i_request | <p class="shorttext synchronized" lang="EN"></p>
  "! @parameter i_response | <p class="shorttext synchronized" lang="EN"></p>
  "! @parameter i_auto_paging_size | <p class="shorttext synchronized" lang="EN"></p>
  "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized" lang="EN"></p>
  methods read_entity_list
            importing
              !i_request type ref to /iwbep/if_v4_requ_basic_list
              !i_response type ref to /iwbep/if_v4_resp_basic_list
              i_auto_paging_size type i default 1000
            raising
              /iwbep/cx_gateway.

  methods READ_REF_KEY_LIST_SALESORDER
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_REF_L
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_REF_L .

  methods /iwbep/if_v4_dp_basic~read_entity redefinition.

  methods /iwbep/if_v4_dp_basic~read_entity_list redefinition.

  methods /iwbep/if_v4_dp_basic~read_ref_target_key_data_list redefinition.

  protected section.

endclass.
class zcl_ov4_orders_dpc implementation.

  method /iwbep/if_v4_dp_basic~read_entity.

    io_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

    case entityset_name.

      when 'entityThatRequiresCustomSelect'.

      when others.

        read_entity( i_request = io_request
                     i_response = io_response ).

    endcase.

  endmethod.
  method /iwbep/if_v4_dp_basic~read_entity_list.

    io_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

    case entityset_name.

      when 'entityThatRequiresCustomSelect'.

      when others.

        me->read_entity_list( i_request = io_request
                              i_response = io_response ).

    endcase.

  endmethod.
  method /iwbep/if_v4_dp_basic~read_ref_target_key_data_list.
*SalesOrder(‘500000000’)/_Item or
*SalesOrder(‘500000000’)?$expand=_Item

    data: lv_source_entity_name type /iwbep/if_v4_med_element=>ty_e_med_internal_name.


    io_request->get_source_entity_type( importing ev_source_entity_type_name = lv_source_entity_name ).

    case lv_source_entity_name.

      when ''.

        read_ref_key_list_salesorder( io_request = io_request
                                      io_response = io_response ).

      when others.

        super->/iwbep/if_v4_dp_basic~read_ref_target_key_data_list(
          exporting
            io_request  = io_request
            io_response = io_response ).

    endcase.


  endmethod.
  method read_entity.

    data structure type ref to data.

    i_request->get_todos( importing es_todo_list = data(todo) ).

    if todo-process-key_data eq abap_true.

      i_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

      data(cds_type) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( entityset_name ) ).

      create data structure type handle cds_type.

      assign structure->* to field-symbol(<structure>).

      cast /iwbep/cl_v4_request_info_pro( i_request )->get_base_request_info( )->get_source_key_data_tab( importing et_source_key_tab = data(key_data_tab) ).

      data(sql_cond) = reduce #( init dyn_cond type string
                                 for <entry> in key_data_tab index into index
                                 let logical_expression = cond #( when index ne 1
                                                                  then ` and ` ) in
                                 next dyn_cond = |{ dyn_cond }{ logical_expression }{ <entry>-name } eq '{ <entry>-value }'| ).

      i_request->get_selected_properties( importing et_selected_property = data(selected_properties_aux) ).

      data(selected_properties) = concat_lines_of( table = selected_properties_aux
                                                   sep = `,` ).

      select single (selected_properties)
        from (entityset_name)
        where (sql_cond)
        into corresponding fields of @<structure>.

      if <structure> is not initial.

        if todo-return-busi_data eq abap_true.

          i_response->set_busi_data( <structure> ).

        endif.

      else.

        i_request->get_entity_type( importing ev_entity_type_name = data(entity_type_name) ).

        raise exception type /iwbep/cx_v4_runtime exporting textid = /iwbep/cx_v4_runtime=>entity_not_found
                                                            entity_type_name = conv #( entity_type_name )
                                                            navigation_key = selected_properties
                                                            http_status_code = /iwbep/cx_gateway=>gcs_http_status_codes-not_found.

      endif.

    endif.

    i_response->set_is_done( value #( if_none_match_etag = value #( ) "how do I get todo-process-if_none_match_etag to be true and what does it mean
                                      key_data = xsdbool( todo-process-key_data eq abap_true )
                                      select = xsdbool( todo-process-select eq abap_true ) ) ).

*    io_response->set_not_modified( ) "what's this

  endmethod.
  method read_entity_list.

    data itab type ref to data.

    field-symbols <itab> type standard table.

    i_request->get_todos( importing es_todo_list = data(todo) ).

    if todo-process-skip_token eq abap_true
       and ( todo-process-skip eq abap_true
             or todo-process-top eq abap_true ).

      raise exception type /iwbep/cx_v4_not_implemented exporting textid = /iwbep/cx_v4_not_implemented=>not_supported_query_options
                                                                  exception_category = /iwbep/cx_v4_not_implemented=>gcs_excep_categories-provider
                                                                  http_status_code = /iwbep/cx_v4_not_implemented=>gcs_http_status_codes-sv_not_implemented.

    endif.

    i_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

    i_request->get_filter_osql_where_clause( importing ev_osql_where_clause = data(sql_cond) ).

    if todo-return-count eq abap_true
       and not ( todo-return-busi_data eq abap_true ).

      data(count) = 0.

      select count(*)
        from (entityset_name)
        where (sql_cond)
        into @count.

      i_response->set_count( count ).

    else.

      i_request->get_selected_properties( importing et_selected_property = data(selected_properties_aux) ).

      data(selected_properties) = concat_lines_of( table = selected_properties_aux
                                                   sep = `,` ).

      "io_request->get_osql_orderby_clause( IMPORTING ev_osql_orderby_clause = data(orderby_string) ). only supported as of 751 or later
      i_request->get_orderby( importing et_orderby_property = data(orderby_property) ).

      if orderby_property is initial.

        data(orderby) = `primary key`.

      else.

        orderby = reduce #( init aux type string
                            for <orderby_property> in orderby_property index into index
                            let direction = cond #( when <orderby_property>-descending eq abap_true
                                                    then 'descending'
                                                    else 'ascending' )
                                separator = cond #( when index ne 1
                                                    then `, ` ) in
                            next aux = |{ aux }{ separator }{ <orderby_property>-name } { direction }| ).
      endif.

      data(cds_type) = cl_abap_tabledescr=>get( p_line_type = cast #( cl_abap_typedescr=>describe_by_name( entityset_name ) )
                                                p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

      create data itab type handle cds_type.

      assign itab->* to <itab>.

      if todo-process-skip eq abap_true.

        i_request->get_skip( importing ev_skip = data(skip) ).

      endif.

      if todo-process-top eq abap_true.

        i_request->get_top( importing ev_top = data(top) ).

      endif.

      if i_auto_paging_size is not initial
         and skip is initial
         and top is initial.

        data(count_for_automatic_paging) = 0.

        select count(*)
          from (entityset_name)
          where (sql_cond)
          into @count_for_automatic_paging.

        top = i_auto_paging_size.

        if todo-process-skip_token eq abap_true.

          i_request->get_skip_token( importing ev_skip_token = data(skip_token) ).

          skip = skip_token.

        endif.

      endif.

      data(max_index) = cond i( when top is not initial "necessary only in 7.50
                                then skip + top
                                else 0 ).

      select (selected_properties)
        from (entityset_name)
        where (sql_cond)
        order by (orderby)
        into corresponding fields of table @<itab>
*        offset @skip only supported as of 751 or later
*        up to @top rows
        up to @max_index rows.

      if skip is not initial.

        delete <itab> to skip.

      endif.

      if todo-return-busi_data eq abap_true.

        i_response->set_busi_data( <itab> ).

        if todo-return-count eq abap_true.

          i_response->set_count( lines( <itab> ) ). "i_response->set_count( sy-dbcnt ). only for 7.51 with offset

        endif.

      endif.

      if count_for_automatic_paging gt skip_token + i_auto_paging_size.

        i_response->set_skip_token( |{ skip_token + i_auto_paging_size number = raw }| ).

      endif.

    endif.

    i_response->set_is_done( value #( delta_token = value #( )
                                      filter = xsdbool( todo-process-filter eq abap_true )
                                      key_data = value #( )
                                      orderby =  xsdbool( todo-process-orderby eq abap_true )
                                      search = value #( )
                                      select = xsdbool( todo-process-select eq abap_true )
                                      skip = xsdbool( todo-process-skip eq abap_true )
                                      skip_token = xsdbool( todo-process-skip_token eq abap_true )
                                      top = xsdbool( todo-process-top eq abap_true ) ) ).

*    io_response->set_delta_token( ).

*    io_response->set_changes_are_tracked( ). what's this

  endmethod.
  method read_ref_key_list_salesorder.

    data: ls_salesorder_key_data type ZI_OV4_Orders,
          lt_salesorderitem_key_data type standard table of ZI_OV4_OrderItems,
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

      when ''.

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
