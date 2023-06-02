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

    "! <p class="shorttext synchronized" lang="EN"></p>
    "!
    "! @parameter i_request | <p class="shorttext synchronized" lang="EN"></p>
    "! @parameter i_response | <p class="shorttext synchronized" lang="EN"></p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized" lang="EN"></p>
    methods read_ref_target
              importing
                !i_request type ref to /iwbep/if_v4_requ_basic_ref_r
                !i_response type ref to /iwbep/if_v4_resp_basic_ref_r
              raising
                /iwbep/cx_gateway.

    "! <p class="shorttext synchronized" lang="EN"></p>
    "!
    "! @parameter i_request | <p class="shorttext synchronized" lang="EN"></p>
    "! @parameter i_response | <p class="shorttext synchronized" lang="EN"></p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized" lang="EN"></p>
    methods read_ref_target_list
              importing
                !i_request type ref to /iwbep/if_v4_requ_basic_ref_l
                !i_response type ref to /iwbep/if_v4_resp_basic_ref_l
              raising
                /iwbep/cx_gateway.

    methods /iwbep/if_v4_dp_basic~read_entity redefinition.

    methods /iwbep/if_v4_dp_basic~read_entity_list redefinition.

    methods /iwbep/if_v4_dp_basic~read_ref_target_key_data redefinition.

    methods /iwbep/if_v4_dp_basic~read_ref_target_key_data_list redefinition.

  protected section.

endclass.
class zcl_ov4_orders_dpc implementation.

  method /iwbep/if_v4_dp_basic~read_entity.

    io_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

    case entityset_name.

      when 'entityThatRequiresCustomSelect'.

      when others.

        me->read_entity( i_request = io_request
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
  method /iwbep/if_v4_dp_basic~read_ref_target_key_data.

    super->/iwbep/if_v4_dp_basic~read_ref_target_key_data( io_request = io_request
                                                           io_response = io_response ).

    io_request->get_source_entity_type( importing ev_source_entity_type_name = data(source_entityset_name) ).

    case source_entityset_name.

      when 'entityThatRequiresCustomSelect'.

      when others.

        me->read_ref_target( i_request = io_request
                             i_response = io_response ).

    endcase.

  endmethod.
  method /iwbep/if_v4_dp_basic~read_ref_target_key_data_list.

    super->/iwbep/if_v4_dp_basic~read_ref_target_key_data_list( io_request = io_request
                                                                io_response = io_response ).

    io_request->get_source_entity_type( importing ev_source_entity_type_name = data(source_entityset_name) ).

    case source_entityset_name.

      when 'entityThatRequiresCustomSelect'.

      when others.

        me->read_ref_target_list( i_request = io_request
                                  i_response = io_response ).

    endcase.

  endmethod.
  method read_entity.

    data structure type ref to data.

    i_request->get_todos( importing es_todo_list = data(todo) ).

    if todo-process-key_data eq abap_true.

      i_request->get_entity_set( importing ev_entity_set_name = data(entityset_name) ).

      i_request->get_selected_properties( importing et_selected_property = data(selected_properties_aux) ).

      data(selected_properties) = concat_lines_of( table = selected_properties_aux
                                                   sep = `,` ).

      data(cds_type) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( entityset_name ) ).

      create data structure type handle cds_type.

      assign structure->* to field-symbol(<structure>).

      cast /iwbep/cl_v4_request_info_pro( i_request )->get_base_request_info( )->get_source_key_data_tab( importing et_source_key_tab = data(key_data_tab) ).

      data(filter_log_exp) = reduce #( init aux type string
                                       for <entry> in key_data_tab index into index
                                       let logical_expression = cond #( when index ne 1
                                                                        then ` and ` ) in
                                       next aux = |{ aux }{ logical_expression }{ <entry>-name } eq '{ <entry>-value }'| ).

      data(log_exp) = filter_log_exp.

      select single (selected_properties)
        from (entityset_name)
        where (log_exp)
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

    i_response->set_is_done( value #( if_none_match_etag = abap_false "generic handle in /IWBEP/IF_V4_DP_ADVANCED~READ_ENTITY; used for HTTP header If-None-Match : W/"20240314213856.6122440" to filter out unchanged entities (returns 304 instead of JSON)
                                      key_data = xsdbool( todo-process-key_data eq abap_true )
                                      select = xsdbool( todo-process-select eq abap_true ) ) ).

  endmethod.
  method read_entity_list.

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

    i_request->get_filter_osql_where_clause( importing ev_osql_where_clause = data(filter_log_exp) ).

    data(delta_token_log_exp) = new delta_token_log_exp_fy( )->from( i_request = i_request
                                                                     i_todo = todo
                                                                     i_osql_where_clause = filter_log_exp )->value( ).

    data(log_exp) = filter_log_exp && delta_token_log_exp.

    if todo-return-count eq abap_true
       and not ( todo-return-busi_data eq abap_true ).

      data(count) = 0.

      select count(*)
        from (entityset_name)
        where (log_exp)
        into @count.

      i_response->set_count( count ).

    else.

      data(selected_properties) = new selected_properties_fy( )->from( i_request = i_request
                                                                       i_todo = todo )->value( ).

      data(sort_criteria) = new sort_criteria_fy( )->from( i_request = i_request
                                                           i_todo = todo )->value( ).

      data(itab) = new itab_fy( )->from( entityset_name )->value( ).

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
          where (log_exp)
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
        where (log_exp)
        order by (sort_criteria)
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

    get time stamp field data(current_ts). "change to make list etag the same as entity etag

    i_response->set_delta_token( |{ substring( val = conv string( current_ts )
                                               len = 8 ) }T{ substring( val = conv string( current_ts )
                                                                        off = 8
                                                                        len = 6 ) }Z| ).

    i_response->set_changes_are_tracked( ).

    if todo-process-key_data eq abap_true.

      break-point.
      message '' type 'X'.

    endif.

    i_response->set_is_done( value #( delta_token = xsdbool( todo-process-delta_token eq abap_true )
                                      filter = xsdbool( todo-process-filter eq abap_true )
                                      key_data = abap_false "I haven't found a query that marks this
                                      orderby =  xsdbool( todo-process-orderby eq abap_true )
                                      search = value #( ) "use semantic key to add two searches with like, one in uppercase and one in lowercase
                                      select = xsdbool( todo-process-select eq abap_true )
                                      skip = xsdbool( todo-process-skip eq abap_true )
                                      skip_token = xsdbool( todo-process-skip_token eq abap_true )
                                      top = xsdbool( todo-process-top eq abap_true ) ) ).

  endmethod.
  method read_ref_target.

    i_request->get_todos( importing es_todo_list = data(todo) ).

    i_request->get_navigation_prop( importing ev_navigation_prop_name = data(navigation_prop_name)
                                              ev_complex_property_path = data(complex_property_path) ).

*    i_request->get_source_key_data( importing es_source_key_data = data(source_key_data) ).

*    i_request->get_target_key_data( importing es_target_key_data = data(target_key_data) ).

  endmethod.
  method read_ref_target_list.

  endmethod.

endclass.
