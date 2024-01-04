*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class selected_properties definition
                          create public.

  public section.

    types t_value type string.

    methods constructor
              importing
                i_value type selected_properties=>t_value.

    methods value
              returning
                value(r_value) type selected_properties=>t_value.

  protected section.

    data a_value type selected_properties=>t_value.

endclass.
class selected_properties_fy definition
                             create public.

  public section.

    methods from
              importing
                i_request type ref to /iwbep/if_v4_requ_basic_list
                i_todo type /iwbep/if_v4_requ_basic_list=>ty_s_todo_list
              returning
                value(r_selected_properties) type ref to selected_properties
              raising
                /iwbep/cx_gateway.

endclass.
class itab definition
           create public.

  public section.

    types t_value type ref to data.

    methods constructor
              importing
                i_value type itab=>t_value.

    methods value
              returning
                value(r_value) type itab=>t_value.

  protected section.

    data a_value type itab=>t_value.

endclass.
class itab_fy definition
              create public.

  public section.

    methods from
              importing
                i_entity_name type /iwbep/if_v4_med_element=>ty_e_med_internal_name
              returning
                value(r_itab) type ref to itab
              raising
                /iwbep/cx_gateway.

endclass.
class delta_token_log_exp definition
                          create public.

  public section.

    types t_value type string.

    methods constructor
              importing
                i_value type delta_token_log_exp=>t_value.

    methods value
              returning
                value(r_value) type delta_token_log_exp=>t_value.

  protected section.

    data a_value type delta_token_log_exp=>t_value.

endclass.
class delta_token_log_exp_fy definition
                             create public.

  public section.

    methods from
              importing
                i_request type ref to /iwbep/if_v4_requ_basic_list
                i_todo type /iwbep/if_v4_requ_basic_list=>ty_s_todo_list
                i_osql_where_clause type string
              returning
                value(r_delta_token_log_exp) type ref to delta_token_log_exp
              raising
                /iwbep/cx_gateway.

endclass.
class sort_criteria definition
                    create public.

  public section.

    types t_value type string.

    methods constructor
              importing
                i_value type sort_criteria=>t_value.

    methods value
              returning
                value(r_value) type sort_criteria=>t_value.

  protected section.

    data a_value type sort_criteria=>t_value.

endclass.
class sort_criteria_fy definition
                       create public.

  public section.

    methods from
              importing
                i_request type ref to /iwbep/if_v4_requ_basic_list
                i_todo type /iwbep/if_v4_requ_basic_list=>ty_s_todo_list
              returning
                value(r_sort_criteria) type ref to sort_criteria
              raising
                /iwbep/cx_gateway.

endclass.
class target_query_info definition
                        create public.

  public section.

    types t_key_path_expressions type string.

    types t_entity_name type c length 30.

    methods constructor
              importing
                i_key_path_expressions type target_query_info=>t_key_path_expressions
                i_entity_name type target_query_info=>t_entity_name.

    methods key_path_expressions
              returning
                value(r_path_exp) type target_query_info=>t_key_path_expressions.

    methods entity_name
              returning
                value(r_entity_name) type target_query_info=>t_entity_name.

  protected section.

    data a_key_path_expressions_str type target_query_info=>t_key_path_expressions.

    data an_entity_name type target_query_info=>t_entity_name.

endclass.
class target_query_info_fy definition
                           create public.

  public section.

    methods from
              importing
                i_current_nav_node_name type /iwbep/if_v4_med_element=>ty_e_med_internal_name
                i_enhanced_request type ref to /iwbep/cl_v4_request_info_pro
              returning
                value(r_target_query_info) type ref to target_query_info
              raising
                /iwbep/cx_gateway.

endclass.
class navigation definition
                 create public
                 inheriting from /iwbep/cl_v4_navigation_node.

  public section.

    methods constructor
              importing
                i_navigation_node type ref to /iwbep/cl_v4_navigation_node.

    methods step
              returning
                value(r_step) type /iwbep/if_v4_request_info=>ty_s_navigation_step.

    methods previous_node
              returning
                value(r_previous_node) type ref to navigation.

    methods next_node
              returning
                value(r_next_node) type ref to navigation.

    methods model
              returning
                value(r_model) type ref to /iwbep/if_v4_med_model_r.

    methods request_info
              returning
                value(r_request_info) type ref to /iwbep/if_v4_request_info.

endclass.
**********************************************************************
class selected_properties implementation.

  method value.

    r_value = me->a_value.

  endmethod.
  method constructor.

    me->a_value = i_value.

  endmethod.

endclass.
class selected_properties_fy implementation.

  method from.

    if i_todo-process-select eq abap_true.

      i_request->get_selected_properties( importing et_selected_property = data(selected_properties_aux) ).

      data(selected_properties) = concat_lines_of( table = selected_properties_aux
                                                   sep = `,` ).

    endif.

    r_selected_properties = new #( selected_properties ).

  endmethod.

endclass.
class itab implementation.

  method value.

    r_value = me->a_value.

  endmethod.
  method constructor.

    me->a_value = i_value.

  endmethod.

endclass.
class itab_fy implementation.

  method from.

    data itab type ref to data.

    data(cds_type) = cl_abap_tabledescr=>get( p_line_type = cast #( cl_abap_typedescr=>describe_by_name( i_entity_name ) )
                                              p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).

    create data itab type handle cds_type.

    r_itab = new #( itab ).

  endmethod.

endclass.
class delta_token_log_exp implementation.

  method value.

    r_value = me->a_value.

  endmethod.
  method constructor.

    me->a_value = i_value.

  endmethod.

endclass.
class delta_token_log_exp_fy implementation.

  method from.

    if i_todo-process-delta_token eq abap_true.

      i_request->get_delta_token( importing ev_delta_token = data(delta_token) ).

      convert date delta_token(8) time delta_token+9(6) into time stamp data(tstmp) time zone '      '.

      data(logical_expression) = |{ cond #( when i_osql_where_clause is not initial "@Semantics.systemDate: { lastChangedAt: true }
                                            then ` and ` ) }ModifiedAt gt { tstmp }|.

    endif.

    r_delta_token_log_exp = new #( logical_expression ).

  endmethod.

endclass.
class sort_criteria implementation.

  method value.

    r_value = me->a_value.

  endmethod.
  method constructor.

    me->a_value = i_value.

  endmethod.

endclass.
class sort_criteria_fy implementation.

  method from. "io_request->get_osql_orderby_clause( IMPORTING ev_osql_orderby_clause = data(orderby_string) ). only supported as of 751 or later

    if i_todo-process-orderby eq abap_true.

      i_request->get_orderby( importing et_orderby_property = data(orderby_property) ).

      data(sort_criteria) = reduce #( init aux type string
                                      for <orderby_property> in orderby_property index into index
                                      let direction = cond #( when <orderby_property>-descending eq abap_true
                                                              then 'descending'
                                                              else 'ascending' )
                                          separator = cond #( when index ne 1
                                                              then `, ` ) in
                                      next aux = |{ aux }{ separator }{ <orderby_property>-name } { direction }| ).

    else.

      sort_criteria = `primary key`.

    endif.

    r_sort_criteria = new #( sort_criteria ).

  endmethod.

endclass.
class target_query_info implementation.

  method constructor.

    me->an_entity_name = i_entity_name.

    me->a_key_path_expressions_str = i_key_path_expressions.

  endmethod.
  method entity_name.

    r_entity_name = me->an_entity_name.

  endmethod.
  method key_path_expressions.

    r_path_exp = me->a_key_path_expressions_str.

  endmethod.

endclass.
class target_query_info_fy implementation.

  method from.

    types key_target_fields_w_path_exp type standard table of string with empty key.

    data(current_navigation_node) = cast /iwbep/if_v4_med_nav_prop( i_enhanced_request->get_source_entity_type( )->get_property_by_path( conv #( i_current_nav_node_name ) ) ).

    current_navigation_node->get_target_entity_type( )->get_key_property_names( importing et_internal_name = data(key_target_properties) ).

    i_enhanced_request->get_navigation_path_raw( importing et_navigation_step = data(full_navigation_tree) ).

    data(path_exp) = ``.

    loop at full_navigation_tree reference into data(navigation_leaf) where not ( nav_prop_path_from_prev_step is initial ).

      path_exp = |{ path_exp }\\{ navigation_leaf->*-nav_prop_path_from_prev_step }|.

      if i_current_nav_node_name eq navigation_leaf->*-nav_prop_path_from_prev_step.

        data(entity_name) = navigation_leaf->*-container_element_name.

        path_exp = |{ path_exp }-|.

        exit.

      endif.

    endloop.

    data(key_target_fields_w_path_exp) = value key_target_fields_w_path_exp( for <key_tgt_prop> in key_target_properties
                                                                             ( |{ path_exp }{ <key_tgt_prop> } AS { <key_tgt_prop> }| ) ).

    data(target_key_fields) = concat_lines_of( table = key_target_fields_w_path_exp
                                               sep = `,` ).

    r_target_query_info = new #( i_entity_name = entity_name
                                 i_key_path_expressions = target_key_fields ).

  endmethod.

endclass.
class navigation implementation.

  method constructor.

    super->constructor( ).

    me->ms_navigation_step = i_navigation_node->ms_navigation_step.

    me->mo_previous_step = i_navigation_node->mo_previous_step.

    me->mo_next_step = i_navigation_node->mo_next_step.

    me->mo_model = i_navigation_node->mo_model.

    me->mo_request_info = i_navigation_node->mo_request_info.

  endmethod.
  method model.

    r_model = me->mo_model.

  endmethod.
  method next_node.

    r_next_node = new navigation( me->mo_next_step ).

  endmethod.
  method previous_node.

    r_previous_node = new navigation( me->mo_previous_step ).

  endmethod.
  method request_info.

    r_request_info = me->mo_request_info.

  endmethod.
  method step.

    r_step = me->ms_navigation_step.

  endmethod.

endclass.
