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
