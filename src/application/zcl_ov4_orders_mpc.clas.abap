class zcl_ov4_orders_mpc definition
                         public
                         inheriting from /iwbep/cl_v4_abs_model_prov
                         create public.

  public section.

    types: begin of cds_views,
             orders type zi_ov4_orders,
             items type zi_ov4_orderitems,
           end of cds_views.

    types: begin of gt_key_range,
             order type range of cds_views-orders-id,
             item type range of cds_views-items-id,
           end of gt_key_range.

    constants: begin of cds_view_names,
                 orders type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'ZI_OV4_ORDERS',
                 items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'ZI_OV4_ORDERITEMS',
               end of cds_view_names,
               begin of entity_type_names,
                 begin of internal,
                   orders type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'ZI_OV4_ORDERS',
                   items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'ZI_OV4_ORDERITEMS',
                 end of internal,
                 begin of edm,
                   items type /iwbep/if_v4_med_element=>ty_e_med_edm_name value 'OrderItems',
                   orders type /iwbep/if_v4_med_element=>ty_e_med_edm_name value 'Orders',
                 end of edm,
               end of entity_type_names,
               begin of entity_set_names,
                 begin of internal,
                   orders type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'ZI_OV4_ORDERS',
                   items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'ZI_OV4_ORDERITEMS',
                 end of internal,
                 begin of edm,
                   orders type /iwbep/if_v4_med_element=>ty_e_med_edm_name value 'Orders',
                   items type /iwbep/if_v4_med_element=>ty_e_med_edm_name value 'OrderItems',
                 end of edm,
               end of entity_set_names,
               begin of nav_prop_names,
                 begin of internal,
                   orders_to_items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value '_ITEMS',
                   orderitems_to_header type /iwbep/if_v4_med_element=>ty_e_med_internal_name value '_HEADER',
                 end of internal,
                 begin of edm,
                   orders_to_items type /iwbep/if_v4_med_element=>ty_e_med_edm_name value '_Items',
                   orderitems_to_header type /iwbep/if_v4_med_element=>ty_e_med_edm_name value '_Header',
                 end of edm,
               end of nav_prop_names.

    class-methods class_constructor.

    methods /iwbep/if_v4_mp_basic~define redefinition.

    methods define_orders
              importing
                i_model type ref to /iwbep/if_v4_med_model
              raising
                 /iwbep/cx_gateway.

    methods define_order_items
              importing
                i_model type ref to /iwbep/if_v4_med_model
              raising
                 /iwbep/cx_gateway.

    methods view_definition
              returning
                value(r_view_definition) type ref to cl_qlast_view_definition.

  protected section.

    class-data a_view_definition type ref to cl_qlast_view_definition.

endclass.



class zcl_ov4_orders_mpc implementation.

  method /iwbep/if_v4_mp_basic~define.

    me->define_orders( io_model ).

    me->define_order_items( io_model ).

  endmethod.
  method define_orders.

    data(entity_type) = i_model->create_entity_type_by_struct( iv_entity_type_name = entity_type_names-internal-orders
                                                               is_structure = value zi_ov4_orders( )
                                                               iv_gen_prim_props = abap_true "better to create properties "manually" from view def?
                                                               iv_add_annos_to_prim_props = abap_true
                                                               iv_add_conv_to_prim_props = abap_true
                                                               iv_add_f4_help_to_prim_props = abap_true ).

    entity_type->set_edm_name( entity_type_names-edm-orders ). "change to singular?

*    data(name) = me->view_definition( )->get_name( upper_case = abap_false ). "ZI_OV4_Orders

    data(select_list) = me->view_definition( )->get_select( )->get_selectlist( ).

    loop at select_list->get_entries( ) reference into data(select_entry).

      try.

        data(property) = entity_type->get_property( exact #( cast cl_qlast_stdselectlist_entry( select_entry->* )->get_alias( ) ) ).

        data(primitive_property) = entity_type->get_primitive_property( property->get_internal_name( ) ).

        primitive_property->set_edm_name( exact #( cast cl_qlast_stdselectlist_entry( select_entry->* )->get_alias( upper_case = abap_false ) ) ).

        if cast cl_qlast_stdselectlist_entry( select_entry->* )->iskeyelement( ).

          primitive_property->set_is_key( ).

        endif.

      catch cx_sy_move_cast_error.
        "EC #NO_CATCH
      catch  /iwbep/cx_v4_med.
        "EC #NO_CATCH
      endtry.

    endloop.

    data(associations) = me->view_definition( )->get_select( )->get_associations( ).

    loop at associations->get_entries( ) reference into data(association_entry) to 1. """""""delete later

      if association_entry->*->is_exposed( ).

        data(navigation) = entity_type->create_navigation_property( exact #( association_entry->*->get_name( ) ) ).

        navigation->set_edm_name( exact #( association_entry->*->get_name( upper_case = abap_false ) ) ).

        navigation->set_target_entity_type_name( exact #( association_entry->*->get_target( )->get_name( ) ) ).

        navigation->set_target_multiplicity( cond #( let cardinality = association_entry->*->get_cardinality( ) in
                                                     when cardinality-min eq 0
                                                          and cardinality-max eq 1
                                                     then /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one_optional
                                                     when cardinality-min eq 1
                                                          and cardinality-max eq 1
                                                     then /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one
                                                     else /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ) ).

        try.

          data(on_expression) = cast cl_qlast_unmanaged_association( association_entry->* )->get_on( ).

          data(test) = cast cl_qlast_comp_expression( on_expression ). "try also with complex assoc

*        navigation->add_referential_constraint( iv_source_property_path = ``
*                                                iv_target_property_path = `` ). cast to CL_QLAST_UNMANAGED_ASSOCIATION

        catch cx_sy_move_cast_error.
          "EC #NO_CATCH
        endtry.

*        navigation->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-cascade ). when?

      endif.

    endloop.

    data(entity_set) = entity_type->create_entity_set( entity_set_names-internal-orders ). "necessary? items doesn't do it

    entity_set->set_edm_name( entity_set_names-edm-orders ).

  endmethod.
  method define_order_items.

    data(entity_type) = i_model->create_entity_type_by_struct( iv_entity_type_name = entity_type_names-internal-items
                                                               is_structure = value zi_ov4_orderitems( )
                                                               iv_gen_prim_props = abap_true
                                                               iv_add_annos_to_prim_props = abap_true
                                                               iv_add_conv_to_prim_props = abap_true
                                                               iv_add_f4_help_to_prim_props = abap_true ).

    entity_type->set_edm_name( entity_type_names-edm-items ).

    data(primitive_up_id) = entity_type->get_primitive_property( 'UPID' ).

    primitive_up_id->set_is_key( ).

    data(primitive_id) = entity_type->get_primitive_property( 'ID' ).

    primitive_id->set_is_key( ).

    entity_type->get_primitive_property( 'MODIFIEDAT' )->set_is_nullable( ). "how to pass empty/null without err

    entity_type->get_primitive_property( 'MODIFIEDBY' )->set_is_nullable( ).

    entity_type->get_primitive_properties( importing et_property = data(primitive_properties) ).

    entity_type->get_properties( importing et_property = data(properties) ).

    loop at primitive_properties assigning field-symbol(<primitive_property>).

      <primitive_property>->set_edm_name( to_mixed( <primitive_property>->get_internal_name( ) ) ). "not great

    endloop.

    data(nav_to_header) = entity_type->create_navigation_property( nav_prop_names-internal-orderitems_to_header ).

    nav_to_header->set_edm_name( nav_prop_names-edm-orderitems_to_header ).

    nav_to_header->set_target_entity_type_name( entity_type_names-internal-orders ).

    nav_to_header->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one ).

    nav_to_header->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-none ).

  endmethod.
  method class_constructor.

    try.

      cl_dd_ddl_handler_factory=>create_internal( )->get_viewdef_from_src( exporting ddlname = to_upper( 'ZI_OV4_Orders' )
                                                                                     get_state = 'A'
                                                                                     langu = 'E'
*                                                                                     ddlsrcv_wa = ddlsrcv_wa "DDL Source
*                                                                                     parse_strictness = 0 "Controls the stringency of checks during parsing
*                                                                                     prid = -1 "ID for Log Writer
*                                                                                     extends_from_parser = abap_false "ABAP_false: No expand of extends by parser
                                                                           importing viewdef = data(ddl_statement) ).

      zcl_ov4_orders_mpc=>a_view_definition = cast #( ddl_statement ).

    catch cx_dd_ddl_read
          cx_dd_ddl_to_view
          cx_sy_move_cast_error into data(error).

      message error->get_text( ) type 'X'.

    endtry.

  endmethod.
  method view_definition.

    r_view_definition = zcl_ov4_orders_mpc=>a_view_definition.

  endmethod.

endclass.
