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
                 orders type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'zi_ov4_orders',
                 items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'zi_ov4_orderitems',
               end of cds_view_names,
               begin of entity_type_names,
                 begin of internal,
                   orders type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'zi_ov4_orders',
                   items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'zi_ov4_orderitems',
                 end of internal,
                 begin of edm,
                   salesorderitem type /iwbep/if_v4_med_element=>ty_e_med_edm_name value 'Orders',
                   salesorder type /iwbep/if_v4_med_element=>ty_e_med_edm_name value 'OrderItems',
                 end of edm,
               end of entity_type_names,
               begin of entity_set_names,
                 begin of internal,
                   orders type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'zi_ov4_orders',
                   items type /iwbep/if_v4_med_element=>ty_e_med_internal_name value 'zi_ov4_orderitems',
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

  protected section.

endclass.



class zcl_ov4_orders_mpc implementation.

  method /iwbep/if_v4_mp_basic~define.

    me->define_orders( io_model ).

    me->define_orders( io_model ).

  endmethod.
  method define_orders.

    data(entity_type) = i_model->create_entity_type_by_struct( iv_entity_type_name = 'ZI_OV4_ORDERS'
                                                               is_structure = value zi_ov4_orders( )
                                                               iv_add_conv_to_prim_props = abap_true
                                                               iv_add_f4_help_to_prim_props = abap_true
                                                               iv_gen_prim_props = abap_true ).

    data(primitive_property) = entity_type->get_primitive_property( 'ID' ).

    primitive_property->set_is_key( ).

    data(nav_to_items) = entity_type->create_navigation_property( nav_prop_names-internal-orders_to_items ).

    nav_to_items->set_edm_name( nav_prop_names-edm-orders_to_items ).

    nav_to_items->set_target_entity_type_name( entity_type_names-internal-items ).

    nav_to_items->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ).

    nav_to_items->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-none ).

  endmethod.
  method define_order_items.

    data(entity_type) = i_model->create_entity_type_by_struct( iv_entity_type_name = 'ZI_OV4_ORDERITEMS'
                                                               is_structure = value zi_ov4_orderitems( )
                                                               iv_add_conv_to_prim_props = abap_true
                                                               iv_add_f4_help_to_prim_props = abap_true
                                                               iv_gen_prim_props = abap_true ).

    data(primitive_property) = entity_type->get_primitive_property( 'ID' ).

    primitive_property->set_is_key( ).

    data(nav_to_header) = entity_type->create_navigation_property( nav_prop_names-internal-orderitems_to_header ).

    nav_to_header->set_edm_name( nav_prop_names-edm-orderitems_to_header ).

    nav_to_header->set_target_entity_type_name( entity_type_names-internal-orders ).

    nav_to_header->set_target_multiplicity( /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one ).

    nav_to_header->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-cascade ).

  endmethod.

endclass.
