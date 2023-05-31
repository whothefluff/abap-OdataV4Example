class zcl_ov4_orders_mpc definition
                         public
                         inheriting from /iwbep/cl_v4_abs_model_prov
                         create public
                         global friends cl_sadl_gw_cds_p_element.

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

    methods define_order_statuses
              importing
                i_model type ref to /iwbep/if_v4_med_model
              raising
                 /iwbep/cx_gateway.

    methods define_lcalized_order_statuses
              importing
                i_model type ref to /iwbep/if_v4_med_model
              raising
                 /iwbep/cx_gateway.

    methods define_unit_of_measures
              importing
                i_model type ref to /iwbep/if_v4_med_model
              raising
                 /iwbep/cx_gateway.

    methods define_order_status_texts
              importing
                i_model type ref to /iwbep/if_v4_med_model
              raising
                 /iwbep/cx_gateway.

    methods view_definition
              returning
                value(r_view_definition) type ref to cl_qlast_view_definition.

    methods sadl_parser
              returning
                value(r_sadl_parser) type ref to if_sadl_gw_cds_parser.

    methods sadl_entity
              returning
                value(r_sadl_entity) type ref to if_sadl_entity.

  protected section.

    class-data a_view_definition type ref to cl_qlast_view_definition.

    class-data a_sadl_cds_parser type ref to if_sadl_gw_cds_parser.

    class-data a_sadl_entity type ref to if_sadl_entity .

endclass.



class zcl_ov4_orders_mpc implementation.

  method /iwbep/if_v4_mp_basic~define.

    me->define_orders( io_model ).

    me->define_order_items( io_model ).

    me->define_lcalized_order_statuses( io_model ).

    me->define_order_statuses( io_model ).

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

    data(asflkasdfasdf) = me->sadl_parser( )->get_elements( ).

    "for each element in sadl_parser, get the equivalent in view_definition (diff data)

    loop at select_list->get_entries( ) reference into data(select_entry).

      try.

        data(property) = entity_type->get_property( exact #( cast cl_qlast_stdselectlist_entry( select_entry->* )->get_alias( ) ) ).

        data(primitive_property) = entity_type->get_primitive_property( property->get_internal_name( ) ).

        primitive_property->set_edm_name( exact #( cast cl_qlast_stdselectlist_entry( select_entry->* )->get_alias( upper_case = abap_false ) ) ).

        if select_entry->*->get_type( ) eq cl_qlast_constants=>selectlist_entry_std.

*          data(testttt) = sadl_gw_cds_p_element=>get_element( cast cl_qlast_stdselectlist_entry( select_entry->* ) ). "don't really add much

          if cast cl_qlast_stdselectlist_entry( select_entry->* )->iskeyelement( ).

            primitive_property->set_is_key( ).

          else.

            primitive_property->set_is_nullable( ).

          endif.

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

        data(auxxxx) = sadl_gw_cds_p_association=>get_association( association_entry->* ). "falla porque el right no se llena hmmmmm

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

          data(comparison_on_expression) = cast cl_qlast_comp_expression( on_expression ). "try also with complex assoc

          data(comparison_select_list) = cast cl_qlast_stdselectlist_entry( cast cl_qlast_assoc_on_element( comparison_on_expression->get_left( ) )->get_selectlist_entry( ) ).

*        navigation->add_referential_constraint( iv_source_property_path = comparison_select_list->get_alias( upper_case = abap_false )
*                                                iv_target_property_path = `` ).

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
*                                                                                     ddlsrcv_wa = ddlsrcv_wa "DDL Source ######################## I think this is the difference
*                                                                                     parse_strictness = 0 "Controls the stringency of checks during parsing
*                                                                                     prid = -1 "ID for Log Writer
*                                                                                     extends_from_parser = abap_false "ABAP_false: No expand of extends by parser
                                                                           importing viewdef = data(ddl_statement) ).

      zcl_ov4_orders_mpc=>a_view_definition = cast #( ddl_statement ).

      data(sadl_entity_provider_cds) = cast if_sadl_entity_provider( new cl_sadl_entity_provider_cds( ) ).

      data(sadl_entity) = sadl_entity_provider_cds->get_entity( iv_id = `ZI_OV4_Orders`
                                                                iv_type = cl_sadl_entity_provider_cds=>gc_type ).

      sadl_entity->get_alternative_keys( importing et_alt_keys_with_elements = data(alternative_keys) ).

      sadl_entity->get_associations( importing et_associations = data(associations) ). "name, target cds, postfix conditions (all uppercase, looks hard to use)

      sadl_entity->get_association_external_names( importing et_names = data(association_names) ). "uppercase name, real name

      data(item_target) = sadl_entity->get_association_target( `_Items` ). "# looks extremely useful

      sadl_entity->get_elements( importing et_elements = data(elements) ). "name, data type, type of type (raw, dec, sstr)

      sadl_entity->get_element_external_names( importing et_names = data(element_names) ). "uppercase name, real name

      sadl_entity->get_key_elements( importing et_key_elements = data(key_elements) ). "uppercase name

      sadl_entity->get_label_elements( importing et_label_elements = data(lable_elements) ).

      sadl_entity->get_primary_key_elements( importing et_primary_key_elements = data(pk_elements) ). "uppercase name

      sadl_entity->get_ui_texts( importing et_ui_texts_by_element_names = data(element_ui_Texts) ). "uppercase name, struct with ddic texts

      sadl_entity->get_queries( importing et_queries = data(queries) ).

      cl_sadl_entity_provider_cds=>get_consumption_view_def( exporting io_cds_entity = sadl_entity
                                                             importing ev_is_consumption_view = data(is_consumption_view)
                                                                       ev_source_id = data(source_id)
                                                                       es_sadl_definition = data(sadl_definition) ). "null

      cl_sadl_entity_provider_cds=>get_metadata_load_from_entity( exporting io_cds_entity = sadl_entity
                                                                  importing es_metadata = data(metadata) ). "useless

      cl_sadl_cds_exposure_helper=>get_involved_entities( exporting iv_entity_id = `ZI_OV4_Orders`
                                                                    iv_entity_type = cl_sadl_entity_provider_cds=>gc_type
                                                          importing et_entities = data(entities) ). "ZI_OV4_Orders, ZI_OV4_ORDERITEMS, ZI_OV4_ORDERSTATUSES, ZI_OV4_ORDERSTATUSLOCALIZED

      new cl_sadl_ddl_parser_consumption( )->parse_cds_view( exporting iv_cds_view = `ZI_OV4_Orders`
                                                             importing es_sadl_definition = data(sadl_definition2) ).

      new cl_sadl_entity_consump_info( iv_id = `ZI_OV4_Orders`                                         ""name, target cds, postfix conditions (all uppercase, looks hard to use)
                                       iv_type = cl_sadl_entity_provider_cds=>gc_type )->if_sadl_entity_consump_info~get_associations( importing et_associations = data(associations2) ).

      data(sadl_parser) = cl_sadl_gw_cds_factory=>get_parser( exporting iv_cds_view = `ZI_OV4_Orders`
*                                                                        iv_ddl_source = `ZI_OV4_Orders`
                                                                        iv_semantic_check = abap_true ).

      data(elements2) = sadl_parser->get_elements( ).

      data(associations3) = sadl_parser->get_associations( ). "alias _ITEMS, content CL_SADL_GW_CDS_P_ASSOCIATION

      data(join_partners) = sadl_parser->get_join_partners( ). "null

      a_sadl_cds_parser = sadl_parser.

      a_sadl_entity = sadl_entity.

      "like zcl_ov4_orders_mpc=>a_view_definition but better
      data(testttt) = new view_definition_factory( )->from_ddl_source( new cds_view_factory( )->from_cds_name( `ZI_OV4_Orders` ) ).

    catch cx_dd_ddl_read
          cx_dd_ddl_to_view
          cx_sy_move_cast_error into data(error).

      message error->get_text( ) type 'X'.

    endtry.

  endmethod.
  method view_definition.

    r_view_definition = zcl_ov4_orders_mpc=>a_view_definition.

  endmethod.
  method sadl_parser.

    r_sadl_parser = zcl_ov4_orders_mpc=>a_sadl_cds_parser.

  endmethod.
  method sadl_entity.

    r_sadl_entity = zcl_ov4_orders_mpc=>a_sadl_entity.

  endmethod.
  method define_lcalized_order_statuses.

    read table me->sadl_parser( )->get_associations( ) with table key alias = to_upper( `_LocalizedStatus` ) into data(assoc) transporting content.

    data(test) = assoc-content->get_source_fields( ). " 'Status' lol

    data(test2) = assoc-content->get_target( ). " 'ZI_OV4_OrderStatusLocalized'

*    assoc-content->get_cardinality( ). could be useful for root

*    assoc-content->get_constraints( ). could be useful for root

    data(test3) = me->sadl_entity( )->get_association_target( `_LocalizedStatus` ).

    test3->get_elements( importing et_elements = data(elements) ). "name, data type, type of type (raw, dec, sstr)

    loop at elements reference into data(element). "solo nombre mayus peeero no assocs

      break-point.

    endloop.

    loop at me->sadl_parser( )->get_elements( ) reference into data(element2).

      break-point.
      element2->content->get_alias(  ). " 'ID'

      element2->content->get_annotations( ).

    endloop.

    data(aux) = value cl_qlast_selectlist=>entry_table_type( for <el> in me->view_definition( )->get_select( )->get_selectlist( )->get_entries( )
                                                              ( cond #( when <el>->get_type( ) eq cl_qlast_constants=>selectlist_entry_std
                                                                        then <el> ) ) ).

    delete aux where table_line is initial.

    data(x) = sadl_gw_cds_p_element=>get_elements( aux ).

    loop at x reference into data(element3).

      "= que el otro pero con mis metodines de acceso chungo

      break-point.

    endloop.

    "nombre interno + externo, annots, clave o no
    "  sadl_parser
    "    anotaciones
    "  sadl_entity
    "    internal names
    "    external names
    "    pk names
    "  view def
    "    internal names
    "    external names
    "    pk bool
    "    anotaciones
    "  view def gana, puesto que tiene todo y permite crear los elementos del sadl_parser

  endmethod.
  method define_order_statuses.

  endmethod.
  method define_order_status_texts.

  endmethod.
  method define_unit_of_measures.

  endmethod.

endclass.
