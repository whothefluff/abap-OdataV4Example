class zcl_ov4_orders_mpc definition
                         public
                         inheriting from /iwbep/cl_v4_abs_model_prov
                         create public.

  public section.

    class-methods class_constructor.

    methods /iwbep/if_v4_mp_basic~define redefinition.

    "! <p class="shorttext synchronized" lang="EN">Define entity and navigations</p>
    "! Navigations are defined from from exposed associations. To work,
    "! the defined navigations have to be created as entities too in the model
    "! <br/>eTags are created from elements with Annotation Semantics.systemDate.lastChangedAt
    "!
    "! @parameter i_name | <p class="shorttext synchronized" lang="EN"></p>
    "! @parameter i_external_name | <p class="shorttext synchronized" lang="EN"></p>
    "! @parameter i_external_set_name | <p class="shorttext synchronized" lang="EN"></p>
    "! @parameter i_model_associations | <p class="shorttext synchronized" lang="EN"></p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized" lang="EN"></p>
    methods define_from_entity
              importing
                i_name type string
                i_external_name type string optional
                i_external_set_name type string optional
                i_model_associations type abap_bool default abap_true
              raising
                 /iwbep/cx_gateway.

    methods metadata_model
              returning
                value(r_metadata_model) type ref to /iwbep/if_v4_med_model.

  protected section.

    data a_metadata_model type ref to /iwbep/if_v4_med_model.

endclass.
class zcl_ov4_orders_mpc implementation.

  method class_constructor.

*    data(sadl_entity_provider_cds) = cast if_sadl_entity_provider( new cl_sadl_entity_provider_cds( ) ).
*
*    data(sadl_entity) = sadl_entity_provider_cds->get_entity( iv_id = `ZI_OV4_Orders`
*                                                              iv_type = cl_sadl_entity_provider_cds=>gc_type ).
*
*    cl_sadl_entity_provider_cds=>get_consumption_view_def( exporting io_cds_entity = sadl_entity
*                                                           importing ev_is_consumption_view = data(is_consumption_view)
*                                                                     ev_source_id = data(source_id)
*                                                                     es_sadl_definition = data(sadl_definition) ).
*
*    cl_sadl_entity_provider_cds=>get_metadata_load_from_entity( exporting io_cds_entity = sadl_entity
*                                                                importing es_metadata = data(metadata) ).
*
*    cl_sadl_cds_exposure_helper=>get_involved_entities( exporting iv_entity_id = `ZI_OV4_Orders`
*                                                                  iv_entity_type = cl_sadl_entity_provider_cds=>gc_type
*                                                        importing et_entities = data(entities) ).
*
*    new cl_sadl_ddl_parser_consumption( )->parse_cds_view( exporting iv_cds_view = `ZI_OV4_Orders`
*                                                           importing es_sadl_definition = data(sadl_definition2) ).
*
*    new cl_sadl_entity_consump_info( iv_id = `ZI_OV4_Orders`
*                                     iv_type = cl_sadl_entity_provider_cds=>gc_type )->if_sadl_entity_consump_info~get_associations( importing et_associations = data(associations2) ).
*
*    data(sadl_parser) = cl_sadl_gw_cds_factory=>get_parser( exporting iv_cds_view = `ZI_OV4_Orders`
**                                                                      iv_ddl_source = ``
*                                                                      iv_semantic_check = abap_true ).

  endmethod.
  method /iwbep/if_v4_mp_basic~define.

    me->a_metadata_model = io_model.

    me->define_from_entity( `ZI_OV4_Orders` ).

    me->define_from_entity( `ZI_OV4_OrderItems` ).

    me->define_from_entity( i_name = `I_UnitOfMeasure`
                            i_model_associations = abap_false ).

    me->define_from_entity( `ZI_OV4_OrderStatuses` ).

    me->define_from_entity( `ZI_OV4_OrderStatusTexts` ).

    me->define_from_entity( `ZI_OV4_OrderStatusLocalized` ).

*    me->define_from_entity( i_name = `I_UnitOfMeasureText`
*                            i_model_associations = abap_false ).

  endmethod.
  method define_from_entity.

    data structure type ref to data.

    data(view_definition) = new view_definition_factory( )->from_ddl_source( new ddl_source_factory( )->from_cds_name( i_name ) ).

    data(cds_type) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( view_definition->get_name( ) ) ).

    create data structure type handle cds_type.

    assign structure->* to field-symbol(<structure>).

    data(entity_type) = me->metadata_model( )->create_entity_type_by_struct( iv_entity_type_name = exact #( view_definition->get_name( ) && '_TY' )
                                                                             is_structure = <structure>
                                                                             iv_gen_prim_props = abap_true
                                                                             iv_add_annos_to_prim_props = abap_true
                                                                             iv_add_conv_to_prim_props = abap_true
                                                                             iv_add_f4_help_to_prim_props = abap_true ).

    entity_type->set_edm_name( cond #( when i_external_name is not initial
                                       then i_external_name
                                       else view_definition->get_name_struct( )-name && `Type` ) ).

    cl_dd_ddl_annotation_service=>get_drct_annos_4_entity_elmnts( exporting entityname = exact #( i_name )
                                                                  importing annos = data(element_annotations) ).

    data(composition_up_annotations) = value cl_dd_ddl_annotation_service=>ty_t_elmnt_anno_value( for <annotation> in element_annotations where ( value eq '#TO_COMPOSITION_PARENT'
                                                                                                                                                  or value eq '#TO_COMPOSITION_ROOT' )
                                                                                                  ( <annotation> ) ).

    if composition_up_annotations is initial.

      data(entity_set) = entity_type->create_entity_set( exact #( view_definition->get_name( ) ) ).

      entity_set->set_edm_name( cond #( when i_external_set_name is not initial
                                        then i_external_set_name
                                        else view_definition->get_name_struct( )-name ) ).

    endif.

    data(select_list) = view_definition->get_select( )->get_selectlist( ).

    loop at select_list->get_entries( ) reference into data(select_entry).

      if select_entry->*->get_type( ) eq cl_qlast_constants=>selectlist_entry_std.

*        data(gw_element) = sadl_gw_cds_p_element=>get_element( cast cl_qlast_stdselectlist_entry( select_entry->* ) ). "don't add much

        data(non_association_entry) = cast cl_qlast_stdselectlist_entry( select_entry->* ).

        data(property) = entity_type->get_property( exact #( non_association_entry->get_alias( ) ) ).

        data(primitive_property) = entity_type->get_primitive_property( property->get_internal_name( ) ).

        primitive_property->set_edm_name( exact #( non_association_entry->get_alias_struct( )-name ) ).

        if non_association_entry->iskeyelement( ).

          primitive_property->set_is_key( ). "if it's part of a condition of a parent entity, it should not be key (things of contained entities don't ask)

        else.

          cl_dd_ddl_annotation_service=>get_direct_annoval_4_element( exporting entityname = exact #( i_name )
                                                                                elementname = exact #( non_association_entry->get_alias_struct( )-name )
                                                                                annoname = 'OBJECTMODEL.FOREIGNKEY.ASSOCIATION'
                                                                      importing values = data(obj_model_fk_assoc) ).

          if not ( line_exists( obj_model_fk_assoc[ 1 ] ) ).

            primitive_property->set_is_nullable( ).

          endif.

          cl_dd_ddl_annotation_service=>get_direct_annoval_4_element( exporting entityname = exact #( i_name )
                                                                                elementname = exact #( non_association_entry->get_alias_struct( )-name )
                                                                                annoname = 'SEMANTICS.SYSTEMDATE.LASTCHANGEDAT'
                                                                      importing values = data(obj_model_last_changed) ).

          if line_exists( obj_model_last_changed[ 1 ] ).

            primitive_property->use_as_etag( ).

          endif.

        endif.

      endif.

    endloop.

    if i_model_associations eq abap_true.

      loop at select_list->get_entries( ) reference into data(select_entry_2). "change name

        if select_entry_2->*->get_type( ) eq cl_qlast_constants=>selectlist_entry_association.

          data(association) = cast cl_qlast_assoc_sl_entry( select_entry_2->* )->get_association( ).

          if association->is_exposed( ).

*            data(gw_association) = sadl_gw_cds_p_association=>get_association( association_entry->* ). don't really add much

            data(navigation) = entity_type->create_navigation_property( exact #( association->get_name( ) ) ).

            navigation->set_edm_name( exact #( association->get_target( )->get_alias_struct( )-name ) ).

            navigation->set_target_entity_type_name( exact #( association->get_target( )->get_name( ) && '_TY' ) ).

            navigation->set_target_multiplicity( cond #( let cardinality = association->get_cardinality( ) in
                                                         when cardinality-min eq 0
                                                              and cardinality-max eq 1
                                                         then /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one_optional
                                                         when cardinality-min eq 1
                                                              and cardinality-max eq 1
                                                         then /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_one
                                                         else /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ) ).

            if entity_set is bound
               and 1 eq 2. "it works for Orders/_Status | Orders/_Items doesn't work because items is contained and is not an entity set | OrderItems/_Header can't possibly work because there cannot be an entity set object

              entity_set->add_navigation_prop_binding( iv_navigation_property_path = association->get_name( )
                                                       iv_target_entity_set = exact #( association->get_target( )->get_name( ) ) ).

            endif.

            cl_dd_ddl_annotation_service=>get_direct_annoval_4_element( exporting entityname = exact #( i_name )
                                                                                  elementname = exact #( association->get_target( )->get_alias_struct( )-name )
                                                                                  annoname = 'OBJECTMODEL.ASSOCIATION.TYPE$1$'
                                                                        importing values = data(obj_model_assoc_type) ).

            if value #( obj_model_assoc_type[ 1 ] optional ) eq '#TO_COMPOSITION_CHILD'.

              navigation->set_is_containment_navigation( ).

              navigation->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-cascade ).

            elseif not ( value #( obj_model_assoc_type[ 1 ] optional ) eq '#TO_COMPOSITION_PARENT'
                         or value #( obj_model_assoc_type[ 1 ] optional ) eq '#TO_COMPOSITION_ROOT' ).

              if not ( navigation->get_target_multiplicity( ) eq /iwbep/if_v4_med_element=>gcs_med_nav_multiplicity-to_many_optional ).
                "try also with assoc that has more than one condition line (maybe gw_association->get_constraints( ) will come in handy)
                data(on_expression) = cast cl_qlast_unmanaged_association( association )->get_on( ).

                data(comparison_on_expression) = cast cl_qlast_comp_expression( on_expression ).

                data(comparison_left_side) = cast cl_qlast_atomic_expression( cast cl_qlast_comp_expression( on_expression )->get_left( ) ).

                data(comparison_right_side) = cast cl_qlast_atomic_expression( cast cl_qlast_comp_expression( on_expression )->get_right( ) ).

                navigation->add_referential_constraint( iv_source_property_path = comparison_left_side->get_identifier( )
                                                        iv_target_property_path = comparison_right_side->get_identifier( ) ).

              endif.

            endif.

            if composition_up_annotations is initial
               and obj_model_assoc_type is initial.

              cl_dd_ddl_annotation_service=>get_drct_annos_4_entity_elmnts( exporting entityname = exact #( association->get_target( )->get_name_struct( )-name )
                                                                            importing annos = data(target_assoc_element_annots) ).

              if not ( line_exists( target_assoc_element_annots[ value = '#TO_COMPOSITION_PARENT' ] )
                       or line_exists( target_assoc_element_annots[ value = '#TO_COMPOSITION_ROOT' ] ) ).

                entity_set->add_navigation_prop_binding( iv_navigation_property_path = association->get_name( )
                                                         iv_target_entity_set = exact #( association->get_target( )->get_name( ) ) ).

              endif.

            endif.

          endif.

        endif.

      endloop.

    endif.

  endmethod.
  method metadata_model.

    r_metadata_model = me->a_metadata_model.

  endmethod.

endclass.
