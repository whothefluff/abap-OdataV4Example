*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
**********************************************************************
*** Expression Wrapper
**********************************************************************
CLASS lcl_expression_wrapper DEFINITION INHERITING FROM cl_qlast_stdselectlist_entry FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_expression TYPE REF TO cl_qlast_expression.
ENDCLASS.

CLASS lcl_expression_wrapper IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->m_expression = io_expression.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Literal
**********************************************************************
CLASS lcl_literal DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_qlast_node TYPE REF TO cl_qlast_stdselectlist_entry
      RAISING   cx_sadl_gw_cds_parser.

    METHODS: if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_content REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_expression TYPE REF TO cl_qlast_literal_expression.
ENDCLASS.

CLASS lcl_literal IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_qlast_node ).
    me->mo_expression ?= io_qlast_node->get_expression( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = abap_true.
  ENDMETHOD.

  METHOD get_content.
    rv_content = me->mo_expression->get_value( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Simple Element
**********************************************************************
CLASS lcl_simple_element DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_qlast_node TYPE REF TO cl_qlast_stdselectlist_entry
      RAISING   cx_sadl_gw_cds_parser.

    METHODS get_datasource
      IMPORTING iv_upper_case        TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_datasource) TYPE string.

    METHODS get_identifier
      IMPORTING iv_upper_case        TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_identifier) TYPE string.

    METHODS:
      if_sadl_gw_cds_parser_element~get_alias REDEFINITION,
      if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS get_content REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_expression TYPE REF TO cl_qlast_atomic_expression.
ENDCLASS.

CLASS lcl_simple_element IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_qlast_node ).
    me->mo_expression ?= io_qlast_node->get_expression( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~get_alias.
    rv_alias = super->if_sadl_gw_cds_parser_element~get_alias( ).
    IF rv_alias IS INITIAL.
      rv_alias = me->get_identifier( ).
    ENDIF.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = abap_false.
  ENDMETHOD.

  METHOD get_content.
    IF me->get_datasource( ) IS INITIAL.
      rv_content = me->get_identifier( ).
    ELSE.
      rv_content = |{ me->get_datasource( ) }.{ me->get_identifier( ) }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_datasource.
    rv_datasource = me->mo_expression->get_tablename( iv_upper_case ).
  ENDMETHOD.

  METHOD get_identifier.
    rv_identifier = me->mo_expression->get_identifier( iv_upper_case ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Path Element
**********************************************************************
CLASS lcl_path_element DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_qlast_node TYPE REF TO cl_qlast_stdselectlist_entry
      RAISING   cx_sadl_gw_cds_parser.

    METHODS get_identifier RETURNING VALUE(rv_identifier) TYPE string.

    METHODS:
      if_sadl_gw_cds_parser_element~get_alias REDEFINITION,
      if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS get_content REDEFINITION.

  PRIVATE SECTION.

    DATA: mo_expression TYPE REF TO cl_qlast_path_expression.
ENDCLASS.

CLASS lcl_path_element IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_qlast_node ).
    me->mo_expression ?= io_qlast_node->get_expression( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~get_alias.
    rv_alias = super->if_sadl_gw_cds_parser_element~get_alias( ).
    IF rv_alias IS INITIAL.
      rv_alias = me->get_identifier( ).
    ENDIF.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = abap_true.
  ENDMETHOD.

  METHOD get_content.
    DATA(lt_entries) = VALUE stringtab(
      BASE VALUE #( ( me->mo_expression->get_entity_name( )-name ) )
      FOR o_entry IN me->mo_expression->get_entries( )
      ( o_entry->get_name( upper_case = abap_false ) )
    ).

    DELETE lt_entries WHERE table_line IS INITIAL.
    rv_content = concat_lines_of( table = lt_entries  sep =  '.' ).
  ENDMETHOD.

  METHOD get_identifier.
    DATA(lt_entries) = me->mo_expression->get_entries( ).
    rv_identifier = lt_entries[ lines( lt_entries ) ]->get_name( upper_case = abap_false ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Cast Function
**********************************************************************
CLASS lcl_cast_function DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_qlast_node TYPE REF TO cl_qlast_stdselectlist_entry
      RAISING   cx_sadl_gw_cds_parser.

    METHODS get_cast_type RETURNING VALUE(rv_type) TYPE string.
    METHODS get_preserving_type_suffix RETURNING VALUE(rv_suffix) TYPE string.

    METHODS: if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_content REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_expression    TYPE REF TO cl_qlast_cast_expression,
          mo_child_element TYPE REF TO sadl_gw_cds_p_element.

ENDCLASS.

CLASS lcl_cast_function IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_qlast_node ).
    me->mo_expression ?= io_qlast_node->get_expression( ).
    me->mo_child_element = get_element( io_qlast_node = NEW lcl_expression_wrapper( me->mo_expression->get_expression( ) ) ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = me->mo_child_element->if_sadl_gw_cds_parser_element~is_transient( ).
  ENDMETHOD.

  METHOD get_cast_type.
    rv_type = me->mo_expression->get_typeref( )->get_typename( upper_case = abap_false ).
  ENDMETHOD.

  METHOD get_preserving_type_suffix.
*    rv_suffix = COND #( WHEN me->mo_expression->is_type_preserving( ) THEN | preserving type| ).
  ENDMETHOD.

  METHOD get_content.
    rv_content = |cast({ me->mo_child_element->get_content( ) } as { me->get_cast_type( ) }{ me->get_preserving_type_suffix( ) })|.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Simple Case
**********************************************************************
CLASS lcl_searched_case DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_qlast_node TYPE REF TO cl_qlast_stdselectlist_entry
      RAISING   cx_sadl_gw_cds_parser lcx_unsupported_element.

    METHODS: if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_content REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_source_element TYPE REF TO sadl_gw_cds_p_element,
          mo_then_element   TYPE REF TO sadl_gw_cds_p_element,
          mo_else_element   TYPE REF TO sadl_gw_cds_p_element.
ENDCLASS.

CLASS lcl_searched_case IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_qlast_node  ).

    "Only the simple "searched case"
    DATA(lo_case_expression) = CAST cl_qlast_case_exp_searched( io_qlast_node->get_expression( ) ).

    IF lo_case_expression->has_default( ) <> abap_true OR lo_case_expression->get_number_of_whens( ) <> 1.
      RAISE EXCEPTION TYPE lcx_unsupported_element.
    ENDIF.

    DATA(lt_case_when) = lo_case_expression->get_when_list( ).
    ASSERT lines( lt_case_when ) = 1.

    DATA(lo_when) = lt_case_when[ 1 ]->get_when( ).
    IF lo_when->get_type( ) <> cl_qlast_constants=>expressiontype_is_null.
      RAISE EXCEPTION TYPE lcx_unsupported_element.
    ENDIF.

    DATA(lo_source) = CAST cl_qlast_is_null_expression( lo_when )->get_left( ).
    DATA(lo_then) = lt_case_when[ 1 ]->get_then( ).
    DATA(lo_else) = lo_case_expression->get_default( ).

    me->mo_source_element = get_element( NEW lcl_expression_wrapper( lo_source ) ).
    me->mo_then_element   = get_element( NEW lcl_expression_wrapper( lo_then ) ).
    me->mo_else_element   = get_element( NEW lcl_expression_wrapper( lo_else ) ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = abap_true.
  ENDMETHOD.

  METHOD get_content.
    rv_content = |case when { me->mo_source_element->get_content( ) } is null then { me->mo_then_element->get_content( ) } else { me->mo_else_element->get_content( ) } end|.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Custom Function
**********************************************************************
CLASS lcl_custom_function DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_qlast_node TYPE REF TO cl_qlast_stdselectlist_entry
      RAISING   cx_sadl_gw_cds_parser.

    METHODS: if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_content REDEFINITION.

    TYPES: tt_element TYPE STANDARD TABLE OF REF TO sadl_gw_cds_p_element WITH DEFAULT KEY.

    DATA: mv_function_name TYPE string,
          mt_elements      TYPE tt_element.
ENDCLASS.

CLASS lcl_custom_function IMPLEMENTATION.
  METHOD constructor.
    super->constructor( io_qlast_node ).

    DATA(lo_function) = CAST cl_qlast_func_expression( io_qlast_node->get_expression( ) ).
    me->mv_function_name = lo_function->get_name( upper_case = abap_false ).

    me->mt_elements = VALUE #( FOR o_parameter IN lo_function->get_parameter( )
      ( get_element( NEW lcl_expression_wrapper( o_parameter ) ) )
    ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = abap_true.
  ENDMETHOD.

  METHOD get_content.
    DATA(lt_element_content) = VALUE stringtab( FOR o_element IN me->mt_elements ( o_element->get_content( ) ) ).
    rv_content = |{ me->mv_function_name }({ concat_lines_of( table = lt_element_content  sep = ',' ) })|.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Unsupported Element
**********************************************************************
CLASS lcl_unparsed_element DEFINITION INHERITING FROM sadl_gw_cds_p_element FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS: if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

  PROTECTED SECTION.
    METHODS: get_content REDEFINITION.
ENDCLASS.

CLASS lcl_unparsed_element IMPLEMENTATION.
  METHOD if_sadl_gw_cds_parser_element~is_transient.
    rv_is_transient = abap_true.
  ENDMETHOD.

  METHOD get_content.
    RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser_unsupp EXPORTING textid = cx_sadl_gw_cds_parser=>unsupported_sel_list_entry.
  ENDMETHOD.
ENDCLASS.
CLASS sadl_gw_cds_p_element IMPLEMENTATION.


  METHOD constructor.
    me->mo_qlast_node = io_qlast_node.
    me->mo_annotations = NEW cl_sadl_gw_cds_p_anno_cont( cl_sadl_gw_cds_p_annotation=>get_annotations( io_qlast_node->if_qlast_annotable~get_annotations( ) ) ).
  ENDMETHOD.


  METHOD find_element_alias.
    IF iv_datasource IS INITIAL AND line_exists( it_elements[ KEY alias COMPONENTS alias = to_upper( iv_name ) ] ).
      iv_datasource = if_sadl_gen_drft_generator=>cs_alias-projection.
    ENDIF.

    TRY.
        rv_alias = _find_element_alias(
            it_elements           = it_elements
            iv_datasource         = to_upper( iv_datasource )
            iv_name               = to_upper( iv_name )
        ).

      CATCH lcx_element_not_found.
        RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
          EXPORTING
            textid       = cx_sadl_gw_cds_parser=>element_not_found
            element_name = |{ iv_datasource }{ COND #( WHEN iv_datasource IS INITIAL THEN '' ELSE '.' ) }{ iv_name }|.
    ENDTRY.
  ENDMETHOD.  "#EC CI_VALPAR


  METHOD get_element.
    TRY.
        ro_element = SWITCH #( io_qlast_node->get_expression( )->get_type( )
          WHEN cl_qlast_constants=>expressiontype_literal         THEN NEW lcl_literal( io_qlast_node )
          WHEN cl_qlast_constants=>expressiontype_element         THEN NEW lcl_simple_element( io_qlast_node )
          WHEN cl_qlast_constants=>expressiontype_path            THEN NEW lcl_path_element( io_qlast_node )
          WHEN cl_qlast_constants=>expressiontype_cast_function   THEN NEW lcl_cast_function( io_qlast_node )
          WHEN cl_qlast_constants=>expressiontype_case_searched   THEN NEW lcl_searched_case( io_qlast_node )
          WHEN cl_qlast_constants=>expressiontype_custom_function THEN NEW lcl_custom_function( io_qlast_node )
          ELSE THROW lcx_unsupported_element( )
        ).

      CATCH lcx_unsupported_element.
        ro_element = NEW lcl_unparsed_element( io_qlast_node ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_elements.
    LOOP AT it_select_list INTO DATA(lo_entry).
      ASSERT lo_entry->get_type( ) = cl_qlast_constants=>selectlist_entry_std.
      DATA(lo_element) = get_element( io_qlast_node = CAST #( lo_entry ) ).

      DATA(lo_annotation) = lo_element->if_sadl_gw_cds_annotatable~get_annotation( 'AbapCatalog.internal.isMandt' ).
      IF lo_annotation IS BOUND AND lo_annotation->is_truthy( ).
        CONTINUE.
      ENDIF.

      INSERT VALUE #( alias = to_upper( lo_element->if_sadl_gw_cds_parser_element~get_alias( ) )  content = lo_element ) INTO TABLE rt_elements.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_annotatable~get_annotation.
    ro_annotation = me->mo_annotations->get_annotation( iv_identifier ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_annotatable~get_annotations.
    rt_annotations = me->mo_annotations->get_annotations( ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_element~get_alias.
    rv_alias = me->mo_qlast_node->get_alias( upper_case = abap_false ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_element~has_alias.
    rv_has_alias = xsdbool( me->mo_qlast_node->get_alias( ) IS NOT INITIAL ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_element~is_key.
    rv_is_key = me->mo_qlast_node->iskeyelement( ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_object~as_ddl_string.
    rv_ddl_string = COND #( WHEN me->if_sadl_gw_cds_parser_element~is_key( )  THEN |key | )
                 && COND #( WHEN iv_simplified <> abap_true                   THEN |{ me->get_content( ) } as | )
                 && me->if_sadl_gw_cds_parser_element~get_alias( ).
  ENDMETHOD.
  METHOD _find_element_alias.
    IF iv_datasource = to_upper( if_sadl_gen_drft_generator=>cs_alias-projection ).
      TRY.
          rv_alias = it_elements[ KEY alias COMPONENTS alias = iv_name ]-content->get_alias( ).
          RETURN.

        CATCH cx_sy_itab_line_not_found.
          RAISE EXCEPTION TYPE lcx_element_not_found.
      ENDTRY.
    ENDIF.

    LOOP AT it_elements ASSIGNING FIELD-SYMBOL(<s_element>).
      CHECK <s_element>-content IS INSTANCE OF lcl_simple_element.
      DATA(lo_element) = CAST lcl_simple_element( <s_element>-content ).
      DATA(lv_datasource) = lo_element->get_datasource( abap_true ).

      IF iv_name = lo_element->get_identifier( abap_true ) AND ( iv_datasource IS INITIAL OR lv_datasource IS INITIAL OR iv_datasource = lv_datasource ).
        rv_alias = lo_element->if_sadl_gw_cds_parser_element~get_alias( ).
        RETURN.
      ENDIF.
    ENDLOOP.

    RAISE EXCEPTION TYPE lcx_element_not_found.
  ENDMETHOD.
  METHOD annotations.

    r_annotations = me->mo_annotations.

  ENDMETHOD.
  METHOD expression.

    assign me->('MO_EXPRESSION') to field-symbol(<exp>).

    if sy-subrc eq 0.

      r_expression = <exp>.

    endif.

  ENDMETHOD.
  METHOD qlast_node.

    r_qlast_node = me->mo_qlast_node.

  ENDMETHOD.

ENDCLASS.
**********************************************************************
*** Invalid association
**********************************************************************
CLASS lcl_unparsed_association DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_sadl_gw_cds_parser_assoc.
    METHODS constructor
      IMPORTING io_association      TYPE REF TO cl_qlast_association
                ix_parser_exception TYPE REF TO cx_sadl_gw_cds_parser.

  PROTECTED SECTION.
    METHODS on_unsupported_operation RAISING cx_sadl_gw_cds_parser_unsupp.

    DATA: mo_qlast_association TYPE REF TO cl_qlast_association,
          mx_parser_exception  TYPE REF TO cx_sadl_gw_cds_parser,
          mv_alias             TYPE        string,
          mv_is_exposed        TYPE        abap_bool,
          mo_annotations       TYPE REF TO if_sadl_gw_cds_annotatable.
ENDCLASS.

CLASS lcl_unparsed_association IMPLEMENTATION.
  METHOD constructor.
    me->mo_qlast_association  = io_association.
    me->mx_parser_exception   = ix_parser_exception.
    me->mv_alias              = me->mo_qlast_association->get_name( upper_case = abap_false ).
    me->mv_is_exposed         = abap_false.
  ENDMETHOD.

  METHOD on_unsupported_operation.
    RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser_unsupp EXPORTING previous = me->mx_parser_exception.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~get_alias.
    rv_alias = me->mv_alias.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_annotatable~get_annotation.
    IF me->mo_annotations IS BOUND.
      ro_annotation = me->mo_annotations->get_annotation( iv_identifier  ).
    ENDIF.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_annotatable~get_annotations.
    IF me->mo_annotations IS BOUND.
      rt_annotations = me->mo_annotations->get_annotations( ).
    ENDIF.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~get_cardinality.
    rs_cardinality = me->mo_qlast_association->get_cardinality( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~get_connective_operator.
    me->on_unsupported_operation( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~get_constraints.
    me->on_unsupported_operation( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~get_target.
    rv_target = me->mo_qlast_association->get_target( )->get_name( upper_case = abap_false ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~is_exposed.
    rv_is_exposed = xsdbool( iv_strict_mode = abap_true AND me->mv_is_exposed = abap_true ) .
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_object~as_ddl_string.
    me->on_unsupported_operation( ).
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~inherit.
    DATA(lo_association) = NEW lcl_unparsed_association(
        io_association      = me->mo_qlast_association
        ix_parser_exception = me->mx_parser_exception
    ).
    ro_association            = lo_association.
    lo_association->mv_alias  = me->mv_alias.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~mark_as_exposed.
    ASSERT me->mv_is_exposed <> abap_true.

    me->mv_is_exposed   = abap_true.
    me->mv_alias        = iv_exposure_alias.
    me->mo_annotations  = io_annotations.
  ENDMETHOD.

  METHOD if_sadl_gw_cds_parser_assoc~get_source_fields.
    me->on_unsupported_operation( ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Element
**********************************************************************
CLASS lcl_join_element DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_join_field.

    METHODS constructor
      IMPORTING
        iv_association_alias TYPE        string OPTIONAL
        io_expression        TYPE REF TO cl_qlast_expression.

  PROTECTED SECTION.
    DATA: mv_association_alias         TYPE string,
          mv_exposed_association_alias TYPE string,
          mv_datasource                TYPE string,
          mv_alias                     TYPE string,
          mo_element                   TYPE REF TO cl_qlast_atomic_expression.

ENDCLASS.

CLASS lcl_join_element IMPLEMENTATION.
  METHOD constructor.
    me->mo_element                    = CAST cl_qlast_atomic_expression( io_expression ).
    me->mv_association_alias          = iv_association_alias.
    me->mv_exposed_association_alias  = me->mv_association_alias.
    me->mv_datasource                 = me->mo_element->get_tablename( upper_case = abap_false ).
    me->mv_alias                      = me->mo_element->get_identifier( upper_case = abap_false ).
  ENDMETHOD.

  METHOD lif_join_field~is_source.
    rv_is_source = xsdbool( NOT me->lif_join_field~is_target( ) ).
  ENDMETHOD.

  METHOD lif_join_field~is_target.
    rv_is_target = xsdbool( me->mv_association_alias = me->mv_datasource ).
  ENDMETHOD.

  METHOD lif_join_field~get_type.
    rv_type = cl_qlast_constants=>expressiontype_element.
  ENDMETHOD.

  METHOD lif_join_field~get_qualified_name.
    IF me->lif_join_field~is_target( ) = abap_true.
      rv_name = |{ me->mv_exposed_association_alias }.{ me->mv_alias }|.
    ELSE.
      rv_name = |{ if_sadl_gen_drft_generator=>cs_alias-projection }.{ me->mv_alias }|.
    ENDIF.
  ENDMETHOD.

  METHOD lif_join_field~set_exposure.
    me->mv_exposed_association_alias = iv_exposed_association_alias.

    IF me->lif_join_field~is_target( ).
      RETURN.
    ENDIF.

    TRY.
        me->mv_alias = cl_sadl_gw_cds_p_element=>find_element_alias(
           it_elements           = it_projection_elements
           iv_datasource         = me->mv_datasource
           iv_name               = me->mv_alias
       ).
      CATCH cx_sadl_gw_cds_parser INTO DATA(lx_exception).
        RAISE EXCEPTION TYPE cx_fatal_exception EXPORTING previous = lx_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD lif_join_field~inherit.
    DATA(lo_join_field) = NEW lcl_join_element( me->mo_element ).
    ro_join_field = lo_join_field.

    lo_join_field->mv_association_alias          = me->mv_association_alias.
    lo_join_field->mv_exposed_association_alias  = me->mv_exposed_association_alias.
    lo_join_field->mv_alias                      = me->mv_alias.

    lo_join_field->mv_datasource = COND #( WHEN me->lif_join_field~is_target( )
      THEN me->mv_datasource
      ELSE iv_datasource_name
    ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Constants Expressions (abstract)
**********************************************************************
CLASS lcl_constant_expression DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_join_field
      ABSTRACT METHODS
      get_qualified_name
      get_type.

    METHODS constructor
      IMPORTING
        iv_association_alias TYPE        string OPTIONAL
        io_expression        TYPE REF TO cl_qlast_expression.

  PROTECTED SECTION.
    DATA: mo_expression TYPE REF TO cl_qlast_expression.
ENDCLASS.

CLASS lcl_constant_expression IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->mo_expression = io_expression.
  ENDMETHOD.

  METHOD lif_join_field~is_source.
    rv_is_source = abap_false.
  ENDMETHOD.

  METHOD lif_join_field~is_target.
    rv_is_target = abap_false.
  ENDMETHOD.

  METHOD lif_join_field~set_exposure.
    RETURN.
  ENDMETHOD.

  METHOD lif_join_field~inherit.
    ro_join_field = me.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Literal
**********************************************************************
CLASS lcl_join_literal DEFINITION INHERITING FROM lcl_constant_expression FINAL.
  PUBLIC SECTION.
    METHODS:
      lif_join_field~get_qualified_name REDEFINITION,
      lif_join_field~get_type REDEFINITION.
ENDCLASS.

CLASS lcl_join_literal IMPLEMENTATION.
  METHOD lif_join_field~get_qualified_name.
    rv_name = CAST cl_qlast_literal_expression( me->mo_expression )->get_value( ).
  ENDMETHOD.

  METHOD lif_join_field~get_type.
    rv_type = cl_qlast_constants=>expressiontype_literal.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Session Variable
**********************************************************************
CLASS lcl_session_variable DEFINITION INHERITING FROM lcl_constant_expression FINAL.
  PUBLIC SECTION.
    METHODS:
      lif_join_field~get_qualified_name REDEFINITION,
      lif_join_field~get_type REDEFINITION.
ENDCLASS.

CLASS lcl_session_variable IMPLEMENTATION.
  METHOD lif_join_field~get_qualified_name.
    rv_name = CAST cl_qlast_session_variable_expr( me->mo_expression )->get_name( upper_case = abap_false ).
  ENDMETHOD.

  METHOD lif_join_field~get_type.
    rv_type = cl_qlast_constants=>expressiontype_session_var.
  ENDMETHOD.
ENDCLASS.
CLASS sadl_gw_cds_p_association IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    IF io_association->get_type( ) <> cl_qlast_constants=>association_type_unmanaged.
      RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
        EXPORTING
          textid     = cx_sadl_gw_cds_parser=>unsupported_association
          assoc_name = me->mv_alias.
    ENDIF.

    me->mo_qlast_association ?= io_association.

    IF me->mo_qlast_association->get_on( ) IS NOT BOUND .
      RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
        EXPORTING
          textid     = cx_sadl_gw_cds_parser=>unsupported_association
          assoc_name = me->mv_alias.
    ENDIF.

    IF iv_inheritance = abap_true.
      "Association is inherited -> Members are filled by inheritance logic
      RETURN.
    ENDIF.

    me->mv_alias = me->mo_qlast_association->get_name( upper_case = abap_false ).
    me->parse_condition( me->mo_qlast_association->get_on( ) ).
  ENDMETHOD.


  METHOD extract_assoc_element.
    ro_expression = COND #(
      LET o_expression      = CAST cl_qlast_assoc_on_element( io_expression )
          o_sel_list_entry  = CAST cl_qlast_stdselectlist_entry( o_expression->get_selectlist_entry( ) )

      IN WHEN o_sel_list_entry IS NOT INITIAL
      THEN o_sel_list_entry->get_expression( )
      ELSE THROW cx_sadl_gw_cds_parser( textid = cx_sadl_gw_cds_parser=>unsupported_association  assoc_name = me->if_sadl_gw_cds_parser_assoc~get_alias( ) )
    ).
  ENDMETHOD.


  METHOD get_associations.
    LOOP AT io_qlast_node->get_entries( ) INTO DATA(lo_qlast_association).
      DATA(ls_association) = VALUE if_sadl_gw_cds_parser_assoc=>ts_association( ).
      DATA(lv_alias) = lo_qlast_association->get_name( upper_case = abap_false ).

      TRY.
          " changed for downport
          IF CAST cl_qlast_unmanaged_association( lo_qlast_association )->get_on( ) IS BOUND.
            ls_association-alias = to_upper( lv_alias ).
            ls_association-content = NEW sadl_gw_cds_p_association( lo_qlast_association ).

          ELSE.
            ls_association-alias = to_upper( it_exposure[ alias = lv_alias ]-name ).
*            ls_association-content = inherit_association(
*              it_exposure           = it_exposure
*              io_qlast_association  = lo_qlast_association
*              iv_alias              = lv_alias
*            ).
*            ASSERT ls_association-alias = to_upper( ls_association-content->get_alias( ) ).
            RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser.
          ENDIF.

        CATCH cx_sadl_gw_cds_parser INTO DATA(lx_exception).
          ls_association-content = NEW lcl_unparsed_association(
              io_association      = lo_qlast_association
              ix_parser_exception = lx_exception
          ).
      ENDTRY.

      ASSERT ls_association-alias IS NOT INITIAL.
      INSERT ls_association INTO TABLE rt_associations.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_join_field.
    ro_join_field = SWITCH #( LET v_alias = me->if_sadl_gw_cds_parser_assoc~get_alias( ) IN io_expression->get_type( )
      WHEN cl_qlast_constants=>expressiontype_element
      THEN NEW lcl_join_element( iv_association_alias = v_alias  io_expression = io_expression )

      WHEN cl_qlast_constants=>expressiontype_literal
      THEN NEW lcl_join_literal( iv_association_alias = v_alias  io_expression = io_expression )

      WHEN cl_qlast_constants=>expressiontype_session_var
      THEN NEW lcl_session_variable( iv_association_alias = v_alias  io_expression = io_expression )

      WHEN cl_qlast_constants=>expressiontype_assoc_elem
      THEN me->get_join_field( me->extract_assoc_element( io_expression ) )

      ELSE THROW cx_sadl_gw_cds_parser(
        textid          = cx_sadl_gw_cds_parser=>unsupported_association
        assoc_name      = v_alias
        expression_type = io_expression->get_type( )
      )
    ).
  ENDMETHOD.


  METHOD get_parser_instance.
    TRY.
        ro_parser = cl_sadl_gw_cds_parser_factory=>get_instance( iv_cds_view_name ).
      CATCH cx_sadl_gen_drft_generator INTO DATA(lx_exception).
        RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser EXPORTING previous = lx_exception.
    ENDTRY.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_annotatable~get_annotation.
    IF me->mo_annotations IS BOUND.
      ro_annotation = me->mo_annotations->get_annotation( iv_identifier ).
    ENDIF.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_annotatable~get_annotations.
    IF me->mo_annotations IS BOUND.
      rt_annotations = me->mo_annotations->get_annotations( ).
    ENDIF.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~get_alias.
    rv_alias = me->mv_alias.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~get_cardinality.
    rs_cardinality = me->mo_qlast_association->get_cardinality( ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~get_constraints.
    rt_constraints = VALUE #( FOR s_constraint IN me->mt_constraints (
        is_negation  = s_constraint-is_negation
        source_type  = s_constraint-source_field->get_type( )
        source_field = s_constraint-source_field->get_qualified_name( )
        operator     = s_constraint-operator
        target_type  = s_constraint-target_field->get_type( )
        target_field = s_constraint-target_field->get_qualified_name( )
    ) ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~get_source_fields.
    LOOP AT me->mt_constraints ASSIGNING FIELD-SYMBOL(<s_constraint>).
      CHECK <s_constraint>-source_field->is_source( ).
      INSERT segment(
        val   = <s_constraint>-source_field->get_qualified_name( )
        sep   = '.'
        index = 2
      ) INTO TABLE rt_fields.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~get_target.
    rv_target = me->mo_qlast_association->get_target( )->get_name( upper_case = abap_false ).
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~inherit.
    DATA: lo_association TYPE REF TO sadl_gw_cds_p_association.

    TRY.
        lo_association = NEW sadl_gw_cds_p_association(
            io_association        = me->mo_qlast_association
            iv_inheritance        = abap_true
        ).
        ro_association = lo_association.

        lo_association->mv_is_exposed   = abap_false.
        lo_association->mv_alias        = me->mv_alias.
        lo_association->set_connective_operator( me->if_sadl_gw_cds_parser_assoc~get_connective_operator( ) ).

      CATCH cx_sadl_gw_cds_parser INTO DATA(lx_exception).
        "Must not occur
        RAISE EXCEPTION TYPE cx_fatal_exception EXPORTING previous = lx_exception.
    ENDTRY.

    DATA(lv_datasource) = io_source->get_alias( upper_case = abap_false ).

    IF lv_datasource IS INITIAL.
      lv_datasource = io_source->get_name( upper_case = abap_false ).

    ELSEIF lv_datasource(1) = '='.
      "Deep associations
      RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
        EXPORTING
          textid     = cx_sadl_gw_cds_parser=>unsupported_association
          assoc_name = me->mv_alias.
    ENDIF.

    lo_association->mt_constraints  = VALUE #( FOR s_constraint IN me->mt_constraints (
        is_negation   = s_constraint-is_negation
        source_field  = s_constraint-source_field->inherit( lv_datasource )
        operator      = s_constraint-operator
        target_field  = s_constraint-target_field->inherit( lv_datasource )
     ) ) .
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~is_exposed.
    rv_is_exposed = me->mv_is_exposed.
  ENDMETHOD.


  METHOD if_sadl_gw_cds_parser_assoc~mark_as_exposed.
    ASSERT me->if_sadl_gw_cds_parser_assoc~is_exposed( ) <> abap_true.

    me->mv_is_exposed = abap_true.
    me->mv_alias = iv_exposure_alias.

    LOOP AT me->mt_constraints ASSIGNING FIELD-SYMBOL(<s_constraint>).
      <s_constraint>-source_field->set_exposure(
          iv_exposed_association_alias    = me->mv_alias
          it_projection_elements          = it_projection_elements
      ).
      <s_constraint>-target_field->set_exposure(
          iv_exposed_association_alias    = me->mv_alias
          it_projection_elements          = it_projection_elements
      ).
    ENDLOOP.

    me->mo_annotations = io_annotations.
  ENDMETHOD.


  METHOD inherit_association.
    ASSERT 1 = 0.
*    DATA(lo_source) = io_qlast_association->get_source( ).
*    DATA(lv_source_name) = lo_source->get_name( upper_case = abap_false ).
*
*    DATA(lo_parser) = get_parser_instance( lv_source_name ).
*    DATA(lt_associations) = lo_parser->get_associations( ).
*
*    DATA(lv_name) = it_exposure[ alias = iv_alias ]-name.
*
*    TRY.
*        DATA(ls_association) = lt_associations[ KEY alias COMPONENTS alias = lv_name ].
*
*      CATCH cx_sy_itab_line_not_found.
*        " Association is not valid is source-view -> does not appear here
*        RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
*          EXPORTING
*            textid     = cx_sadl_gw_cds_parser=>unsupported_association
*            assoc_name = iv_alias.
*    ENDTRY.
*
*    TRY.
*        ro_content  = ls_association-content->inherit( lo_source ).
*
*      CATCH cx_sadl_gw_cds_parser INTO DATA(lx_exception).
*        lx_exception->assoc_name = iv_alias.
*        RAISE EXCEPTION lx_exception.
*    ENDTRY.
  ENDMETHOD.


  METHOD parse_condition.
    DATA(lv_expression_type) = io_expression->get_type( ).
    CASE lv_expression_type.
      WHEN cl_qlast_constants=>expressiontype_and OR cl_qlast_constants=>expressiontype_or.
        me->set_connective_operator( lv_expression_type ).

        DATA(lo_bool_expression) = CAST cl_qlast_boolean_expression( io_expression ).
        LOOP AT lo_bool_expression->get_conditions( ) INTO DATA(lo_expression).
          me->parse_condition( lo_expression ).
        ENDLOOP.

      WHEN cl_qlast_constants=>expressiontype_not.
        DATA(lo_not_expression) = CAST cl_qlast_not_expression( io_expression ).
        me->parse_constraint(
            io_expression  = CAST #( lo_not_expression->get_condition( ) )
            iv_is_negation = abap_true
        ).

      WHEN OTHERS.
        IF operator_is_supported( lv_expression_type ).
          me->parse_constraint( CAST #( io_expression ) ).
        ELSE.
          RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
            EXPORTING
              textid     = cx_sadl_gw_cds_parser=>unsupported_association
              assoc_name = me->mv_alias.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD parse_constraint.
    DATA(lo_left_field) = me->get_join_field( io_expression->get_left( ) ).
    DATA(lo_right_field) = me->get_join_field( io_expression->get_right( ) ).

    DATA(lv_uses_target) = lo_left_field->is_target( ) && lo_right_field->is_target( ). "EITHER left OR right-field is target
    IF lv_uses_target <> abap_true.
      RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
        EXPORTING
          textid     = cx_sadl_gw_cds_parser=>unsupported_association
          assoc_name = me->if_sadl_gw_cds_parser_assoc~get_alias( ).
    ENDIF.

    INSERT VALUE #( is_negation  = iv_is_negation ) INTO TABLE me->mt_constraints ASSIGNING FIELD-SYMBOL(<s_constraint>).

    IF lo_left_field->is_target( ) = abap_true.
      <s_constraint>-target_field = lo_left_field.
      <s_constraint>-source_field = lo_right_field.
      <s_constraint>-operator     = get_operator( io_expression->get_type( ) ).
    ELSE.
      <s_constraint>-target_field = lo_right_field.
      <s_constraint>-source_field = lo_left_field.
      <s_constraint>-operator     = get_mirror_operator( io_expression->get_type( ) ).
    ENDIF.
  ENDMETHOD.
  METHOD qlast_association.

    r_qlast_association = me->mo_qlast_association.

  ENDMETHOD.
  METHOD get_association.

    TRY.
          IF CAST cl_qlast_unmanaged_association( io_qlast_node )->get_on( ) IS BOUND.
            r_association = NEW sadl_gw_cds_p_association( io_qlast_node ).

          ELSE.
            RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser.
          ENDIF.

        CATCH cx_sadl_gw_cds_parser INTO DATA(lx_exception).
          r_association = NEW lcl_unparsed_association( io_association = io_qlast_node
                                                                 ix_parser_exception = lx_exception ).
      ENDTRY.

  ENDMETHOD.

ENDCLASS.
CLASS cds_view_factory IMPLEMENTATION.

  METHOD from_cds_name.

    r_ddl_source = me->read_ddl_source( to_upper( iv_cds_view ) ).

  ENDMETHOD.

ENDCLASS.
CLASS view_definition_factory IMPLEMENTATION.
  METHOD _parse_ddl_source.
    ro_view_definition = COND #(
      LET o_qlast_statement = _get_ddl_statement( iv_ddl_source = iv_ddl_source  iv_semantic_check = iv_semantic_check ) IN
      WHEN o_qlast_statement->get_type( ) = cl_qlast_constants=>ddlstmt_type_view_definition
      THEN CAST #( o_qlast_statement )
      ELSE THROW cx_sadl_gw_cds_parser( textid     = cx_sadl_gw_cds_parser=>invalid_cds_entity )
    ).
  ENDMETHOD.
  METHOD from_ddl_source.

    data(parser) = new cl_ddl_parser( ).

    r_view_definition = _parse_ddl_source( iv_ddl_source = i_ddl_source
                                           iv_semantic_check = abap_true ).

  ENDMETHOD.
METHOD _get_ddl_statement.
    CONSTANTS co_ddl_bitset TYPE i VALUE 2.
    CONSTANTS co_cds_bitset TYPE i VALUE 1.

    TRY.
        IF iv_semantic_check = abap_true.
          ro_qlast_statement = a_ddl_parser->parse_ddl(
            source         = iv_ddl_source
            semantic_check = abap_true
            bitset         = co_ddl_bitset
          ).

        ELSE.
          DATA(ls_errors) = VALUE cx_ddl_parser_exception=>error_info( ).
          ro_qlast_statement = a_ddl_parser->parse_cds(
            EXPORTING
              it_sources = VALUE #( ( source = iv_ddl_source ) )
              iv_bitset  = co_cds_bitset
            IMPORTING
              errors     = ls_errors
          ).

          IF ro_qlast_statement IS NOT BOUND.
            DATA(lx_ddl_parser) = NEW cx_ddl_parser_exception( ).
            lx_ddl_parser->set_error_info( ls_errors ).
            RAISE EXCEPTION lx_ddl_parser.
          ENDIF.
        ENDIF.

      CATCH cx_ddl_parser_exception INTO DATA(lx_exception).
        RAISE EXCEPTION TYPE cx_sadl_gw_cds_parser
          EXPORTING
            textid   = cx_sadl_gw_cds_parser=>ddl_parsing_error
            previous = lx_exception.
    ENDTRY.
endmethod.
  METHOD constructor.

   a_ddl_parser = new #( ).

  ENDMETHOD.

ENDCLASS.
