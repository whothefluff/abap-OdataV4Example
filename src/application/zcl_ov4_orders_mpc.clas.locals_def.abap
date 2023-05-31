*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
class lcx_unsupported_element definition
                              inheriting from cx_static_check.

endclass.
class lcx_element_not_found definition
                            inheriting from cx_static_check.

endclass.
class sadl_gw_cds_p_element definition
                            abstract
                            create protected.

  public section.

    interfaces: if_sadl_gw_cds_annotatable,
                if_sadl_gw_cds_parser_element
                  abstract methods
                    is_transient,
                if_sadl_gw_cds_parser_object.

    methods qlast_node
              returning
                value(r_qlast_node) type ref to cl_qlast_stdselectlist_entry.

    methods annotations
              returning
                value(r_annotations) type ref to if_sadl_gw_cds_annotatable.

    methods expression
              returning
                value(r_expression) type ref to CL_QLAST_EXPRESSION.

    class-methods get_elements
                    importing
                      !it_select_list type cl_qlast_selectlist=>entry_table_type
                    returning
                      value(rt_elements) type if_sadl_gw_cds_parser_element~tt_element
                    raising
                      cx_sadl_gw_cds_parser.

    class-methods find_element_alias
                    importing
                      !it_elements type if_sadl_gw_cds_parser_element~tt_element
                      value(iv_datasource) type string optional
                      value(iv_name) type string
                    returning
                      value(rv_alias) type string
                    raising
                      cx_sadl_gw_cds_parser.

    class-methods get_element
                    importing
                      !io_qlast_node type ref to cl_qlast_stdselectlist_entry
                    returning
                      value(ro_element) type ref to sadl_gw_cds_p_element
                    raising
                      cx_sadl_gw_cds_parser.

  protected section.

    data mo_qlast_node type ref to cl_qlast_stdselectlist_entry.

    data mo_annotations type ref to if_sadl_gw_cds_annotatable.

    methods constructor
              importing
                !io_qlast_node type ref to cl_qlast_stdselectlist_entry
              raising
                cx_sadl_gw_cds_parser.

    methods get_content
              abstract
              returning
                value(rv_content) type string
              raising
                cx_sadl_gw_cds_parser_unsupp.

  private section.

    class-methods _find_element_alias
                    importing
                      !it_elements type if_sadl_gw_cds_parser_element~tt_element
                      !iv_datasource type string
                      !iv_name type string
                    returning
                      value(rv_alias) type string
                    raising
                      lcx_element_not_found.

endclass.
INTERFACE lif_join_field.
  METHODS is_source
    RETURNING VALUE(rv_is_source) TYPE abap_bool.

  METHODS is_target
    RETURNING VALUE(rv_is_target) TYPE abap_bool.

  METHODS get_type
    RETURNING VALUE(rv_type) TYPE qlast_expression_type.

  METHODS get_qualified_name
    RETURNING VALUE(rv_name) TYPE string.

  METHODS inherit
    IMPORTING iv_datasource_name   TYPE string
    RETURNING VALUE(ro_join_field) TYPE REF TO lif_join_field.

  METHODS set_exposure
    IMPORTING iv_exposed_association_alias TYPE string
              it_projection_elements       TYPE if_sadl_gw_cds_parser_element=>tt_element.
ENDINTERFACE.
CLASS sadl_gw_cds_p_association DEFINITION
                                INHERITING FROM cl_sadl_gw_cds_p_assoc_base
                                CREATE PRIVATE.

  PUBLIC SECTION.

    methods qlast_association
              returning
                value(r_qlast_association) type ref to cl_qlast_unmanaged_association.

    CLASS-METHODS get_associations
      IMPORTING
        !io_qlast_node         TYPE REF TO cl_qlast_associations
        !it_exposure           TYPE if_sadl_gw_cds_parser_assoc~tt_exposure
      RETURNING
        VALUE(rt_associations) TYPE if_sadl_gw_cds_parser_assoc~tt_association
      RAISING
        cx_sadl_gw_cds_parser .

    CLASS-METHODS get_association
      IMPORTING
        !io_qlast_node         TYPE REF TO cl_qlast_association
      RETURNING
        VALUE(r_association) TYPE ref to if_sadl_gw_cds_parser_assoc
      RAISING
        cx_sadl_gw_cds_parser .

    METHODS if_sadl_gw_cds_annotatable~get_annotation
        REDEFINITION .
    METHODS if_sadl_gw_cds_annotatable~get_annotations
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~get_alias
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~get_cardinality
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~get_constraints
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~get_target
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~inherit
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~is_exposed
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~mark_as_exposed
        REDEFINITION .
    METHODS if_sadl_gw_cds_parser_assoc~get_source_fields
        REDEFINITION .
  PROTECTED SECTION.

    DATA mo_qlast_association TYPE REF TO cl_qlast_unmanaged_association .
    DATA mv_is_exposed TYPE abap_bool .
    DATA mv_alias TYPE string .
    DATA mo_annotations TYPE REF TO if_sadl_gw_cds_annotatable .

    METHODS parse_constraint
      IMPORTING
        !io_expression  TYPE REF TO cl_qlast_comp_expression
        !iv_is_negation TYPE abap_bool DEFAULT abap_false
      RAISING
        cx_sadl_gw_cds_parser .
    CLASS-METHODS inherit_association
      IMPORTING
        !it_exposure          TYPE sadl_gw_cds_p_association=>if_sadl_gw_cds_parser_assoc~tt_exposure
        !io_qlast_association TYPE REF TO cl_qlast_association
        !iv_alias             TYPE string
      RETURNING
        VALUE(ro_content)     TYPE if_sadl_gw_cds_parser_assoc=>ts_association-content
      RAISING
        cx_sadl_gw_cds_parser .
    METHODS constructor
      IMPORTING
        !io_association TYPE REF TO cl_qlast_association
        !iv_inheritance TYPE abap_bool DEFAULT abap_false
      RAISING
        cx_sadl_gw_cds_parser .
    CLASS-METHODS get_parser_instance
      IMPORTING
        !iv_cds_view_name TYPE string
      RETURNING
        VALUE(ro_parser)  TYPE REF TO if_sadl_gw_cds_parser
      RAISING
        cx_sadl_gw_cds_parser .
    METHODS parse_condition
      IMPORTING
        !io_expression TYPE REF TO cl_qlast_expression
      RAISING
        cx_sadl_gw_cds_parser .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ts_l_constraint,
        target_field TYPE REF TO lif_join_field,
        source_field TYPE REF TO lif_join_field,
        operator     TYPE        if_sadl_gw_cds_parser_assoc~ty_operator,
        is_negation  TYPE        abap_bool,
      END OF ts_l_constraint .
    TYPES:
      tt_l_constraint TYPE STANDARD TABLE OF ts_l_constraint WITH DEFAULT KEY .

    DATA mt_constraints TYPE tt_l_constraint .

    METHODS extract_assoc_element
      IMPORTING
        !io_expression       TYPE REF TO cl_qlast_expression
      RETURNING
        VALUE(ro_expression) TYPE REF TO cl_qlast_expression
      RAISING
        cx_sadl_gw_cds_parser .
    METHODS get_join_field
      IMPORTING
        !io_expression       TYPE REF TO cl_qlast_expression
      RETURNING
        VALUE(ro_join_field) TYPE REF TO lif_join_field
      RAISING
        cx_sadl_gw_cds_parser .
ENDCLASS.
class cds_view_factory definition
                          create public
                          inheriting from CL_SADL_GW_CDS_FACTORY.

  public section.

    methods from_cds_name
              importing
                !IV_CDS_VIEW type STRING
              returning
                value(r_ddl_source) type string.

endclass.
class view_definition_factory definition
                          create public.

  public section.

  methods constructor.

  methods _GET_DDL_STATEMENT
    importing
      !IV_DDL_SOURCE type STRING
      !IV_SEMANTIC_CHECK type ABAP_BOOL
    returning
      value(RO_QLAST_STATEMENT) type ref to CL_QLAST_DDLSTMT
    raising
      CX_SADL_GW_CDS_PARSER .
  methods _PARSE_DDL_SOURCE
    importing
      !IV_DDL_SOURCE type STRING
      !IV_SEMANTIC_CHECK type ABAP_BOOL
    returning
      value(RO_VIEW_DEFINITION) type ref to CL_QLAST_VIEW_DEFINITION
    raising
      CX_SADL_GW_CDS_PARSER .

    methods from_ddl_source
              importing
                !i_ddl_source type STRING
              returning
                value(r_view_definition) type ref to cl_qlast_view_definition .

  protected section.

    data a_ddl_parser type ref to cl_ddl_parser.

endclass.
