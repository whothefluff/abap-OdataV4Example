*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
class element definition
              create public
              inheriting from cl_sadl_gw_cds_p_element
              friends cl_sadl_gw_cds_p_element.

  public section.

    METHODS: if_sadl_gw_cds_parser_element~is_transient REDEFINITION.

    methods qlast_select
              returning
                value(r_qlast_select) type ref to cl_qlast_stdselectlist_entry .

    methods view_definition
              returning
                value(r_view_definition) type ref to cl_qlast_view_definition.

  PROTECTED SECTION.
    METHODS: get_content REDEFINITION.

endclass.
