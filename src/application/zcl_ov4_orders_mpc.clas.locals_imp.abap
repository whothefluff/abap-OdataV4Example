*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class element implementation.

  method get_content.

  endmethod.
  method if_sadl_gw_cds_parser_element~is_transient.

  endmethod.
  method qlast_select.

    r_qlast_select = me->mo_qlast_node.

  endmethod.
  method view_definition.

*    r_view_definition = cast me->get_element( io_qlast_node = me->mo_qlast_node )->.

*    r_view_definition = me->mo_view_definition.

  endmethod.

endclass.
