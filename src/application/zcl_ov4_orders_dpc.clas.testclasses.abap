*"* use this source file for your ABAP unit test classes
class _test definition
            final
            for testing
            duration short
            risk level harmless.

  private section.

    "! GET $count and $filter sets filtered count
    "! <br/> URI of form: service/0001/entity/$count?$filter=conditions
    methods cnt_n_fltr_sets_filtered_count for testing.

    "! GET $count sets count
    "! <br/> URI of form: service/0001/entity/$count
    methods cnt_sets_count for testing.

    "! GET entity with $count returns collection and count
    "! <br/> URI of form: service/0001/entity?$count=true
    methods nty_n_cnt_rets_col_n_sets_cnt for testing.

    "! GET entity with $select returns collection in adapted json
    "! <br/> URI of form: service/0001/entity?$select=property1,property2
    methods nty_n_slct_rets_short_col for testing.

endclass.
class _test implementation.

  method cnt_n_fltr_sets_filtered_count.

    data(client_proxy) = /iwfnd/cl_sutil_client_proxy=>get_instance( ).

    data(req_headers) = value /iwfnd/sutil_property_t( ( name = if_http_header_fields_sap=>request_method
                                                         value = `GET` )
                                                       ( name = if_http_header_fields_sap=>request_uri
                                                         value = `/sap/opu/odata4/sap/zodatav4/default/sap/zov4_orders/0001/ZI_OV4_Orders/$count?$filter=ExternalId ne '141'` ) ).

    client_proxy->web_request( exporting it_request_header = req_headers
                               importing ev_status_code = data(status_code)
                                         ev_status_text = data(status_text)
                                         ev_content_type = data(content_type)
                                         et_response_header = data(response_header)
                                         ev_response_body = data(response_body)
                                         et_add_request_header = data(add_request_header)
                                         ev_sap_client = data(sap_client)
                                         ev_local_client = data(local_client)
                                         ev_uri_prefix = data(uri_prefix)
                                         ev_sm59_http_dest = data(sm59_http_dest)
                                         ev_request_id = data(request_id)
                                         ev_error_type = data(error_type)
                                         ev_error_text = data(error_text)
                                         ev_error_timestamp = data(error_timestamp)
                                         ev_duration = data(duration) ).

    try.

      data(conv_in) = cl_abap_conv_in_ce=>create( input = response_body
                                                  encoding = '4110' ).

      data(body_c) = ``.

      conv_in->read( importing data = body_c ).

      data(conv_out) = cl_abap_conv_out_ce=>create( encoding = '4110' ).

      conv_out->convert( exporting data = body_c
                         importing buffer = response_body ).

    catch cx_parameter_invalid_type
          cx_parameter_invalid_range
          cx_sy_conversion_codepage
          cx_sy_codepage_converter_init.

      response_body = `426F647920636F6E7461696E7320756E7072696E7461626C6520636861726163746572732E`.

    endtry.

    select count(*)
      from ZI_OV4_Orders
      where ExternalId ne '141'
      into @data(db_count).

    cl_abap_unit_assert=>assert_equals( act = conv i( body_c )
                                        exp = db_count ).

  endmethod.
  method cnt_sets_count.

    data(client_proxy) = /iwfnd/cl_sutil_client_proxy=>get_instance( ).

    data(req_headers) = value /iwfnd/sutil_property_t( ( name = if_http_header_fields_sap=>request_method
                                                         value = `GET` )
                                                       ( name = if_http_header_fields_sap=>request_uri
                                                         value = `/sap/opu/odata4/sap/zodatav4/default/sap/zov4_orders/0001/ZI_OV4_Orders/$count` ) ).

    client_proxy->web_request( exporting it_request_header = req_headers
                               importing ev_status_code = data(status_code)
                                         ev_status_text = data(status_text)
                                         ev_content_type = data(content_type)
                                         et_response_header = data(response_header)
                                         ev_response_body = data(response_body)
                                         et_add_request_header = data(add_request_header)
                                         ev_sap_client = data(sap_client)
                                         ev_local_client = data(local_client)
                                         ev_uri_prefix = data(uri_prefix)
                                         ev_sm59_http_dest = data(sm59_http_dest)
                                         ev_request_id = data(request_id)
                                         ev_error_type = data(error_type)
                                         ev_error_text = data(error_text)
                                         ev_error_timestamp = data(error_timestamp)
                                         ev_duration = data(duration) ).

    try.

      data(conv_in) = cl_abap_conv_in_ce=>create( input = response_body
                                                  encoding = '4110' ).

      data(body_c) = ``.

      conv_in->read( importing data = body_c ).

      data(conv_out) = cl_abap_conv_out_ce=>create( encoding = '4110' ).

      conv_out->convert( exporting data = body_c
                         importing buffer = response_body ).

    catch cx_parameter_invalid_type
          cx_parameter_invalid_range
          cx_sy_conversion_codepage
          cx_sy_codepage_converter_init.

      response_body = `426F647920636F6E7461696E7320756E7072696E7461626C6520636861726163746572732E`.

    endtry.

    select count(*)
      from ZI_OV4_Orders
      into @data(db_count).

    cl_abap_unit_assert=>assert_equals( act = conv i( body_c )
                                        exp = db_count ).

  endmethod.
  method nty_n_cnt_rets_col_n_sets_cnt.

    data(client_proxy) = /iwfnd/cl_sutil_client_proxy=>get_instance( ).

    data(req_headers) = value /iwfnd/sutil_property_t( ( name = if_http_header_fields_sap=>request_method
                                                         value = `GET` )
                                                       ( name = if_http_header_fields_sap=>request_uri
                                                         value = `/sap/opu/odata4/sap/zodatav4/default/sap/zov4_orders/0001/ZI_OV4_Orders?$count=true` ) ).

    client_proxy->web_request( exporting it_request_header = req_headers
                               importing ev_status_code = data(status_code)
                                         ev_status_text = data(status_text)
                                         ev_content_type = data(content_type)
                                         et_response_header = data(response_header)
                                         ev_response_body = data(response_body)
                                         et_add_request_header = data(add_request_header)
                                         ev_sap_client = data(sap_client)
                                         ev_local_client = data(local_client)
                                         ev_uri_prefix = data(uri_prefix)
                                         ev_sm59_http_dest = data(sm59_http_dest)
                                         ev_request_id = data(request_id)
                                         ev_error_type = data(error_type)
                                         ev_error_text = data(error_text)
                                         ev_error_timestamp = data(error_timestamp)
                                         ev_duration = data(duration) ).

    try.

      data(conv_in) = cl_abap_conv_in_ce=>create( input = response_body
                                                  encoding = '4110' ).

      data(body_c) = ``.

      conv_in->read( importing data = body_c ).

      data(conv_out) = cl_abap_conv_out_ce=>create( encoding = '4110' ).

      conv_out->convert( exporting data = body_c
                         importing buffer = response_body ).

    catch cx_parameter_invalid_type
          cx_parameter_invalid_range
          cx_sy_conversion_codepage
          cx_sy_codepage_converter_init.

      response_body = `426F647920636F6E7461696E7320756E7072696E7461626C6520636861726163746572732E`.

    endtry.

*    cl_abap_unit_assert=>assert_equals( act = conv i( body_c )
*                                        exp = db_count ).

  endmethod.
  method nty_n_slct_rets_short_col.

    data(client_proxy) = /iwfnd/cl_sutil_client_proxy=>get_instance( ).

    data(req_headers) = value /iwfnd/sutil_property_t( ( name = if_http_header_fields_sap=>request_method
                                                         value = `GET` )
                                                       ( name = if_http_header_fields_sap=>request_uri
                                                         value = `/sap/opu/odata4/sap/zodatav4/default/sap/zov4_orders/0001/ZI_OV4_Orders?$select=ExternalId,CreatedAt` ) ).

    client_proxy->web_request( exporting it_request_header = req_headers
                               importing ev_status_code = data(status_code)
                                         ev_status_text = data(status_text)
                                         ev_content_type = data(content_type)
                                         et_response_header = data(response_header)
                                         ev_response_body = data(response_body)
                                         et_add_request_header = data(add_request_header)
                                         ev_sap_client = data(sap_client)
                                         ev_local_client = data(local_client)
                                         ev_uri_prefix = data(uri_prefix)
                                         ev_sm59_http_dest = data(sm59_http_dest)
                                         ev_request_id = data(request_id)
                                         ev_error_type = data(error_type)
                                         ev_error_text = data(error_text)
                                         ev_error_timestamp = data(error_timestamp)
                                         ev_duration = data(duration) ).

    try.

      data(conv_in) = cl_abap_conv_in_ce=>create( input = response_body
                                                  encoding = '4110' ).

      data(body_c) = ``.

      conv_in->read( importing data = body_c ).

      data(conv_out) = cl_abap_conv_out_ce=>create( encoding = '4110' ).

      conv_out->convert( exporting data = body_c
                         importing buffer = response_body ).

    catch cx_parameter_invalid_type
          cx_parameter_invalid_range
          cx_sy_conversion_codepage
          cx_sy_codepage_converter_init.

      response_body = `426F647920636F6E7461696E7320756E7072696E7461626C6520636861726163746572732E`.

    endtry.

*    cl_abap_unit_assert=>assert_equals( act = conv i( body_c )
*                                        exp = db_count ).

  endmethod.

endclass.
