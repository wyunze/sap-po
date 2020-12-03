"! <p class="shorttext synchronized" lang="en">XML Assertion</p>
INTERFACE zif_xml_assertion
  PUBLIC .
  METHODS:
    "! <p class="shorttext synchronized" lang="en">assertion by contained text</p>
    "!
    "! @parameter iv_xml_xpayload | <p class="shorttext synchronized" lang="en">payload in xstring xml</p>
    "! @parameter iv_contain_text | <p class="shorttext synchronized" lang="en">contained text</p>
    "! @parameter rv_succeed | <p class="shorttext synchronized" lang="en">succeed indicator</p>
    assert_by_contain_text
      IMPORTING
                iv_xml_xpayload   TYPE xstring
                iv_contain_text   TYPE string
      RETURNING VALUE(rv_succeed) TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">assertion Value by tag name</p>
    "!
    "! @parameter iv_xml_xpayload | <p class="shorttext synchronized" lang="en">payload in xstring xml</p>
    "! @parameter iv_tag_name | <p class="shorttext synchronized" lang="en">tag name</p>
    "! @parameter iv_elem_value | <p class="shorttext synchronized" lang="en">element value</p>
    "! @parameter iv_match_first_only | <p class="shorttext synchronized" lang="en">only match first node</p>
    "! @parameter rv_succeed | <p class="shorttext synchronized" lang="en">succeed indicator</p>
    assert_by_tag_value
      IMPORTING
                iv_xml_xpayload     TYPE xstring
                iv_tag_name         TYPE string
                iv_elem_value       TYPE string
                iv_match_first_only TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_succeed)   TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">assertion count by tag</p>
    "!
    "! @parameter iv_xml_xpayload | <p class="shorttext synchronized" lang="en">payload</p>
    "! @parameter iv_tag_name | <p class="shorttext synchronized" lang="en">tag name</p>
    "! @parameter iv_count | <p class="shorttext synchronized" lang="en">count Value</p>
    "! @parameter rv_succeed | <p class="shorttext synchronized" lang="en">succeed indicator</p>
    assert_by_tag_count
      IMPORTING
                iv_xml_xpayload   TYPE xstring
                iv_tag_name       TYPE string
                iv_count          TYPE string
      RETURNING VALUE(rv_succeed) TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">assertion Value by xpath</p>
    "!
    "! @parameter iv_xml_xpayload | <p class="shorttext synchronized" lang="en">payload in xstring xml</p>
    "! @parameter iv_xml_xpath | <p class="shorttext synchronized" lang="en">xpath of xml</p>
    "! @parameter iv_elem_value | <p class="shorttext synchronized" lang="en">element value</p>
    "! @parameter iv_xml_namespace | <p class="shorttext synchronized" lang="en">xml namespace</p>
    "! @parameter rv_succeed | <p class="shorttext synchronized" lang="en">succeed indicator</p>
    assert_by_xpath_value
      IMPORTING
                iv_xml_xpayload   TYPE xstring
                iv_xml_xpath      TYPE string
                iv_elem_value     TYPE string
                iv_xml_namespace  TYPE string OPTIONAL
      RETURNING VALUE(rv_succeed) TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">assertion count by xpath</p>
    "!
    "! @parameter iv_xml_xpayload | <p class="shorttext synchronized" lang="en">payload</p>
    "! @parameter iv_xml_xpath | <p class="shorttext synchronized" lang="en">xpath</p>
    "! @parameter iv_count | <p class="shorttext synchronized" lang="en">count value</p>
    "! @parameter iv_xml_namespace | <p class="shorttext synchronized" lang="en">xml namespace</p>
    "! @parameter rv_succeed | <p class="shorttext synchronized" lang="en">succeed indicator</p>
    assert_by_xpath_count
      IMPORTING
                iv_xml_xpayload   TYPE xstring
                iv_xml_xpath      TYPE string
                iv_count          TYPE string
                iv_xml_namespace  TYPE string OPTIONAL
      RETURNING VALUE(rv_succeed) TYPE abap_bool.

ENDINTERFACE.
