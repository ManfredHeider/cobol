      ***----------------------------------------------------------------
      *** (C) Copyright 2006-2018,2020 Micro Focus or one of its affiliates.
      ***
      *** The only warranties for products and services of Micro Focus and its
      *** affiliates and licensors ("Micro Focus") are set forth in the express
      *** warranty statements accompanying such products and services. Nothing
      *** herein should be construed as constituting an additional warranty.
      *** Micro Focus shall not be liable for technical or editorial errors or
      *** omissions contained herein. The information contained herein is
      *** subject to change without notice.
      ***----------------------------------------------------------------

      ***----------------------------------------------------------------
      ***
      *** Name: SAFAPI.CPY
      ***
      *** Project: ES SAF API
      ***
      *** Related source elements:
      ***
      *** SAFAPI.CPY   - SAF API definition copybook
      *** safapi.h     - SAF API definition header file
      *** SAFROUTE.CBL - SAF API routine
      *** SAFROUTE.RC  - SAF API routine Windows resource file
      *** SAFROUTE.LIB - SAF API routine DLL lib for C
      *** SAFROUTE.DLL - SAF API routine runtime
      *** tsafapi.c    - SAF API C usage examples
      *** TSAFAPI.CBL  - SAF API COBOL usage examples
      *** SAFAPI.WIN   - Makefile
      ***
      *** Description:
      ***
      *** This is the copybook that defines the data elements for
      *** ES SAF security request calls.
      *** The ()-safpb-parameter-block structure is used for all
      *** SAF API calls.
      ***
      *** This copybook contains the following major sections:
      ***   SAF API parameter list structure (safpb-parameter-block)
      ***   SAF API constants
      ***   SAF API return and reason codes
      ***----------------------------------------------------------------
      *** Request parameter list usage common to all requests
      ***----------------------------------------------------------------
      ***
      ***    Copy SAFAPI replacing ==()== by ==ws==.
      ***
      *** 1  ()-safpb-parameter-block.
      ***
      ***    ()-safpb-version               <-- in
      ***    ()-safpb-request               <-- in
      ***    ()-safpb-type                  <-- in
      ***    ()-safpb-modifier              <-- in
      ***    ()-safpb-flag                  <-- in
      ***    ()-safpb-codeset               <-- in
      ***    ()-safpb-api-rc                <-- OUT
      ***    ()-safpb-mgr-return            <-- OUT
      ***    ()-safpb-mgr-reason            <-- OUT
      ***    ()-safpb-safesm-index          <-- OUT
      ***----------------------------------------------------------------
      *** VERIFY request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-verify-USERID-len     <-- in
      ***    ()-safpb-verify-PWRD-len       <-- in
      ***    ()-safpb-verify-NEWPWRD-len    <-- in
      ***    ()-safpb-verify-TOKEN-len      <-- in/OUT
      ***    ()-safpb-verify-group          <-- in
      ***    ()-safpb-verify-ACEE-ptr       <-- in/OUT
      ***    ()-safpb-verify-USERID-ptr     <-- in
      ***    ()-safpb-verify-PWRD-ptr       <-- in
      ***    ()-safpb-verify-NEWPWRD-ptr    <-- in
      ***    ()-safpb-verify-TOKEN-ptr      <-- in/OUT
      ***----------------------------------------------------------------
      *** AUTH request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-auth-ENTITY-len       <-- in
      ***    ()-safpb-auth-FACILITY-len     <-- in
      ***    ()-safpb-auth-TRANSACTION-len  <-- in
      ***    ()-safpb-auth-class            <-- in
      ***    ()-safpb-auth-ACEE-ptr         <-- in
      ***    ()-safpb-auth-ENTITY-ptr       <-- in
      ***    ()-safpb-auth-FACILITY-ptr     <-- in
      ***    ()-safpb-auth-TRANSACTION-ptr  <-- in
      ***----------------------------------------------------------------
      *** XAUTH request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-xauth-ENTITY-len      <-- in
      ***    ()-safpb-xauth-STRING1-len     <-- in
      ***    ()-safpb-xauth-STRING2-len     <-- in
      ***    ()-safpb-xauth-CLASS-len       <-- in
      ***    ()-safpb-xauth-PERMISSIONS     <-- in/OUT
      ***    ()-safpb-xauth-ACEE-ptr        <-- in
      ***    ()-safpb-xauth-ENTITY-ptr      <-- in
      ***    ()-safpb-xauth-STRING1-ptr     <-- in
      ***    ()-safpb-xauth-STRING2-ptr     <-- in
      ***    ()-safpb-xauth-CLASS-ptr       <-- in
      ***----------------------------------------------------------------
      *** AUDIT request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-audit-ENTITY-len      <-- in
      ***    ()-safpb-audit-LOGSTR-len      <-- in
      ***    ()-safpb-audit-ACEE-ptr        <-- in
      ***    ()-safpb-audit-ENTITY-ptr      <-- in
      ***    ()-safpb-audit-LOGSTR-ptr      <-- in
      ***----------------------------------------------------------------
      *** STAT request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-stat-QUERY-NAME-len   <-- in
      ***    ()-safpb-stat-NEXT-len         <-- in
      ***    ()-safpb-stat-RETURN-AREA-len  <-- in/OUT
      ***    ()-safpb-stat-QUERY-NAME-ptr   <-- in
      ***    ()-safpb-stat-NEXT-ptr         <-- in
      ***    ()-safpb-stat-RETURN-AREA-ptr  <-- in/OUT
      ***----------------------------------------------------------------
      *** ADMIN request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-admin-ARGSTR-len      <-- in (optional or 0)
      ***    ()-safpb-admin-ARGTBL-count    <-- in
      ***    ()-safpb-admin-LIST-count      <-- OUT
      ***    ()-safpb-admin-ARGSTR-ptr      <-- in (optional or NULL)
      ***    ()-safpb-admin-ARGTBL-ptr      <-- in
      ***    ()-safpb-admin-LIST-ptr        <-- OUT
      ***----------------------------------------------------------------
      *** UPDATE request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-update-ENTITY-len     <-- in (optional or 0)
      ***    ()-safpb-update-ACTION         <-- in (optional or 0)
      ***    ()-safpb-update-ENTITY-ptr     <-- in (optional or NULL)
      ***----------------------------------------------------------------
      *** MAP request-specific parameter list usage
      ***----------------------------------------------------------------
      ***    ()-safpb-map-INPUT-len         <-- in
      ***    ()-safpb-map-OUTPUT-len        <-- OUT
      ***    ()-safpb-map-INPUT-ptr         <-- in
      ***    ()-safpb-map-OUTPUT            <-- OUT
      ***----------------------------------------------------------------
      *** Conditional assembly variable - saf78-safadmin-data-areas
      ***----------------------------------------------------------------
      *** Create an instance of this variable to cause SAFAPI
      *** copybook expansion to include the data area definitions for
      *** SAF ADMIN API calls.
      *** 78  saf78-safadmin-data-areas value 1. <=== example
      ***----------------------------------------------------------------
      *** Conditional assembly variable - saf78-safadmin-argmax
      ***----------------------------------------------------------------
      *** Specify the number of rows to be generated in the SAFADMIN
      *** argument table when the SAFAPI copybook is included.
      *** One row is needed for each keyword-value or stand-alone
      *** keyword specified in a SAFADMIN command and passed to the
      *** SAFROUTE API for processing.  The same is true for
      *** programmed callers that assemble keywords and keyword-value
      *** pairs and then call SAFROUTE ADMIN directly.
      *** Conditional assembly variable saf78-safadmin-data-areas must
      *** also be specified to cause inclusion of the argument table.
      *** The number of rows is defaulted to 200 if saf78-safadmin-argmax
      *** is not explicitly specified.
      *** 78  saf78-safadmin-argmax     value 100.  <=== example
      ***----------------------------------------------------------------
      *** Conditional assembly variable - 78-CASSAF-DEVELOPMENT
      ***----------------------------------------------------------------
      *** Create an instance of this variable to cause SAFAPI
      *** copybook expansion to include additional data areas in the
      *** Safpb-Parameter-Block structure.
      *** These are additional data areas found in the parallel copybook
      *** used in CAS.
      *** 78  78-CASSAF-DEVELOPMENT value 1. <=== example
      ***----------------------------------------------------------------
      *** Conditional assembly variable - 78-nm-len
      ***----------------------------------------------------------------
      *** Specify the size of name character strings used by CAS.
      *** Conditional assembly variable 78-CASSAF-DEVELOPMENT must also
      *** be specified to cause inclusion of the name character strings.
      *** The string length is defaulted to 8 if 78-nm-len is not
      *** explicitly specified.
      *** 78  78-nm-len value 8. <=== example
      ***----------------------------------------------------------------
      ***                        Change Log
      ***----------------------------------------------------------------
      ***
      *** Date      Initials  Reason
      *** --------  --------  -------------------------------------------
      *** 04/25/06  sc/rlw    Initial version
      *** 05/08/06  sc/rlw    Refinements and cascbsaf compatibility
      *** 05/10/06  sc/rlw    Convergence with cascbsaf; one API def
      *** 05/25/06  rlw       Include all IBM-defined class names 78/88
      *** 05/30/06  rlw       Updates for cascbsaf.cpy revision 1.20
      *** 06/08/06  rlw       Remove obsolete flags per Stu;add api modes
      *** 06/23/06  rlw       Correct UTF8 constant value in level78
      *** 06/29/06  rlw       New XAUTH call for extended authorizations
      *** 08/01/06  rlw       New ADMIN call for repository modifications
      ***----------------------------------------------------------------

      ***----------------------------------------------------------------
      *** S A F   A P I   p a r a m e t e r   l i s t
      ***----------------------------------------------------------------

       01  ()-safpb-parameter-block.

      ***
      *** SAF API version identifier
      ***----------------------------------------------------------------
           03  ()-safpb-version                        pic x(4) comp-5.
               88  ()-safpb-cur-ver-88                 value 2.
      ***
      *** request code
      ***----------------------------------------------------------------
           03  ()-safpb-request                        pic x(1) comp-5.
               88  ()-safpb-verify-req-88              value 1.
               88  ()-safpb-auth-req-88                value 2.
               88  ()-safpb-audit-req-88               value 3.
               88  ()-safpb-stat-req-88                value 4.
               88  ()-safpb-xauth-req-88               value 5.
               88  ()-safpb-admin-req-88               value 6.
               88  ()-safpb-update-req-88              value 7.
               88  ()-safpb-map-req-88                 value 8.
      ***
      *** request qualifier (usage varies by request code)
      ***----------------------------------------------------------------
           03  ()-safpb-type                           pic x(1) comp-5.

               88  ()-safpb-verify-create-88           value 1.
               88  ()-safpb-verify-destroy-88          value 2.
               88  ()-safpb-verify-token-88            value 3.
               88  ()-safpb-verify-tok-dest-88         value 4.
               88  ()-safpb-verify-user-check-88       value 5.

               88  ()-safpb-auth-none-88               value h'00'.
               88  ()-safpb-auth-stat-access-88        value h'01'.
               88  ()-safpb-auth-read-88               value h'02'.
               88  ()-safpb-auth-update-88             value h'04'.
               88  ()-safpb-auth-control-88            value h'08'.
               88  ()-safpb-auth-exec-88               value h'10'.
               88  ()-safpb-auth-alter-88              value h'80'.

               88  ()-safpb-audit-success-88           value 1.
               88  ()-safpb-audit-failure-88           value 2.

               88  ()-safpb-stat-esm-88                value 1.
               88  ()-safpb-stat-class-88              value 2.

               88  ()-safpb-admin-adduser-88           value 1.
               88  ()-safpb-admin-addgroup-88          value 2.
               88  ()-safpb-admin-addres-88            value 3.
               88  ()-safpb-admin-altuser-88           value 4.
               88  ()-safpb-admin-altgroup-88          value 5.
               88  ()-safpb-admin-altres-88            value 6.
               88  ()-safpb-admin-deluser-88           value 7.
               88  ()-safpb-admin-delgroup-88          value 8.
               88  ()-safpb-admin-delres-88            value 9.
               88  ()-safpb-admin-listuser-88          value 10.
               88  ()-safpb-admin-listgroup-88         value 11.
               88  ()-safpb-admin-listres-88           value 12.
               88  ()-safpb-admin-conuser-88           value 13.
               88  ()-safpb-admin-remuser-88           value 14.
               88  ()-safpb-admin-setpswd-88           value 15.
               88  ()-safpb-admin-permit-88            value 16.
               88  ()-safpb-admin-setopts-88           value 17.
               88  ()-safpb-admin-freelist-88          value 18.
               88  ()-safpb-admin-listclass-88         value 19.
               88  ()-safpb-admin-delclass-88          value 20.
               88  ()-safpb-admin-listrefs-88          value 21.
               88  ()-safpb-admin-addmember-88         value 22.
               88  ()-safpb-admin-delmember-88         value 23.
               88  ()-safpb-admin-addace-88            value 24.
               88  ()-safpb-admin-delace-88            value 25.
               88  ()-safpb-admin-altace-88            value 26.
               88  ()-safpb-admin-getpswd-88           value 27.

               88  ()-safpb-update-user-88             value 1.
               88  ()-safpb-update-group-88            value 2.
               88  ()-safpb-update-resource-88         value 3.
               88  ()-safpb-update-users-88            value 4.
               88  ()-safpb-update-groups-88           value 5.
               88  ()-safpb-update-resources-88        value 6.
               88  ()-safpb-update-all-88              value 0.

               88  ()-safpb-map-long-to-short-88       value 1.
               88  ()-safpb-map-short-to-long-88       value 2.

      ***
      *** subsystem identifier
      ***----------------------------------------------------------------
           03  ()-safpb-modifier                       pic x(1) comp-5.
               88  ()-safpb-mod-CICS-88                value 1.
               88  ()-safpb-mod-IMS-88                 value 2.
               88  ()-safpb-mod-JCL-88                 value 3.
               88  ()-safpb-mod-ESMAC-88               value 4.
               88  ()-safpb-mod-WEB-88                 value 5.
               88  ()-safpb-mod-BINP-88                value 6.
               88  ()-safpb-mod-CGI-88                 value 7.
               88  ()-safpb-mod-MFDS-88                value 8.
               88  ()-safpb-mod-MFCS-88                value 9.
               88  ()-safpb-mod-ESCWA-88               value 10.
               88  ()-safpb-mod-DFT-88                 value 11.
               
      ***
      *** flag byte
      ***----------------------------------------------------------------
           03  ()-safpb-flag                           pic x(1) comp-5.
               88  ()-safpb-flag-PW-encrypt-88         value 1 false 0.
               88  ()-safpb-from-cache-88              value 1.
               88  ()-safpb-ver-no-password-88         value 2.
               88  ()-safpb-map-no-ACEE-88             value 2.
               88  ()-safpb-ver-pt-ticket-88           value 4.
               88  ()-safpb-ver-passtoken-88           value 8.
               88  ()-safpb-ver-pt-surrogate-88        value 16.
               88  ()-safpb-ver-pass-changed-88        value 32.
               88  ()-safpb-flag-api-trace-88          value 64.
               88  ()-safpb-flag-api-display-88        value 128.
      ***
      *** character set identifier (future use)
      ***----------------------------------------------------------------
           03  ()-safpb-codeset                        pic x(2) comp-5.
               88  ()-safpb-codeset-utf8-88            value 0.
      ***
      *** available for future use
      ***----------------------------------------------------------------
           03  ()-safpb-flag2                          pic x(1) comp-5.
               88  ()-safpb-audit-88                   value 1.
           03                                          pic x(1) comp-5.
      ***
      *** return and reason codes (three code aggregate)
      ***----------------------------------------------------------------
           03  ()-safpb-return.
               05  ()-safpb-api-rc-aggregate.
                   88  ()-safpb-api-ag-ok-88           value x'000000'.
                   88  ()-safpb-api-ag-authorized-88   value x'000000'.
                   88  ()-safpb-api-ag-not-auth-88     value x'08082c'.
                   88  ()-safpb-api-ag-pw-changed-88   value x'000004'.
                   88  ()-safpb-api-ag-pw-error-88     value x'080800'.
                   88  ()-safpb-api-ag-pw-expired-88   value x'080c00'.
                   88  ()-safpb-api-ag-pw-upd-error-88 value x'081000'.
                   88  ()-safpb-api-ag-pw-chg-error-88 value x'081000'.
                   88  ()-safpb-api-ag-pw-upd-hist-88  value x'081004'.
                   88  ()-safpb-api-ag-pw-chg-invld-88 value x'081004'.
                   88  ()-safpb-api-ag-pw-upd-allow-88 value x'081008'.
                   88  ()-safpb-api-ag-pw-chg-illgc-88 value x'081008'.
                   88  ()-safpb-api-ag-id-error-88     value x'080400'.
                   88  ()-safpb-api-ag-id-not-in-gr-88 value x'081400'.
                   88  ()-safpb-api-ag-id-revoked-88   value x'081c00'.
                   88  ()-safpb-api-ag-id-illogic-88   value x'085c00'.
                   88  ()-safpb-api-ag-id-g-revoked-88 value x'082400'.
                   88  ()-safpb-api-ag-version-err-88  value x'0c0100'.
                   88  ()-safpb-api-ag-not-protect-88  value x'040400'.
                   88  ()-safpb-api-ag-no-class-def-88 value x'040408'.
                   88  ()-safpb-api-ag-esm-error-88    value x'041800'.
                   88  ()-safpb-api-ag-pt-refused-88   value x'086800'.
                   88  ()-safpb-api-ag-pt-format-88    value x'086804'.
                   88  ()-safpb-api-ag-pt-expire-88    value x'086808'.
                   88  ()-safpb-api-ag-pt-type-88      value x'08680c'.
                   88  ()-safpb-api-ag-pt-invalid-88   value x'086810'.
                   88  ()-safpb-api-ag-pt-error-88     value x'086814'.
                   88  ()-safpb-api-ag-parm-null-88    value x'081820'.
                   88  ()-safpb-api-ag-parm-unsup-88   value x'081828'.
                   88  ()-safpb-api-ag-cl-dont-care-88 value x'08182c'.
                   88  ()-safpb-api-ag-rs-dont-care-88 value x'081830'.
                   88  ()-safpb-api-ag-access-set-88   value x'001400'.
                   88  ()-safpb-api-ag-inv-pwd-val-88  value x'0C0902'.
                   *> These are bogus and only kept for compatibility
                   88  ()-safpb-api-ag-pamr-null-88    value x'0c0000'.
                   88  ()-safpb-api-ag-parm-invalid-88 value x'0c0000'.
      ***
      *** return and reason codes (SAF API return code)
      ***----------------------------------------------------------------
                   07  ()-safpb-api-rc                 pic x(1) comp-5.
                       88  ()-safpb-api-rc-ok-88       value h'00'.
                       88  ()-safpb-api-rc-incomp-88   value h'04'.
                       88  ()-safpb-api-rc-failed-88   value h'08'.
      ***
      *** return and reason codes (SAF Manager / ESM return code)
      ***----------------------------------------------------------------
                   07  ()-safpb-mgr-return             pic x(1) comp-5.
                       88  ()-safpb-mgr-rc-ok-88       value h'00'.
                       88  ()-safpb-mgr-not-protect-88 value h'04'.
                       88  ()-safpb-mgr-not-auth-88    value h'08'.
                       88  ()-safpb-mgr-access-88      value h'14'.
                       88  ()-safpb-mgr-error-88       value h'18'.
                       88  ()-safpb-mgr-token-88       value h'68'.
      ***
      *** return and reason codes (SAF Manager / ESM reason code)
      ***----------------------------------------------------------------
                   07  ()-safpb-mgr-reason             pic x(1) comp-5.
                       88  ()-safpb-access-not-auth-88 value h'00'.
                       88  ()-safpb-access-read-88     value h'04'.
                       88  ()-safpb-access-update-88   value h'08'.
                       88  ()-safpb-no-class-def-88    value h'08'.
                       88  ()-safpb-access-control-88  value h'0c'.
                       88  ()-safpb-access-alter-88    value h'10'.
                       88  ()-safpb-map-default-88     value h'04'.
                       88  ()-safpb-map-acee-88        value h'08'.
      ***
      *** ESM index (identifies ESM that passed/failed a request)
      ***----------------------------------------------------------------
           03  ()-safpb-safesm-index                   pic x(1) comp-5.

      *> Pad out all the request-specific structures to the same size.
      *> Why COBOL can't do this for us I do not know.
      $if P64 set
       78  78-saf-request-padding                      value 68.
      $else
       78  78-saf-request-padding                      value 84.
      $end

      ***
      *** VERIFY request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-verify.
               05  ()-safpb-verify-USERID-len          pic x(2) comp-5.
               05  ()-safpb-verify-PWRD-len            pic x(2) comp-5.
               05  ()-safpb-verify-NEWPWRD-len         pic x(2) comp-5.
               05  ()-safpb-verify-TOKEN-len           pic x(2) comp-5.
               05  ()-safpb-verify-group               pic x(8).
               05  ()-safpb-verify-ACEE-ptr            pointer.
               05  ()-safpb-verify-USERID-ptr          pointer.
               05  ()-safpb-verify-PWRD-ptr            pointer.
               05  ()-safpb-verify-NEWPWRD-ptr         pointer.
               05  ()-safpb-verify-TOKEN-ptr           pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** AUTH request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-auth redefines ()-safpb-verify.
               05  ()-safpb-auth-ENTITY-len            pic x(2) comp-5.
               05  ()-safpb-auth-FACILITY-len          pic x(2) comp-5.
               05  ()-safpb-auth-TRANSACTION-len       pic x(2) comp-5.
               05  ()-safpb-auth-name-04-len           pic x(2) comp-5.
               05  ()-safpb-auth-class                 pic x(8).
      ***
      ***           JES classes
      ***
                   88  ()-safpb-auth-jes-DATASET-88    value 'DATASET'.
                   88  ()-safpb-auth-jes-JESINPUT-88   value 'JESINPUT'.
                   88  ()-safpb-auth-jes-JESJOBS-88    value 'JESJOBS'.
                   88  ()-safpb-auth-jes-JESSPOOL-88   value 'JESSPOOL'.
                   88  ()-safpb-auth-jes-SURROGAT-88   value 'SURROGAT'.
                   88  ()-safpb-auth-jes-PHYSFILE-88   value 'PHYSFILE'.
                   88  ()-safpb-auth-jes-DCBINFO-88    value 'DCBINFO'.
      ***
      ***           MFDS classes
      ***
                   88  ()-safpb-auth-mfd-ESRCP-88      value 'ESRCP'.
                   88  ()-safpb-auth-mfd-ESRIIN-88     value 'ESRIIN'.
                   88  ()-safpb-auth-mfd-ESRIMPR-88    value 'ESRIMPR'.
                   88  ()-safpb-auth-mfd-ESRJIN-88     value 'ESRJIN'.
                   88  ()-safpb-auth-mfd-ESRJPR-88     value 'ESRJPR'.
                   88  ()-safpb-auth-mfd-ESRLIS-88     value 'ESRLIS'.
                   88  ()-safpb-auth-mfd-ESRPKG-88     value 'ESRPKG'.
                   88  ()-safpb-auth-mfd-ESRRQH-88     value 'ESRRQH'.
                   88  ()-safpb-auth-mfd-ESRSRV-88     value 'ESRSRV'.
                   88  ()-safpb-auth-mfd-ESRSVC-88     value 'ESRSVC'.
                   88  ()-safpb-auth-mfd-ESRXRM-88     value 'ESRXRM'.
                   88  ()-safpb-auth-mfd-MFDSADMN-88   value 'MFDSADMN'.
                   88  ()-safpb-auth-mfd-MFDSREP-88    value 'MFDSREP'.
      ***
      ***           ES Start/Stop class
      ***
                   88  ()-safpb-auth-OPERCMDS-88       value 'OPERCMDS'.
      ***
      ***           ESMAC classes
      ***
                   88  ()-safpb-auth-ESMAC-88          value 'ESMAC'.
                   88  ()-safpb-auth-MFESMAC-88        value 'MFESMAC'.
               05  ()-safpb-auth-ACEE-ptr              pointer.
               05  ()-safpb-auth-ENTITY-ptr            pointer.
               05  ()-safpb-auth-FACILITY-ptr          pointer.
               05  ()-safpb-auth-TRANSACTION-ptr       pointer.
               05  ()-safpb-auth-name-04-ptr           pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** XAUTH request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-xauth redefines ()-safpb-verify.
               05  ()-safpb-xauth-ENTITY-len           pic x(2) comp-5.
               05  ()-safpb-xauth-STRING1-len          pic x(2) comp-5.
               05  ()-safpb-xauth-STRING2-len          pic x(2) comp-5.
               05  ()-safpb-xauth-CLASS-len            pic x(2) comp-5.
               05  ()-safpb-xauth-PERMISSIONS          pic x(4) comp-5.
                   88  ()-safpb-xauth-execute-88       value h'01'.
                   88  ()-safpb-xauth-read-88          value h'02'.
                   88  ()-safpb-xauth-update-88        value h'04'.
                   88  ()-safpb-xauth-add-88           value h'08'.
                   88  ()-safpb-xauth-delete-88        value h'10'.
                   88  ()-safpb-xauth-control-88       value h'20'.
                   88  ()-safpb-xauth-alter-88         value h'40'.
               05                                      pic x(4) comp-5.
               05  ()-safpb-xauth-ACEE-ptr             pointer.
               05  ()-safpb-xauth-ENTITY-ptr           pointer.
               05  ()-safpb-xauth-STRING1-ptr          pointer.
               05  ()-safpb-xauth-STRING2-ptr          pointer.
               05  ()-safpb-xauth-CLASS-ptr            pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** AUDIT request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-audit redefines ()-safpb-verify.
               05  ()-safpb-audit-ENTITY-len           pic x(2) comp-5.
               05  ()-safpb-audit-LOGSTR-len           pic x(2) comp-5.
               05  ()-safpb-audit-name-03-len          pic x(2) comp-5.
               05  ()-safpb-audit-name-04-len          pic x(2) comp-5.
               05                                      pic x(8).
               05  ()-safpb-audit-ACEE-ptr             pointer.
               05  ()-safpb-audit-ENTITY-ptr           pointer.
               05  ()-safpb-audit-LOGSTR-ptr           pointer.
               05  ()-safpb-audit-name-03-ptr          pointer.
               05  ()-safpb-audit-name-04-ptr          pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** STAT request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-stat redefines ()-safpb-verify.
               05  ()-safpb-stat-QUERY-NAME-len        pic x(2) comp-5.
               05  ()-safpb-stat-NEXT-len              pic x(2) comp-5.
               05  ()-safpb-stat-RETURN-AREA-len       pic x(2) comp-5.
               05  ()-safpb-stat-name-04-len           pic x(2) comp-5.
               05                                      pic x(8).
               05                                      pointer.
               05  ()-safpb-stat-QUERY-NAME-ptr        pointer.
               05  ()-safpb-stat-NEXT-ptr              pointer.
               05  ()-safpb-stat-RETURN-AREA-ptr       pointer.
               05  ()-safpb-stat-name-04-ptr           pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** ADMIN request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-admin redefines ()-safpb-verify.
               05  ()-safpb-admin-ARGSTR-len           pic x(2) comp-5.
               05  ()-safpb-admin-ARGTBL-count         pic x(2) comp-5.
               05  ()-safpb-admin-LIST-count           pic x(2) comp-5.
               05  ()-safpb-admin-name-04-len          pic x(2) comp-5.
               05                                      pic x(8).
               05  ()-safpb-admin-ACEE-ptr             pointer.
               05  ()-safpb-admin-ARGSTR-ptr           pointer.
               05  ()-safpb-admin-ARGTBL-ptr           pointer.
               05  ()-safpb-admin-LIST-ptr             pointer.
               05  ()-safpb-admin-name-04-ptr          pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** UPDATE request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-update redefines ()-safpb-verify.
               05  ()-safpb-update-ENTITY-LEN          pic x(2) comp-5.
               05  ()-safpb-update-ACTION              pic x(2) comp-5.
                   88  ()-safpb-update-add-88          value 1.
                   88  ()-safpb-update-modify-88       value 2.
                   88  ()-safpb-update-delete-88       value 3.
                   88  ()-safpb-update-other-88        value 0.
               05  ()-safpb-update-name-03-len         pic x(2) comp-5.
               05  ()-safpb-update-name-04-len         pic x(2) comp-5.
               05                                      pic x(8).
               05  ()-safpb-update-ACEE-ptr            pointer.
               05  ()-safpb-update-ENTITY-ptr          pointer.
               05  ()-safpb-update-unused-ptr          pointer.
               05  ()-safpb-update-name-03-ptr         pointer.
               05  ()-safpb-update-name-04-ptr         pointer.
               05  filler                pic x(78-saf-request-padding).
      ***
      *** MAP request-specific data items
      ***----------------------------------------------------------------
       78 78-saf-long-name-max                         value 100.

           03  ()-safpb-map redefines ()-safpb-verify.
               05  ()-safpb-map-INPUT-len              pic x(2) comp-5.
               05  ()-safpb-map-OUTPUT-len             pic x(2) comp-5.
               05                                      pic x(2) comp-5.
               05                                      pic x(2) comp-5.
               05  ()-safpb-map-group                  pic x(8).
               05  ()-safpb-map-INPUT-ptr              pointer.
               05  ()-safpb-map-OUTPUT     pic x(78-saf-long-name-max).

       78  78-res-len                                  value 256.
      $if 78-CASSAF-DEVELOPMENT defined
      ***
      *** Extensions for current CAS development
      ***----------------------------------------------------------------
      ***
      *** generic version of request-specific data items
      ***----------------------------------------------------------------
           03  ()-safpb-name redefines ()-safpb-verify.
               05  ()-safpb-lengths.
                   07  ()-safpb-name-01-len            pic x(2) comp-5.
                   07  ()-safpb-name-02-len            pic x(2) comp-5.
                   07  ()-safpb-name-03-len            pic x(2) comp-5.
                   07  ()-safpb-name-04-len            pic x(2) comp-5.
               05  ()-safpb-name-char                  pic x(8).
               05  ()-safpb-pointers.
                   07  ()-safpb-name-ptr               pointer.
                   07  ()-safpb-name-01-ptr            pointer.
                   07  ()-safpb-name-02-ptr            pointer.
                   07  ()-safpb-name-03-ptr            pointer.
                   07  ()-safpb-name-04-ptr            pointer.
      ***
      *** The following is private for casesm SNT level security
      ***----------------------------------------------------------------
           03  ()-safpb-entity-pointers.
               05  ()-safpb-entity-1-ptr               pointer.
               05  ()-safpb-entity-2-ptr               pointer.
               05  ()-safpb-entity-3-ptr               pointer.
               05  ()-safpb-entity-4-ptr               pointer.
      $if 78-nm-len not defined
      $if 78-ES-SAF-pwd-len defined
       78  78-nm-len                            value 78-ES-SAF-pwd-len.
      $else (78-ES-SAF-pwd-len defined)
       78  78-nm-len                                   value 8.
      $end not (78-ES-SAF-pwd-len defined)
      $end (78-nm-len not defined)
       78  78-saf-entry-name                           value 'SAFROUTE'.
       78  78-saf-entity-irdr-CICS                     value 'STCINRDR'.
       78  78-saf-entity-irdr-ESMC                     value 'TSUINRDR'.
       78  78-saf-entity-irdr-IMSI                     value 'STCINRDR'.
       78  78-saf-entity-irdr-JESI                     value 'TSUINRDR'.
      ***----------------------------------------------------------------
       01  ()-safpb-name-01.
           03  ()-safpb-name-01-txt                    pic x(78-nm-len).
      ***----------------------------------------------------------------
       01  ()-safpb-name-02.
           03  ()-safpb-name-02-txt                    pic x(78-nm-len).
      ***----------------------------------------------------------------
       01  ()-safpb-name-03.
           03  ()-safpb-name-03-txt                    pic x(78-nm-len).
      ***----------------------------------------------------------------
      $if 78-token-len not defined
       78  78-token-len                                value 50.
      $end (78-token-len not defined)
       01  ()-safpb-name-04.
           03  ()-safpb-name-04-txt                    pic x(78-token-len).
       01  ()-safpb-admin-value                        pic x(78-res-len).
      $end (78-CASSAF-DEVELOPMENT defined)


      ***----------------------------------------------------------------
      *** E n d   o f   S A F   A P I   p a r a m e t e r   l i s t
      ***----------------------------------------------------------------

      ***----------------------------------------------------------------
      *** S A F   A P I   7 8 - l e v e l   c o n s t a n t s
      ***----------------------------------------------------------------
      ***
      *** SAP API version (safpb-version)
      ***----------------------------------------------------------------
       78  ()-saf78-VERSION-1             value 1.
       78  ()-saf78-VERSION-2             value 2.
       78  ()-saf78-VERSION-CURRENT       value 2.
      ***
      *** SAF request types (safpb-request)
      ***----------------------------------------------------------------
       78  ()-saf78-REQUEST-VERIFY        value 1.
       78  ()-saf78-REQUEST-AUTH          value 2.
       78  ()-saf78-REQUEST-AUDIT         value 3.
       78  ()-saf78-REQUEST-STAT          value 4.
       78  ()-saf78-REQUEST-XAUTH         value 5.
       78  ()-saf78-REQUEST-ADMIN         value 6.
       78  ()-saf78-REQUEST-UPDATE        value 7.
       78  ()-saf78-REQUEST-MAP           value 8.
      ***
      *** VERIFY request qualifiers (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-ENVIR-CREATE          value 1.
       78  ()-saf78-TYPE-ENVIR-DESTROY         value 2.
       78  ()-saf78-TYPE-TOKEN-CREATE          value 3.
       78  ()-saf78-TYPE-TOKEN-DESTROY         value 4.
       78  ()-saf78-TYPE-USER-CHECK            value 5.
         78  ()-saf78-TYPE-SHORT-TOKEN-CREATE     value 6.

      ***
      *** AUTH access attributes (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-ATTR-NONE        value 0.
       78  ()-saf78-TYPE-ATTR-STAT-ACC    value 1.
       78  ()-saf78-TYPE-ATTR-READ        value 2.
       78  ()-saf78-TYPE-ATTR-UPDATE      value 4.
       78  ()-saf78-TYPE-ATTR-CONTROL     value 8.
       78  ()-saf78-TYPE-ATTR-EXECUTE     value 16.
       78  ()-saf78-TYPE-ATTR-ALTER       value 128.
      ***
      *** AUDIT result qualifiers (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-RESULT-SUCCESS   value 1.
       78  ()-saf78-TYPE-RESULT-FAILURE   value 2.
      ***
      *** STAT query types (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-QUERY-ESM        value 1.
       78  ()-saf78-TYPE-QUERY-CLASS      value 2.
      ***
      *** ADMIN subcommand types (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-ADMIN-ADDUSER           value  1.
       78  ()-saf78-TYPE-ADMIN-ADDGROUP          value  2.
       78  ()-saf78-TYPE-ADMIN-ADDCLASS          value 13.
       78  ()-saf78-TYPE-ADMIN-ADDRES            value  3.
       78  ()-saf78-TYPE-ADMIN-ALTUSER           value  4.
       78  ()-saf78-TYPE-ADMIN-ALTGROUP          value  5.
       78  ()-saf78-TYPE-ADMIN-ALTCLASS          value 14.
       78  ()-saf78-TYPE-ADMIN-ALTRES            value  6.
       78  ()-saf78-TYPE-ADMIN-DELUSER           value  7.
       78  ()-saf78-TYPE-ADMIN-DELGROUP          value  8.
       78  ()-saf78-TYPE-ADMIN-DELCLASS          value 20.
       78  ()-saf78-TYPE-ADMIN-DELRES            value  9.
       78  ()-saf78-TYPE-ADMIN-LISTUSER          value 10.
       78  ()-saf78-TYPE-ADMIN-LISTGROUP         value 11.
       78  ()-saf78-TYPE-ADMIN-LISTRES           value 12.
       78  ()-saf78-TYPE-ADMIN-LISTCLASS         value 19.
       78  ()-saf78-TYPE-ADMIN-LISTREFS          value 21.
       78  ()-saf78-TYPE-ADMIN-SETPSWD           value 15.
       78  ()-saf78-TYPE-ADMIN-PERMIT            value 16.
       78  ()-saf78-TYPE-ADMIN-SETOPTS           value 17.
       78  ()-saf78-TYPE-ADMIN-FREELIST          value 18.
       78  ()-saf78-TYPE-ADMIN-ADDMEMBER         value 22.
       78  ()-saf78-TYPE-ADMIN-DELMEMBER         value 23.
       78  ()-saf78-TYPE-ADMIN-ADDACE            value 24.
       78  ()-saf78-TYPE-ADMIN-DELACE            value 25.
       78  ()-saf78-TYPE-ADMIN-ALTACE            value 26.
       78  ()-saf78-TYPE-ADMIN-GETPSWD           value 27.
       78  ()-saf78-NUM-ADMIN-TYPES              value 27.
      ***
      *** UPDATE object types (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-UPDATE-USER             value  1.
       78  ()-saf78-TYPE-UPDATE-GROUP            value  2.
       78  ()-saf78-TYPE-UPDATE-RESOURCE         value  3.
       78  ()-saf78-TYPE-UPDATE-USERS            value  4.
       78  ()-saf78-TYPE-UPDATE-GROUPS           value  5.
       78  ()-saf78-TYPE-UPDATE-RESOURCES        value  6.
       78  ()-saf78-TYPE-UPDATE-ALL              value  0.
      ***
      *** MAP direction types (safpb-type)
      ***----------------------------------------------------------------
       78  ()-saf78-TYPE-LONG-TO-SHORT           value  1.
       78  ()-saf78-TYPE-SHORT-TO-LONG           value  2.
      ***
      *** Subsystem identifiers (safpb-modifier)
      ***----------------------------------------------------------------
       78  ()-saf78-MODIFIER-NONE         value 0.
       78  ()-saf78-MODIFIER-CICS         value 1.
       78  ()-saf78-MODIFIER-IMS          value 2.
       78  ()-saf78-MODIFIER-JCL          value 3.
       78  ()-saf78-MODIFIER-ESMAC        value 4.
       78  ()-saf78-MODIFIER-WEB          value 5.
       78  ()-saf78-MODIFIER-BINP         value 6.
       78  ()-saf78-MODIFIER-CGI          value 7.
       78  ()-saf78-MODIFIER-MFDS         value 8.
       78  ()-saf78-MODIFIER-MFCS         value 9.
       78  ()-saf78-MODIFIER-ESCWA        value 10.
       78  ()-saf78-MODIFIER-DFT          value 11.
      ***
      *** Flags (safpb-flag)
      ***----------------------------------------------------------------
       78  ()-saf78-FLAG-PW-ENCRYPT       value 1.
       78  ()-saf78-FROM-CACHE            value 1.
       78  ()-saf78-VER-NO-PASSWORD       value 2.
       78  ()-saf78-MAP-NO-ACEE           value 2.
       78  ()-saf78-VER-PT-TICKET         value 4.
       78  ()-saf78-VER-PASSTOKEN         value 8.
       78  ()-saf78-VER-PT-SURROGATE      value 16.
       78  ()-saf78-FLAG-API-TRACE        value 64.
       78  ()-saf78-FLAG-API-DISPLAY      value 128.
      ***
      *** String codeset identifier (safpb-codeset)
      ***----------------------------------------------------------------
       78  ()-saf78-CODESET-UTF8          value 0.
      ***
      *** z/OS system resource classes (safpb-auth-class)
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-DATASET         value 'DATASET '.
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-ALCSAUTH        value 'ALCSAUTH'.
       78  ()-saf78-CLASS-APPCLU          value 'APPCLU  '.
       78  ()-saf78-CLASS-APPCPORT        value 'APPCPORT'.
       78  ()-saf78-CLASS-APPCSERV        value 'APPCSERV'.
       78  ()-saf78-CLASS-APPCSI          value 'APPCSI  '.
       78  ()-saf78-CLASS-APPCTP          value 'APPCTP  '.
       78  ()-saf78-CLASS-APPL            value 'APPL    '.
       78  ()-saf78-CLASS-CACHECLS        value 'CACHECLS'.
       78  ()-saf78-CLASS-CBIND           value 'CBIND   '.
       78  ()-saf78-CLASS-CDT             value 'CDT     '.
       78  ()-saf78-CLASS-CONSOLE         value 'CONSOLE '.
       78  ()-saf78-CLASS-CSFKEYS         value 'CSFKEYS '.
       78  ()-saf78-CLASS-CSFSERV         value 'CSFSERV '.
       78  ()-saf78-CLASS-DASDVOL         value 'DASDVOL '.
       78  ()-saf78-CLASS-DBNFORM         value 'DBNFORM '.
       78  ()-saf78-CLASS-DEVICES         value 'DEVICES '.
       78  ()-saf78-CLASS-DIGTCERT        value 'DIGTCERT'.
       78  ()-saf78-CLASS-DIGTCRIT        value 'DIGTCRIT'.
       78  ()-saf78-CLASS-DIGTNMAP        value 'DIGTNMAP'.
       78  ()-saf78-CLASS-DIGTRING        value 'DIGTRING'.
       78  ()-saf78-CLASS-DIRAUTH         value 'DIRAUTH '.
       78  ()-saf78-CLASS-DLFCLASS        value 'DLFCLASS'.
       78  ()-saf78-CLASS-FACILITY        value 'FACILITY'.
       78  ()-saf78-CLASS-FIELD           value 'FIELD   '.
       78  ()-saf78-CLASS-GCSFKEYS        value 'GCSFKEYS'.
       78  ()-saf78-CLASS-GDASDVOL        value 'GDASDVOL'.
       78  ()-saf78-CLASS-GLOBAL          value 'GLOBAL  '.
       78  ()-saf78-CLASS-GMBR            value 'GMBR    '.
       78  ()-saf78-CLASS-GSDSF           value 'GSDSF   '.
       78  ()-saf78-CLASS-GTERMINL        value 'GTERMINL'.
       78  ()-saf78-CLASS-GXFACILI        value 'GXFACILI'.
       78  ()-saf78-CLASS-IBMOPC          value 'IBMOPC  '.
       78  ()-saf78-CLASS-JESINPUT        value 'JESINPUT'.
       78  ()-saf78-CLASS-JESJOBS         value 'JESJOBS '.
       78  ()-saf78-CLASS-JESSPOOL        value 'JESSPOOL'.
       78  ()-saf78-CLASS-KEYSMSTR        value 'KEYSMSTR'.
       78  ()-saf78-CLASS-LDAPBIND        value 'LDAPBIND'.
       78  ()-saf78-CLASS-LOGSTRM         value 'LOGSTRM '.
       78  ()-saf78-CLASS-NODES           value 'NODES   '.
       78  ()-saf78-CLASS-NODMBR          value 'NODMBR  '.
       78  ()-saf78-CLASS-OPERCMDS        value 'OPERCMDS'.
       78  ()-saf78-CLASS-PHYSFILE        value 'PHYSFILE'.
       78  ()-saf78-CLASS-PMBR            value 'PMBR    '.
       78  ()-saf78-CLASS-PROGRAM         value 'PROGRAM '.
       78  ()-saf78-CLASS-PROPCNTL        value 'PROPCNTL'.
       78  ()-saf78-CLASS-PSFMPL          value 'PSFMPL  '.
       78  ()-saf78-CLASS-PTKTDATA        value 'PTKTDATA'.
       78  ()-saf78-CLASS-RACGLIST        value 'RACGLIST'.
       78  ()-saf78-CLASS-RACFEVNT        value 'RACFEVNT'.
       78  ()-saf78-CLASS-RRSFDATA        value 'RRSFDATA'.
       78  ()-saf78-CLASS-RVARSMBR        value 'RVARSMBR'.
       78  ()-saf78-CLASS-SCDMBR          value 'SCDMBR  '.
       78  ()-saf78-CLASS-SDSF            value 'SDSF    '.
       78  ()-saf78-CLASS-SECDATA         value 'SECDATA '.
       78  ()-saf78-CLASS-SECLABEL        value 'SECLABEL'.
       78  ()-saf78-CLASS-SECLMBR         value 'SECLMBR '.
       78  ()-saf78-CLASS-SERVAUTH        value 'SERVAUTH'.
       78  ()-saf78-CLASS-SERVER          value 'SERVER  '.
       78  ()-saf78-CLASS-SMESSAGE        value 'SMESSAGE'.
       78  ()-saf78-CLASS-SOMDOBJS        value 'SOMDOBJS'.
       78  ()-saf78-CLASS-STARTED         value 'STARTED '.
       78  ()-saf78-CLASS-SURROGAT        value 'SURROGAT'.
       78  ()-saf78-CLASS-SYSMVIEW        value 'SYSMVIEW'.
       78  ()-saf78-CLASS-TAPEVOL         value 'TAPEVOL '.
       78  ()-saf78-CLASS-TEMPDSN         value 'TEMPDSN '.
       78  ()-saf78-CLASS-TERMINAL        value 'TERMINAL'.
       78  ()-saf78-CLASS-VTAMAPPL        value 'VTAMAPPL'.
       78  ()-saf78-CLASS-WRITER          value 'WRITER  '.
       78  ()-saf78-CLASS-XFACILIT        value 'XFACILIT'.
      ***
      *** CICS resource classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-ACICSPCT        value 'ACICSPCT'.
       78  ()-saf78-CLASS-BCICSPCT        value 'BCICSPCT'.
       78  ()-saf78-CLASS-CCICSCMD        value 'CCICSCMD'.
       78  ()-saf78-CLASS-CPSMOBJ         value 'CPSMOBJ '.
       78  ()-saf78-CLASS-CPSMXMP         value 'CPSMXMP '.
       78  ()-saf78-CLASS-DCICSDCT        value 'DCICSDCT'.
       78  ()-saf78-CLASS-ECICSDCT        value 'ECICSDCT'.
       78  ()-saf78-CLASS-FCICSFCT        value 'FCICSFCT'.
       78  ()-saf78-CLASS-GCICSTRN        value 'GCICSTRN'.
       78  ()-saf78-CLASS-GCPSMOBJ        value 'GCPSMOBJ'.
       78  ()-saf78-CLASS-HCICSFCT        value 'HCICSFCT'.
       78  ()-saf78-CLASS-JCICSJCT        value 'JCICSJCT'.
       78  ()-saf78-CLASS-KCICSJCT        value 'KCICSJCT'.
       78  ()-saf78-CLASS-MCICSPPT        value 'MCICSPPT'.
       78  ()-saf78-CLASS-NCICSPPT        value 'NCICSPPT'.
       78  ()-saf78-CLASS-PCICSPSB        value 'PCICSPSB'.
       78  ()-saf78-CLASS-QCICSPSB        value 'QCICSPSB'.
       78  ()-saf78-CLASS-SCICSTST        value 'SCICSTST'.
       78  ()-saf78-CLASS-TCICSTRN        value 'TCICSTRN'.
       78  ()-saf78-CLASS-UCICSTST        value 'UCICSTST'.
       78  ()-saf78-CLASS-VCICSCMD        value 'VCICSCMD'.
      ***
      *** DB2 resource classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-DSNADM          value 'DSNADM  '.
       78  ()-saf78-CLASS-DSNR            value 'DSNR    '.
       78  ()-saf78-CLASS-GDSNBP          value 'GDSNBP  '.
       78  ()-saf78-CLASS-GDSNCL          value 'GDSNCL  '.
       78  ()-saf78-CLASS-GDSNDB          value 'GDSNDB  '.
       78  ()-saf78-CLASS-GDSNJR          value 'GDSNJR  '.
       78  ()-saf78-CLASS-GDSNPK          value 'GDSNPK  '.
       78  ()-saf78-CLASS-GDSNPN          value 'GDSNPN  '.
       78  ()-saf78-CLASS-GDSNSC          value 'GDSNSC  '.
       78  ()-saf78-CLASS-GDSNSG          value 'GDSNSG  '.
       78  ()-saf78-CLASS-GDSNSM          value 'GDSNSM  '.
       78  ()-saf78-CLASS-GDSNSP          value 'GDSNSP  '.
       78  ()-saf78-CLASS-GDSNSQ          value 'GDSNSQ  '.
       78  ()-saf78-CLASS-GDSNTB          value 'GDSNTB  '.
       78  ()-saf78-CLASS-GDSNTS          value 'GDSNTS  '.
       78  ()-saf78-CLASS-GDSNUT          value 'GDSNUT  '.
       78  ()-saf78-CLASS-MDSNBP          value 'MDSNBP  '.
       78  ()-saf78-CLASS-MDSNCL          value 'MDSNCL  '.
       78  ()-saf78-CLASS-MDSNDB          value 'MDSNDB  '.
       78  ()-saf78-CLASS-MDSNJR          value 'MDSNJR  '.
       78  ()-saf78-CLASS-MDSNPK          value 'MDSNPK  '.
       78  ()-saf78-CLASS-MDSNPN          value 'MDSNPN  '.
       78  ()-saf78-CLASS-MDSNSC          value 'MDSNSC  '.
       78  ()-saf78-CLASS-MDSNSG          value 'MDSNSG  '.
       78  ()-saf78-CLASS-MDSNSM          value 'MDSNSM  '.
       78  ()-saf78-CLASS-MDSNSP          value 'MDSNSP  '.
       78  ()-saf78-CLASS-MDSNSQ          value 'MDSNSQ  '.
       78  ()-saf78-CLASS-MDSNTB          value 'MDSNTB  '.
       78  ()-saf78-CLASS-MDSNTS          value 'MDSNTS  '.
       78  ()-saf78-CLASS-MDSNUF          value 'MDSNUF  '.
       78  ()-saf78-CLASS-MDSNUT          value 'MDSNUT  '.
      ***
      *** DCE resource classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-DCEUUIDS        value 'DCEUUIDS'.
       78  ()-saf78-CLASS-RAUDITX         value 'RAUDITX '.
       78  ()-saf78-CLASS-EJBROLE         value 'EJBROLE '.
       78  ()-saf78-CLASS-GEJBROLE        value 'GEJBROLE'.
       78  ()-saf78-CLASS-JAVA            value 'JAVA    '.
      ***
      *** IMS resource classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-AIMS            value 'AIMS    '.
       78  ()-saf78-CLASS-CIMS            value 'CIMS    '.
       78  ()-saf78-CLASS-DIMS            value 'DIMS    '.
       78  ()-saf78-CLASS-FIMS            value 'FIMS    '.
       78  ()-saf78-CLASS-GIMS            value 'GIMS    '.
       78  ()-saf78-CLASS-HIMS            value 'HIMS    '.
       78  ()-saf78-CLASS-IIMS            value 'IIMS    '.
       78  ()-saf78-CLASS-JIMS            value 'JIMS    '.
       78  ()-saf78-CLASS-LIMS            value 'LIMS    '.
       78  ()-saf78-CLASS-MIMS            value 'MIMS    '.
       78  ()-saf78-CLASS-OIMS            value 'OIMS    '.
       78  ()-saf78-CLASS-PIMS            value 'PIMS    '.
       78  ()-saf78-CLASS-QIMS            value 'QIMS    '.
       78  ()-saf78-CLASS-SIMS            value 'SIMS    '.
       78  ()-saf78-CLASS-TIMS            value 'TIMS    '.
       78  ()-saf78-CLASS-UIMS            value 'UIMS    '.
       78  ()-saf78-CLASS-WIMS            value 'WIMS    '.
      ***
      *** Infoprint Server classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-PRINTSRV        value 'PRINTSRV'.
      ***
      *** Info/Mgmt (Tivoli Service Desk) classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-GINFOMAN        value 'GINFOMAN'.
       78  ()-saf78-CLASS-INFOMAN         value 'INFOMAN '.
      ***
      *** LFS/ESA classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-LFSCLASS        value 'LFSCLASS'.
      ***
      *** License Manager classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-ILMADMIN        value 'ILMADMIN'.
      ***
      *** Lotus Notes for z/OS classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-NDSLINK         value 'NDSLINK '.
       78  ()-saf78-CLASS-NOTELINK        value 'NOTELINK'.
      ***
      *** MQSeries classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-GMQADMIN        value 'GMQADMIN'.
       78  ()-saf78-CLASS-GMQCHAN         value 'GMQCHAN '.
       78  ()-saf78-CLASS-GMQNLIST        value 'GMQNLIST'.
       78  ()-saf78-CLASS-GMQPROC         value 'GMQPROC '.
       78  ()-saf78-CLASS-GMQQUEUE        value 'GMQQUEUE'.
       78  ()-saf78-CLASS-MQADMIN         value 'MQADMIN '.
       78  ()-saf78-CLASS-MQCHAN          value 'MQCHAN  '.
       78  ()-saf78-CLASS-MQCMDS          value 'MQCMDS  '.
       78  ()-saf78-CLASS-MQCONN          value 'MQCONN  '.
       78  ()-saf78-CLASS-MQNLIST         value 'MQNLIST '.
       78  ()-saf78-CLASS-MQPROC          value 'MQPROC  '.
       78  ()-saf78-CLASS-MQQUEUE         value 'MQQUEUE '.
      ***
      *** NetView classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-NETCMDS         value 'NETCMDS '.
       78  ()-saf78-CLASS-NETSPAN         value 'NETSPAN'.
       78  ()-saf78-CLASS-NVASAPDT        value 'NVASAPDT'.
       78  ()-saf78-CLASS-PTKTVAL         value 'PTKTVAL '.
       78  ()-saf78-CLASS-RMTOPS          value 'RMTOPS  '.
       78  ()-saf78-CLASS-RODMMGR         value 'RODMMGR '.
      ***
      *** Network Authentication Service classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-KERBLINK        value 'KERBLINK'.
       78  ()-saf78-CLASS-REALM           value 'REALM   '.
      ***
      *** SMS (DFSMSdfp) classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-MGMTCLAS        value 'MGMTCLAS'.
       78  ()-saf78-CLASS-STORCLAS        value 'STORCLAS'.
       78  ()-saf78-CLASS-SUBSYSNM        value 'SUBSYSNM'.
      ***
      *** Tivoli classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-ROLE            value 'ROLE    '.
       78  ()-saf78-CLASS-TMEADMIN        value 'TMEADMIN'.
      ***
      *** TSO classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-ACCTNUM         value 'ACCTNUM '.
       78  ()-saf78-CLASS-PERFGRP         value 'PERFGRP '.
       78  ()-saf78-CLASS-TSOAUTH         value 'TSOAUTH '.
       78  ()-saf78-CLASS-TSOPROC         value 'TSOPROC '.
      ***
      *** z/OS UNIX classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-DIRACC          value 'DIRACC  '.
       78  ()-saf78-CLASS-DIRSRCH         value 'DIRSRCH '.
       78  ()-saf78-CLASS-FSOBJ           value 'FSOBJ   '.
       78  ()-saf78-CLASS-FSSEC           value 'FSSEC   '.
       78  ()-saf78-CLASS-IPCOBJ          value 'IPCOBJ  '.
       78  ()-saf78-CLASS-PROCACT         value 'PROCACT '.
       78  ()-saf78-CLASS-PROCESS         value 'PROCESS '.
       78  ()-saf78-CLASS-UNIXMAP         value 'UNIXMAP '.
       78  ()-saf78-CLASS-UNIXPRIV        value 'UNIXPRIV'.
      ***
      *** ES resource classes
      ***----------------------------------------------------------------
       78  ()-saf78-CLASS-ESRDCT          value 'ESRDCT  '.
       78  ()-saf78-CLASS-ESRFCT          value 'ESRFCT  '.
       78  ()-saf78-CLASS-ESRGRP          value 'ESRGRP  '.
       78  ()-saf78-CLASS-ESRICE          value 'ESRICE  '.
       78  ()-saf78-CLASS-ESRIMS          value 'ESRIMS  '.
       78  ()-saf78-CLASS-ESRJCT          value 'ESRJCT  '.
       78  ()-saf78-CLASS-ESRJES          value 'ESRJES  '.
       78  ()-saf78-CLASS-ESRMPR          value 'ESRMPR  '.
       78  ()-saf78-CLASS-ESRPCT          value 'ESRPCT  '.
       78  ()-saf78-CLASS-ESRPLT          value 'ESRPLT  '.
       78  ()-saf78-CLASS-ESRPPT          value 'ESRPPT  '.
       78  ()-saf78-CLASS-ESRSIT          value 'ESRSIT  '.
       78  ()-saf78-CLASS-ESRSNT          value 'ESRSNT  '.
       78  ()-saf78-CLASS-ESRSUP          value 'ESRSUP  '.
       78  ()-saf78-CLASS-ESRSYS6         value 'ESRSYS6 '.
       78  ()-saf78-CLASS-ESRSYSC         value 'ESRSYSC '.
       78  ()-saf78-CLASS-ESRTABLE        value 'ESRTABLE'.
       78  ()-saf78-CLASS-ESRTCT          value 'ESRTCT  '.
       78  ()-saf78-CLASS-ESRTERM         value 'ESRTERM '.
       78  ()-saf78-CLASS-ESRTST          value 'ESRTST  '.
       78  ()-saf78-CLASS-ESRTTYPE        value 'ESRTTYPE'.
       78  ()-saf78-CLASS-ESRXAT          value 'ESRXAT  '.
       78  ()-saf78-CLASS-ESRXLT          value 'ESRXLT  '.
       78  ()-saf78-CLASS-ESRCP           value 'ESRCP   '.
       78  ()-saf78-CLASS-ESRIIN          value 'ESRIIN  '.
       78  ()-saf78-CLASS-ESRIMPR         value 'ESRIMPR '.
       78  ()-saf78-CLASS-ESRJIN          value 'ESRJIN  '.
       78  ()-saf78-CLASS-ESRJPR          value 'ESRJPR  '.
       78  ()-saf78-CLASS-ESRLIS          value 'ESRLIS  '.
       78  ()-saf78-CLASS-ESRPKG          value 'ESRPKG  '.
       78  ()-saf78-CLASS-ESRRQH          value 'ESRRQH  '.
       78  ()-saf78-CLASS-ESRSRV          value 'ESRSRV  '.
       78  ()-saf78-CLASS-ESRSVC          value 'ESRSVC  '.
       78  ()-saf78-CLASS-ESRXRM          value 'ESRXRM  '.
       78  ()-saf78-CLASS-MFDSADMN        value 'MFDSADMN'.
       78  ()-saf78-CLASS-MFDSREP         value 'MFDSREP '.
       78  ()-saf78-CLASS-DCBINFO         value 'DCBINFO '.


      ***----------------------------------------------------------------
      *** S A F   A P I   r e t u r n   &   r e a s o n   c o d e s
      ***----------------------------------------------------------------
      ***
      *** VERIFY return and reason codes
      ***----------------------------------------------------------------

       78  ()-saf78-SAF-RC-SUCCESS              value 0.
           78  ()-saf78-RC-NORMAL                   value 0.
               78  ()-saf78-RS-NORMAL                   value 0.
           78  ()-saf78-RC-TOKEN-INFO               value 4.
               78  ()-saf78-RS-TOKEN-TOO-LARGE          value 12.
               78  ()-saf78-RS-STOKEN-TOO-LARGE         value 16.

       78  ()-saf78-SAF-RC-NOT-COMPLETE         value 4.
           78  ()-saf78-RC-NO-DECISION              value 0.
               78  ()-saf78-RS-ESM-NOT-CALLED           value 0.
           78  ()-saf78-RC-NO-USER-PROFILE          value 4.
           78  ()-saf78-RC-SECURITY-NOT-ACTIVE      value 32.
           78  ()-saf78-RC-NO-FACILITY-CLASS        value 88.

       78  ()-saf78-SAF-RC-FAILURE              value 8.
      /    78  ()-saf78-RC-NO-USER-PROFILE          value 4.
           78  ()-saf78-RC-PWRD-INVALID             value 8.
           78  ()-saf78-RC-PWRD-EXPIRED             value 12.
           78  ()-saf78-RC-PWRD-CHANGE-ERR          value 16.
               78  ()-saf78-RS-PWD-HIST                 value 4.
               78  ()-saf78-RS-PWD-NO-CHANGE            value 8.
           78  ()-saf78-RC-USER-NOT-IN-GROUP        value 20.
           78  ()-saf78-RC-INST-EXIT                value 24.
           78  ()-saf78-RC-USER-REVOKED             value 28.
           78  ()-saf78-RC-GROUP-REVOKED            value 36.
           78  ()-saf78-RC-OID-REQUIRED             value 40.
           78  ()-saf78-RC-OID-INVALID              value 44.
           78  ()-saf78-RC-PORT-OF-ENTRY-ERR        value 48.
               78  ()-saf78-RS-PORT                     value 0.
               78  ()-saf78-RS-USER-TIME                value 4.
               78  ()-saf78-RS-PORT-TIME                value 8.
           78  ()-saf78-RC-NOT-AUTH-FOR-APPL        value 52.
           78  ()-saf78-RC-SECLABEL-FAIL            value 56.
               78  ()-saf78-RS-MLACTIVE-REQUIRES        value 4.
               78  ()-saf78-RS-NOT-AUTH-SECLABEL        value 8.
               78  ()-saf78-RS-MULTILEVEL-DOM           value 12.
               78  ()-saf78-RS-DISJOINT                 value 16.
               78  ()-saf78-RS-NOT-EQUIVALENT           value 20.
           78  ()-saf78-RC-DEFAULT-TOKEN-USED       value 68.
           78  ()-saf78-RC-UNPRIV-TRANQUIL          value 72.
           78  ()-saf78-RC-NODES-CHECK-FAIL         value 76.
               78  ()-saf78-RS-NODE-NOT-ALLOWED         value 0.
               78  ()-saf78-RS-USERID-UACC-NONE         value 4.
               78  ()-saf78-RS-GROUP-UACC-NONE          value 8.
               78  ()-saf78-RS-SECLABEL-UACC-NONE       value 12.
               78  ()-saf78-RS-NO-LOCAL-SUBMIT          value 16.
               78  ()-saf78-RS-REVERIFY-FAIL            value 20.
           78  ()-saf78-RC-SURROGATE-SUB-FAIL       value 80.
               78  ()-saf78-RS-SURROGATE-INACTIVE       value 4.
               78  ()-saf78-RS-SUB-NOT-PERMITTED        value 8.
               78  ()-saf78-RS-JOB-SECLABEL             value 12.
           78  ()-saf78-RC-JESJOBS-CHECK-FAIL       value 84.
           78  ()-saf78-RC-DATABASE-ERROR           value 92.
           78  ()-saf78-RC-CHECK-SUBPARAM           value 100.
           78  ()-saf78-RC-TOKEN-REFUSED            value 104.
               78  ()-saf78-RS-TOKEN-FORMAT             value 4.
               78  ()-saf78-RS-TOKEN-EXPIRED            value 8.
               78  ()-saf78-RS-TOKEN-TYPE               value 12.
               78  ()-saf78-RS-TOKEN-INVALID            value 16.
               78  ()-saf78-RS-TOKEN-ERROR              value 20.
      ***
      *** AUTH return and reason codes
      ***----------------------------------------------------------------

      /78  ()-saf78-SAF-RC-SUCCESS              value 0.
           78  ()-saf78-RC-USER-IS-AUTH             value 0.
      /        78  ()-saf78-RS-NORMAL                   value 0.
               78  ()-saf78-RS-RESOURCE-WARN            value 4.
               78  ()-saf78-RS-TAPE-VTOC                value 16.
               78  ()-saf78-RS-TAPE-CAN-VTOC            value 32.
               78  ()-saf78-RS-TAPE-NO-VTOC             value 36.
           78  ()-saf78-RC-ACCESS-INFO              value 20.
               78  ()-saf78-RS-ACCESS-NONE              value 0.
               78  ()-saf78-RS-ACCESS-READ              value 4.
               78  ()-saf78-RS-ACCESS-UPDATE            value 8.
               78  ()-saf78-RS-ACCESS-CONTROL           value 12.
               78  ()-saf78-RS-ACCESS-ALTER             value 16.

      /78  ()-saf78-SAF-RC-NOT-COMPLETE          value 4.
      /    78  ()-saf78-RC-NO-DECISION               value 0.
      /        78  ()-saf78-RS-ESM-NOT-CALLED           value 0.
           78  ()-saf78-RC-RESOURCE-NOT-PROT         value 4.
               78  ()-saf78-RS-NO-RESOURCE-PROF         value 0.
               78  ()-saf78-RS-STATUS-ERASE             value 4.
               78  ()-saf78-RS-CLASS-NOT-DEFINED        value 8.

      /78  ()-saf78-SAF-RC-FAILURE              value 8.
           78  ()-saf78-RC-USER-NOT-AUTH            value 8.
               78  ()-saf78-RS-PROTALL-NO-PROF          value 0.
               78  ()-saf78-RS-USER-ERASE               value 4.
               78  ()-saf78-RS-USER-TAPEVOL             value 8.
               78  ()-saf78-RS-USER-TAPE-DS             value 12.
               78  ()-saf78-RS-USER-TAPE-BLP            value 16.
               78  ()-saf78-RS-NON-CATALOGED            value 20.
               78  ()-saf78-RS-USER-AUTH-TRANQ          value 24.
               78  ()-saf78-RS-EXEC-ATTEMPT-READ        value 28.
               78  ()-saf78-RS-SECLABEL-NOT-DOM         value 32.
               78  ()-saf78-RS-SECLABEL-NEV-DOM         value 36.
               78  ()-saf78-RS-USER-NO-SECLABEL         value 40.
               78  ()-saf78-RS-COND-ACCESS              value 44.
           78  ()-saf78-RC-OLDVOL-ERROR             value 12.
           78  ()-saf78-RC-THIRD-PARTY-FAIL         value 16.
      /    78  ()-saf78-RC-CHECK-SUBPARAM           value 100.
      ***
      *** AUDIT return and reason codes
      ***----------------------------------------------------------------

      /78  ()-saf78-SAF-RC-SUCCESS              value 0.
      /    78  ()-saf78-RC-NORMAL                   value 0.
      /        78  ()-saf78-RS-NORMAL                   value 0.

      /78  ()-saf78-SAF-RC-NOT-COMPLETE         value 4.
      /    78  ()-saf78-RC-NO-DECISION              value 0.
      /        78  ()-saf78-RS-ESM-NOT-CALLED           value 0.
      /    78  ()-saf78-RC-MACT-CINACT              value 4.

      /78  ()-saf78-SAF-RC-FAILURE              value 8.
           78  ()-saf78-RC-NO-CLASS                 value 8.
           78  ()-saf78-RC-INTERNAL-ERROR           value 12.
           78  ()-saf78-RC-PARAM-ERROR              value 16.
               78  ()-saf78-RS-EVENT-INVALID            value 0.
               78  ()-saf78-RS-QUAL-INVALID             value 4.
               78  ()-saf78-RS-BAD-LIST-VER             value 8.
               78  ()-saf78-RS-BAD-LIST-LEN             value 12.
               78  ()-saf78-RS-ENTITY-INVALID           value 16.
           78  ()-saf78-RC-NO-AUDIT-DONE            value 20.
      ***
      *** STAT return and reason codes
      ***----------------------------------------------------------------

      /78  ()-saf78-SAF-RC-SUCCESS              value 0.
      /    78  ()-saf78-RC-NORMAL                   value 0.
      /        78  ()-saf78-RS-NORMAL                   value 0.

      /78  ()-saf78-SAF-RC-NOT-COMPLETE         value 4.
      /    78  ()-saf78-RC-NO-DECISION              value 0.
      /        78  ()-saf78-RS-ESM-NOT-CALLED           value 0.
           78  ()-saf78-RC-MACT-CINACT              value 4.
           78  ()-saf78-RC-MACT-NO-CLASS            value 8.
           78  ()-saf78-RC-MINACT-CACT              value 12.
           78  ()-saf78-RC-MINACT-CINACT            value 16.
           78  ()-saf78-RC-MINACT-NO-CLASS          value 20.
           78  ()-saf78-RC-NO-ESM                   value 24.
           78  ()-saf78-RC-BAD-PARMLIST             value 28.
      /    78  ()-saf78-RC-CHECK-SUBPARAM           value 100.
      ***
      *** ADMIN return and reason codes
      ***----------------------------------------------------------------
      /78  ()-saf78-SAF-RC-NOT-COMPLETE         value 4.
      /    78  ()-saf78-RC-NO-DECISION              value 0.
      /        78  ()-saf78-RS-ESM-NOT-CALLED           value 0.
      /78  ()-saf78-SAF-RC-SUCCESS              value 0.
      /    78 ()-saf78-RC-NORMAL                    value 0.
      /        78  ()-saf78-RS-NORMAL                   value 0.

      /78  ()-saf78-SAF-RC-NOT-COMPLETE         value 4.
      /    78  ()-saf78-RC-NO-DECISION              value 0.
      /        78  ()-saf78-RS-ESM-NOT-CALLED           value 0.
           78  ()-saf78-RC-KEYWORD-ERROR            value 4.
               78  ()-saf78-RS-KEYWORD-UNKNOWN          value 4.

      /78  ()-saf78-SAF-RC-FAILURE              value 8.
           78  ()-saf78-RC-VALUE-ERROR              value 4.
               78  ()-saf78-RS-VALUE-INVALID            value 4.
               78  ()-saf78-RS-VALUE-LENGTH             value 8.
      ***
      *** MAP return and reason codes
      ***----------------------------------------------------------------
      /78  ()-saf78-SAF-RC-SUCCESS              value 0.
      /    78  ()-saf78-RC-NORMAL                   value 0.
               78  ()-saf78-RS-MAP-DEFAULT              value 4.
               78  ()-saf78-RS-MAP-ACEE                 value 8.
      ***
      *** Return and reason codes common to all requests
      ***----------------------------------------------------------------

       78  ()-saf78-SAF-RC-PARM-ERROR           value 12.
           78  ()-saf78-RC-VERSION                  value 1.
           78  ()-saf78-RC-REQUEST                  value 2.
           78  ()-saf78-RC-TYPE                     value 3.
           78  ()-saf78-RC-MODIFIER                 value 4.
           78  ()-saf78-RC-FLAG                     value 5.
           78  ()-saf78-RC-CODESET                  value 6.
           78  ()-saf78-RC-SAFESM-INDEX             value 7.
           78  ()-saf78-RC-USERID                   value 8.
           78  ()-saf78-RC-PWRD                     value 9.
           78  ()-saf78-RC-NEWPWRD                  value 10.
           78  ()-saf78-RC-ENTITY                   value 11.
           78  ()-saf78-RC-FACILITY                 value 12.
           78  ()-saf78-RC-TRANSACTION              value 13.
           78  ()-saf78-RC-LOGSTR                   value 14.
           78  ()-saf78-RC-QUERY-NAME               value 15.
           78  ()-saf78-RC-NEXT                     value 16.
           78  ()-saf78-RC-RETURN-AREA              value 17.
           78  ()-saf78-RC-ACEE-PTR                 value 18.
           78  ()-saf78-RC-GROUP                    value 19.
           78  ()-saf78-RC-CLASS                    value 20.
           78  ()-saf78-RC-STRING1                  value 21.
           78  ()-saf78-RC-STRING2                  value 22.
           78  ()-saf78-RC-PERMISSIONS              value 23.
           78  ()-saf78-RC-ARGSTR                   value 24.
           78  ()-saf78-RC-ARGTBL                   value 25.
           78  ()-saf78-RC-ARGTBL-KEYPTR            value 26.
           78  ()-saf78-RC-ARGTBL-KEYLEN            value 27.
           78  ()-saf78-RC-ARGTBL-VALPTR            value 28.
           78  ()-saf78-RC-ARGTBL-VALLEN            value 29.
           78  ()-saf78-RC-TICKET                   value 30.
           78  ()-saf78-RC-TOKEN                    value 31.
           78  ()-saf78-RC-LIST                     value 32.
           78  ()-saf78-RC-INPUT                    value 33.
           78  ()-saf78-RC-INPUT-LEN                value 34.
           78  ()-saf78-RS-BAD-POINTER              value 1.
           78  ()-saf78-RS-BAD-VALUE                value 2.

      $if saf78-safadmin-data-areas defined
      ***----------------------------------------------------------------
      *** S A F   A D M I N   a r g u m e n t   t a b l e
      ***----------------------------------------------------------------

      $if   saf78-safadmin-argmax not defined
       78   saf78-safadmin-argmax                       value 200.
      $end  saf78-safadmin-argmax

       01  ()-safadmin-argtbl.
           03 ()-safadmin-argtbl-entry
                 occurs saf78-safadmin-argmax times.
              05 ()-safadmin-argkeyptr  pointer         value NULL.
              05 ()-safadmin-argvalptr  pointer         value NULL.
              05 ()-safadmin-argkeylen  pic x(4) comp-5 value 0.
              05 ()-safadmin-argvallen  pic x(4) comp-5 value 0.
              05 ()-safadmin-argesmword pic x(4) comp-5 value 0.
      $if P64 set
              05 filler                 pic x(4) comp-5.
      $end

      ***----------------------------------------------------------------
      *** S A F   A D M I N
      ***----------------------------------------------------------------

       78  78-max-cmdstr-len                     value 4096.
       01  ()-safpb-admin-str                  pic x(78-max-cmdstr-len).

       78   78-safadmin-argmax                   value 200.
       01  ()-safpb-admin-vars.
           03  ()-safpb-admin-nb-parms           pic x(2) comp-5.
           03  ()-safpb-admin-parm-len           pic x(2) comp-5.
           03  ()-safpb-admin-class-name         pic x(8).
           03  ()-safpb-admin-resource-name      pic x(78-res-len).
           03  ()-safpb-admin-item-count         pic x(2) comp-5.
           03  ()-safpb-admin-item-ptr           pointer.
           03  ()-safpb-admin-value-count        pic x(2) comp-5.
           03  ()-safpb-admin-value-ptr          pointer.
           03  ()-safpb-admin-key-ptr            pointer.
           03  ()-safpb-admin-value-len          pic x(4) comp-5.
           03  ()-safpb-admin-value-req-len      pic x(4) comp-5.
           03  ()-safpb-admin-keyword-len        pic x(4) comp-5.
           03  ()-safpb-admin-keyword            pic x(78-res-len).
           03  ()-safpb-admin-found-flag         pic x comp-x.
               88  ()-safpb-admin-found-88       value 1.
               88  ()-safpb-admin-not-found-88   value 2.

      ***----------------------------------------------------------------
      *** E n d   o f   S A F   A D M I N   a r g u m e n t   t a b l e
      ***----------------------------------------------------------------

      ***----------------------------------------------------------------
      *** S A F   A D M I N   s u b c o m m a n d   t a b l e
      ***----------------------------------------------------------------

       01 ()-safadmin-cmdtbl.
          03 filler pic x(16)       value "ADDUSER".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ADDUSER.
          03 filler pic x(1) comp-5 value 7.

          03 filler pic x(16)       value "ADDGROUP".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ADDGROUP.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "ADDCLASS".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ADDCLASS.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "ADDRESOURCE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ADDRES.
          03 filler pic x(1) comp-5 value 11.

          03 filler pic x(16)       value "ALTUSER".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ALTUSER.
          03 filler pic x(1) comp-5 value 7.

          03 filler pic x(16)       value "ALTGROUP".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ALTGROUP.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "ALTCLASS".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ALTCLASS.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "ALTRESOURCE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ALTRES.
          03 filler pic x(1) comp-5 value 11.

          03 filler pic x(16)       value "DELUSER".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-DELUSER.
          03 filler pic x(1) comp-5 value 7.

          03 filler pic x(16)       value "DELGROUP".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-DELGROUP.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "DELCLASS".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-DELCLASS.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "DELRESOURCE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-DELRES.
          03 filler pic x(1) comp-5 value 11.

          03 filler pic x(16)       value "FREELIST".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-FREELIST.
          03 filler pic x(1) comp-5 value 18.

          03 filler pic x(16)       value "LISTUSER".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-LISTUSER.
          03 filler pic x(1) comp-5 value 8.

          03 filler pic x(16)       value "LISTGROUP".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-LISTGROUP.
          03 filler pic x(1) comp-5 value 9.

          03 filler pic x(16)       value "LISTCLASS".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-LISTCLASS.
          03 filler pic x(1) comp-5 value 9.

          03 filler pic x(16)       value "LISTREFERENCES".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-LISTREFS.
          03 filler pic x(1) comp-5 value 14.

          03 filler pic x(16)       value "LISTRESOURCE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-LISTRES.
          03 filler pic x(1) comp-5 value 12.

          03 filler pic x(16)       value "PERMIT".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-PERMIT.
          03 filler pic x(1) comp-5 value 6.

          03 filler pic x(16)       value "SETPASSWORD".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-SETPSWD.
          03 filler pic x(1) comp-5 value 11.

          03 filler pic x(16)       value "SETOPTIONS".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-SETOPTS.
          03 filler pic x(1) comp-5 value 10.

          03 filler pic x(16)       value "ADDMEMBER".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ADDMEMBER.
          03 filler pic x(1) comp-5 value 9.

          03 filler pic x(16)       value "DELMEMBER".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-DELMEMBER.
          03 filler pic x(1) comp-5 value 9.

          03 filler pic x(16)       value "ADDACE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ADDACE.
          03 filler pic x(1) comp-5 value 6.

          03 filler pic x(16)       value "DELACE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-DELACE.
          03 filler pic x(1) comp-5 value 6.

          03 filler pic x(16)       value "ALTACE".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-ALTACE.
          03 filler pic x(1) comp-5 value 6.

          03 filler pic x(16)       value "GETPASSWORD".
          03 filler pic x(1) comp-5 value ()-saf78-TYPE-ADMIN-GETPSWD.
          03 filler pic x(1) comp-5 value 11.

          03 filler pic x(16)       value SPACES.
          03 filler pic x(1) comp-5 value 0.
          03 filler pic x(1) comp-5 value 0.

       78 ()-saf78-cmdtbl-size value ()-saf78-NUM-ADMIN-TYPES + 1.
       01 filler redefines ()-safadmin-cmdtbl.
          03 ()-safadmin-cmdtbl-entry occurs ()-saf78-cmdtbl-size times.
             05 ()-safadmin-cmdname   pic x(16).
             05 ()-safadmin-cmdtype   pic x(1) comp-5.
             05 ()-safadmin-cmdlen    pic x(1) comp-5.

      ***----------------------------------------------------------------
      *** E n d   S A F   A D M I N   s u b c o m m a n d   t a b l e
      ***----------------------------------------------------------------

      ***----------------------------------------------------------------
      *** S A F   A D M I N   k e y  n a m e s
      ***----------------------------------------------------------------
       78 ()-saf78-admin-ace           value "ACE".
       78 ()-saf78-admin-acl           value "ACL".
       78 ()-saf78-admin-allow-on      value "ALLOW".
       78 ()-saf78-admin-class         value "CLASS".
       78 ()-saf78-admin-computed      value "COMPUTED".
       78 ()-saf78-admin-consistent    value "CNSSTNT".
       78 ()-saf78-admin-cust-txt      value "CUSTOM".
       78 ()-saf78-admin-def-group     value "DEFGROUP".
       78 ()-saf78-admin-descript      value "DESC".
       78 ()-saf78-admin-dsp-name      value "NAME".
       78 ()-saf78-admin-expiration    value "EXPIRES".
       78 ()-saf78-admin-groupname     value "GROUP".
       78 ()-saf78-admin-groups        value "GROUPS".
       78 ()-saf78-admin-grp-prefix    value "GRPPFX".
       78 ()-saf78-admin-literal       value "LITERAL".
       78 ()-saf78-admin-login-time    value "LASTTIME".
       78 ()-saf78-admin-login-tries   value "ATTEMPTS".
       78 ()-saf78-admin-member        value "MEMBER".
       78 ()-saf78-admin-members       value "MEMBERS".
       78 ()-saf78-admin-oper-class    value "OPCLASS".
       78 ()-saf78-admin-oper-id       value "OPID".
       78 ()-saf78-admin-pass-chg      value "PASSCHG".
       78 ()-saf78-admin-pass-chg-time value "CHNGTIME".
       78 ()-saf78-admin-pass-exp      value "PASSEXP".
       78 ()-saf78-admin-password      value "PASSWORD".
       78 ()-saf78-admin-priority      value "PRIORITY".
       78 ()-saf78-admin-ref-name      value "NAME".
       78 ()-saf78-admin-ref-type      value "TYPE".
       78 ()-saf78-admin-ref-comment   value "COMMENT".
       78 ()-saf78-admin-resname       value "RESOURCE".
       78 ()-saf78-admin-timeout       value "TIMEOUT".
       78 ()-saf78-admin-tok-create    value "GETTOKEN".
       78 ()-saf78-admin-tok-use       value "USETOKEN".
       78 ()-saf78-admin-uid           value "UID".
       78 ()-saf78-admin-username      value "USER".
       78 ()-saf78-admin-value         value "VALUE".
      $end saf78-safadmin-data-areas


      ***--------------------------------------------------------------*
      ***  $Id: SAFAPI.CPY 1036559 2020-08-20 02:19:05Z mww $ end
      ***--------------------------------------------------------------*
