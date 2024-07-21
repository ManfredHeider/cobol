      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WVVB0009.CPv  $
      *
      *   Rev 2.1   05 Dec 2001 09:09:44   t13ae13
      *AVB00544: Umstellung 40%-Klausel auf variabel
      *
      *   Rev 2.0   Feb 27 1999 15:59:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:18:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0009                                      * 00022004
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  001                                              * 00082003
      ***************************************************************** 00090000
      *                                                                 00100000
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00544!06.11.01! 2.1!WB-MS-40PROZ-REGEL entfällt! Walb     *
      *         !        !    !WB-MS-ABZPROZ-REGEL neu    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0009                           *
      ******************************************************************
        05  WB-NI-VIVB0009-TAB.
           10  WB-NI-VIVB0009-ELE  PIC S9(04)  COMP   OCCURS  19 TIMES.
        05  WB-NI-VIVB0009          REDEFINES   WB-NI-VIVB0009-TAB.
           10 WB-NI-MS-NR           PIC S9(04)  COMP.
           10 WB-NI-MS-EINSTIEG     PIC S9(04)  COMP.
           10 WB-NI-MS-HPT          PIC S9(04)  COMP.
           10 WB-NI-MS-34C          PIC S9(04)  COMP.
061101*    10 WB-NI-MS-40PROZ-REGEL PIC S9(04)  COMP.
061101     10 WB-NI-MS-ABZPROZ-REGEL PIC S9(04)  COMP.
           10 WB-NI-MS-EING-EH-EIG  PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EIG-GES   PIC S9(04)  COMP.
           10 WB-NI-MS-EH-EIG-PZTR  PIC S9(04)  COMP.
           10 WB-NI-MS-EH-GRU-PZTR  PIC S9(04)  COMP.
           10 WB-NI-MS-EH-GES-PZTR  PIC S9(04)  COMP.
           10 WB-NI-MS-WEIT-EH-EIG  PIC S9(04)  COMP.
           10 WB-NI-MS-ZEITRAUM     PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-EIG      PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-GRU      PIC S9(04)  COMP.
           10 WB-NI-MS-EQF-GES      PIC S9(04)  COMP.
           10 WB-NI-MS-PROV-STF     PIC S9(04)  COMP.
           10 WB-NI-MS-GUELTIG-BIS  PIC S9(04)  COMP.
           10 WB-NI-MS-BEZ-KURZ     PIC S9(04)  COMP.
           10 WB-NI-MS-BEZ          PIC S9(04)  COMP.
        05  WB-VIVB0009.
           10 WB-MS-NR             PIC S9(4) COMP           VALUE ZERO.
           10 WB-MS-EINSTIEG       PIC X(1)                 VALUE SPACE.
           10 WB-MS-HPT            PIC X(1)                 VALUE SPACE.
           10 WB-MS-34C            PIC X(1)                 VALUE SPACE.
061101*    10 WB-MS-40PROZ-REGEL   PIC X(1)                 VALUE SPACE.
061101     10 WB-MS-ABZPROZ-REGEL  PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-EING-EH-EIG    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-EIG-GES     PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-EIG-PZTR    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-GRU-PZTR    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-EH-GES-PZTR    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-WEIT-EH-EIG    PIC S9(09)V9 COMP-3      VALUE ZERO.
           10 WB-MS-ZEITRAUM       PIC S9(4) COMP           VALUE ZERO.
           10 WB-MS-EQF-EIG        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-EQF-GRU        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-EQF-GES        PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-PROV-STF       PIC S999V99 COMP-3       VALUE ZERO.
           10 WB-MS-GUELTIG-BIS    PIC X(10)                VALUE SPACE.
           10 WB-MS-BEZ-KURZ       PIC X(4)                 VALUE SPACE.
           10 WB-MS-BEZ            PIC X(40)                VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
