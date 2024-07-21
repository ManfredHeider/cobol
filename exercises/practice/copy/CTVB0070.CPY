      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0070.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0070.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   30 Jan 1996 10:59:46   P13SP99
      *PVCS-Keywords
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: CTVB0070                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    DCLGEN VON TBVB0070 (KIRH)                                 * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: J. SCHINDLER                                     * 00060000
      *    DATUM:    01.12.1995                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0244!01.12.95! 01 ! NEU                       !SCHINDLER *  00140000
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * DCLGEN TABLE(TBVB0070)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0070 TABLE
CCC004*    ( KIRH_ALTER         SMALLINT       NOT NULL,
CCC004*      KNR_GESCHL         SMALLINT       NOT NULL,
CCC004*      KNR_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KIRH_LAST_UPDATE   TIMESTAMP      NOT NULL,
CCC004*      KTY_GESCHL         SMALLINT       NOT NULL,
CCC004*      KTY_WAEHRUNG       SMALLINT       NOT NULL,
CCC004*      KIRH_VERS_SUM      DECIMAL(11, 2) NOT NULL,
CCC004*      KIRH_ABL_SUM       DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0070                           *
      ******************************************************************
       01  TBVB0070-GESAMT.
        05  NI-TBVB0070-TAB.
           10 NI-TBVB0070-ELE      PIC S9(04) COMP  OCCURS 8 TIMES.
        05  NI-TBVB0070            REDEFINES  NI-TBVB0070-TAB.
           10 NI-KIRH-ALTER        PIC S9(4)    USAGE COMP.
           10 NI-KNR-GESCHL        PIC S9(4)    USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-LAST-UPDATE  PIC S9(4)    USAGE COMP.
           10 NI-KTY-GESCHL        PIC S9(4)    USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4)    USAGE COMP.
           10 NI-KIRH-VERS-SUM     PIC S9(4)    USAGE COMP.
           10 NI-KIRH-ABL-SUM      PIC S9(4)    USAGE COMP.
        05  DCL-TBVB0070.
           10 KIRH-ALTER           PIC S9(4)    USAGE COMP.
           10 KNR-GESCHL           PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-LAST-UPDATE     PIC X(26).
           10 KTY-HESCHL           PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-VERS-SUM        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-ABL-SUM         PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
