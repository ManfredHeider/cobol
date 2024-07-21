      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/CVVBA009.CPv  $
      *
      *   Rev 2.1   Jun 20 2002 14:23:14   t13ae13
      *AVB00606: Einführung Seminare für DVAG-Wien
      *
      *   Rev 2.0   Jun 11 2002 11:48:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00606!13.06.02!2.1 ! Neu                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVBA009)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA009))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA009 TABLE
CCC004*    ( MS_NR                          SMALLINT NOT NULL,
CCC004*      MS_EINSTIEG                    CHAR(1) NOT NULL,
CCC004*      MS_HPT                         CHAR(1) NOT NULL,
CCC004*      MS_34C                         CHAR(1) NOT NULL,
CCC004*      MS_ABZPROZ_REGEL               DECIMAL (5, 2) NOT NULL,
CCC004*      MS_EING_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_EIG_GES                  DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_EIG_PZTR                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_GRU_PZTR                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_GES_PZTR                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_WEIT_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_ZEITRAUM                    SMALLINT NOT NULL,
CCC004*      MS_EQF_EIG                     DECIMAL(5, 2) NOT NULL,
CCC004*      MS_EQF_GRU                     DECIMAL(5, 2) NOT NULL,
CCC004*      MS_EQF_GES                     DECIMAL(5, 2) NOT NULL,
CCC004*      MS_PROV_STF                    DECIMAL(5, 2) NOT NULL,
CCC004*      MS_GUELTIG_BIS                 DATE,
CCC004*      MS_BEZ_KURZ                    CHAR(4) NOT NULL,
CCC004*      MS_BEZ                         CHAR(40) NOT NULL,
CCC004*      MS_FG                          CHAR(1) NOT NULL,
CCC004*      MS_127                         CHAR(1) NOT NULL,
CCC004*      KTY_GESELL_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_GESELL_TYP                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA009                           *
      ******************************************************************
       01  VIVBA009-GESAMT.
        05  NI-VIVBA009-TAB.
           10  NI-VIVBA009-ELE     PIC S9(04)  COMP   OCCURS  23 TIMES.
        05  NI-VIVBA009            REDEFINES   NI-VIVBA009-TAB.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-MS-EINSTIEG       PIC S9(04)  COMP.
           10 NI-MS-HPT            PIC S9(04)  COMP.
           10 NI-MS-34C            PIC S9(04)  COMP.
           10 NI-MS-ABZPROZ-REGEL  PIC S9(04)  COMP.
           10 NI-MS-EING-EH-EIG    PIC S9(04)  COMP.
           10 NI-MS-EH-EIG-GES     PIC S9(04)  COMP.
           10 NI-MS-EH-EIG-PZTR    PIC S9(04)  COMP.
           10 NI-MS-EH-GRU-PZTR    PIC S9(04)  COMP.
           10 NI-MS-EH-GES-PZTR    PIC S9(04)  COMP.
           10 NI-MS-WEIT-EH-EIG    PIC S9(04)  COMP.
           10 NI-MS-ZEITRAUM       PIC S9(04)  COMP.
           10 NI-MS-EQF-EIG        PIC S9(04)  COMP.
           10 NI-MS-EQF-GRU        PIC S9(04)  COMP.
           10 NI-MS-EQF-GES        PIC S9(04)  COMP.
           10 NI-MS-PROV-STF       PIC S9(04)  COMP.
           10 NI-MS-GUELTIG-BIS    PIC S9(04)  COMP.
           10 NI-MS-BEZ-KURZ       PIC S9(04)  COMP.
           10 NI-MS-BEZ            PIC S9(04)  COMP.
           10 NI-MS-FG             PIC S9(04)  COMP.
           10 NI-MS-127            PIC S9(04)  COMP.
           10 NI-KTY-GESELL-TYP    PIC S9(04)  COMP.
           10 NI-KNR-GESELL-TYP    PIC S9(04)  COMP.
        05  DCL-VIVBA009.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 MS-EINSTIEG          PIC X(1).
           10 MS-HPT               PIC X(1).
           10 MS-34C               PIC X(1).
           10 MS-ABZPROZ-REGEL     PIC S999V99 USAGE COMP-3.
           10 MS-EING-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-EIG-GES        PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-EIG-PZTR       PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-GRU-PZTR       PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-GES-PZTR       PIC S999999999V9 USAGE COMP-3.
           10 MS-WEIT-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 MS-ZEITRAUM          PIC S9(4) USAGE COMP.
           10 MS-EQF-EIG           PIC S999V99 USAGE COMP-3.
           10 MS-EQF-GRU           PIC S999V99 USAGE COMP-3.
           10 MS-EQF-GES           PIC S999V99 USAGE COMP-3.
           10 MS-PROV-STF          PIC S999V99 USAGE COMP-3.
           10 MS-GUELTIG-BIS       PIC X(10).
           10 MS-BEZ-KURZ          PIC X(4).
           10 MS-BEZ               PIC X(40).
           10 MS-FG                PIC X(1).
           10 MS-127               PIC X(1).
           10 KTY-GESELL-TYP       PIC S9(4) USAGE COMP.
           10 KNR-GESELL-TYP       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
