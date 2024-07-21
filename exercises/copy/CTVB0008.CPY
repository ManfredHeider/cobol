      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:52:58   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0008)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0008))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0008 TABLE
CCC004*    ( MS_NR                          SMALLINT NOT NULL,
CCC004*      BS_NR                          SMALLINT NOT NULL,
CCC004*      MSBS_EH_EIG_GES                DECIMAL(10, 1) NOT NULL,
CCC004*      MSBS_EH_EIG_PZTR               DECIMAL(10, 1) NOT NULL,
CCC004*      MSBS_ZEITRAUM                  SMALLINT NOT NULL,
CCC004*      MSBS_EQF_EIG                   DECIMAL(5, 2) NOT NULL,
CCC004*      MSBS_PROV_STF                  DECIMAL(5, 2) NOT NULL,
CCC004*      MSBS_LAUFZEIT                  SMALLINT NOT NULL,
CCC004*      MSBS_GUELTIG_BIS               DATE,
CCC004*      MSBS_BEZ                       CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0008                           *
      ******************************************************************
       01  TBVB0008-GESAMT.
        05  NI-TBVB0008-TAB.
           10  NI-TBVB0008-ELE     PIC S9(04)  COMP   OCCURS  10 TIMES.
        05  NI-TBVB0008            REDEFINES   NI-TBVB0008-TAB.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-BS-NR             PIC S9(04)  COMP.
           10 NI-MSBS-EH-EIG-GES   PIC S9(04)  COMP.
           10 NI-MSBS-EH-EIG-PZTR  PIC S9(04)  COMP.
           10 NI-MSBS-ZEITRAUM     PIC S9(04)  COMP.
           10 NI-MSBS-EQF-EIG      PIC S9(04)  COMP.
           10 NI-MSBS-PROV-STF     PIC S9(04)  COMP.
           10 NI-MSBS-LAUFZEIT     PIC S9(04)  COMP.
           10 NI-MSBS-GUELTIG-BIS  PIC S9(04)  COMP.
           10 NI-MSBS-BEZ          PIC S9(04)  COMP.
        05  DCL-TBVB0008.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 BS-NR                PIC S9(4) USAGE COMP.
           10 MSBS-EH-EIG-GES      PIC S999999999V9 USAGE COMP-3.
           10 MSBS-EH-EIG-PZTR     PIC S999999999V9 USAGE COMP-3.
           10 MSBS-ZEITRAUM        PIC S9(4) USAGE COMP.
           10 MSBS-EQF-EIG         PIC S999V99 USAGE COMP-3.
           10 MSBS-PROV-STF        PIC S999V99 USAGE COMP-3.
           10 MSBS-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 MSBS-GUELTIG-BIS     PIC X(10).
           10 MSBS-BEZ             PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
