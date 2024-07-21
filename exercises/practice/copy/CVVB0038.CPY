      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0038.CPv  $
      *
      *   Rev 2.1   Feb 11 2005 11:41:26   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Feb 27 1999 15:56:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:19:16   P13SP13
      *Initial revision.
      *
      *    Version:  003 Verwendung der Tab. + COPY für PARTEH
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( VIVB0038)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0038))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VIVB0038 TABLE
CCC004*    ( PARTEH_BETR                    INTEGER        NOT NULL,
CCC004*      PARTEH_UNTST                   INTEGER        NOT NULL,
CCC004*      PARTEH_MONAT                   DATE           NOT NULL,
CCC004*      KNR_WB_TYP                     SMALLINT       NOT NULL,
CCC004*      KNR_WERTUNGS_TYP               SMALLINT       NOT NULL,
CCC004*      KTY_WB_TYP                     SMALLINT       NOT NULL,
CCC004*      KTY_WERTUNGS_TYP               SMALLINT       NOT NULL,
CCC004*      PARTEH_LAST_UPDATE             TIMESTAMP      NOT NULL,
CCC004*      PARTEH_EH                      DECIMAL(10, 1) NOT NULL,
CCC004*      PARTEH_EBENE                   SMALLINT       NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VIVB0038                          *
      ******************************************************************
       01  VIVB0038-GESAMT.
        05  NI-VIVB0038-TAB.
           10  NI-VIVB0038-ELE     PIC S9(04)  COMP   OCCURS 10 TIMES.
        05  NI-VIVB0038            REDEFINES   NI-VIVB0038-TAB.
           10 NI-PARTEH-BETR           PIC S9(04)  COMP.
           10 NI-PARTEH-UNTST          PIC S9(04)  COMP.
           10 NI-PARTEH-MONAT          PIC S9(04)  COMP.
           10 NI-KNR-WB-TYP            PIC S9(04)  COMP.
           10 NI-KNR-WERTUNGS-TYP      PIC S9(04)  COMP.
           10 NI-KTY-WB-TYP            PIC S9(04)  COMP.
           10 NI-KTY-WERTUNGS-TYP      PIC S9(04)  COMP.
           10 NI-PARTEH-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-PARTEH-EH             PIC S9(04)  COMP.
           10 NI-PARTEH-EBENE          PIC S9(04)  COMP.
        05  DCL-VIVB0038.
           10 PARTEH-BETR              PIC S9(9) USAGE COMP.
           10 PARTEH-UNTST             PIC S9(9) USAGE COMP.
           10 PARTEH-MONAT             PIC X(10).
           10 KNR-WB-TYP               PIC S9(4) USAGE COMP.
           10 KNR-WERTUNGS-TYP         PIC S9(4) USAGE COMP.
           10 KTY-WB-TYP               PIC S9(4) USAGE COMP.
           10 KTY-WERTUNGS-TYP         PIC S9(4) USAGE COMP.
           10 PARTEH-LAST-UPDATE       PIC X(026).
           10 PARTEH-EH                PIC S999999999V9 USAGE COMP-3.
           10 PARTEH-EBENE             PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
