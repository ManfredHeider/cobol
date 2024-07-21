      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA038.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 09:03:06   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Jan 26 2006 15:03:10   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( VIVBA038)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA038))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VIVBA038 TABLE
CCC004*    ( PARTEH_BETR                    INTEGER        NOT NULL,
CCC004*      PARTEH_UNTST                   INTEGER        NOT NULL,
CCC004*      PARTEH_MONAT                   DATE           NOT NULL,
CCC004*      KNR_WB_TYP                     SMALLINT       NOT NULL,
CCC004*      KNR_WERTUNGS_TYP               SMALLINT       NOT NULL,
CCC004*      KTY_WB_TYP                     SMALLINT       NOT NULL,
CCC004*      KTY_WERTUNGS_TYP               SMALLINT       NOT NULL,
CCC004*      PARTEH_LAST_UPDATE             TIMESTAMP      NOT NULL,
CCC004*      PARTEH_EH                      DECIMAL(10, 1) NOT NULL,
CCC004*      PARTEH_EBENE                   SMALLINT       NOT NULL,
CCC004*      PARTEH_KNOTEN                  SMALLINT,
CCC004*      PARTEH_URSPR_STRUK             CHAR(1)        NOT NULL,
CCC004*      PARTEH_DRUCK_KZ                SMALLINT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VIVBA038                          *
      ******************************************************************
       01  VIVBA038-GESAMT.
        05  NI-VIVBA038-TAB.
           10  NI-VIVBA038-ELE     PIC S9(04)  COMP   OCCURS 13 TIMES.
        05  NI-VIVBA038            REDEFINES   NI-VIVBA038-TAB.
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
           10 NI-PARTEH-KNOTEN         PIC S9(04)  COMP.
           10 NI-PARTEH-URSPR-STRUK    PIC S9(04)  COMP.
           10 NI-PARTEH-DRUCK-KZ       PIC S9(04)  COMP.
        05  DCL-VIVBA038.
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
           10 PARTEH-KNOTEN            PIC S9(04)  COMP.
           10 PARTEH-URSPR-STRUK       PIC X(001).
           10 PARTEH-DRUCK-KZ          PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
