      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBB038.CPv  $
      *
      *   Rev 2.1   Mar 24 2009 14:20:56   t13ae27
      *AAS00813: Einheiten neuer Partner
      *
      *   Rev 2.0   Mar 12 2009 10:31:26   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE( VIVBB038)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA038))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VIVBB038 TABLE
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
CCC004*      PARTEH_DRUCK_KZ                SMALLINT,
CCC004*      PARTEH_BETR_TYP                CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VIVBB038                          *
      ******************************************************************
       01  VIVBB038-GESAMT.
        05  NI-VIVBB038-TAB.
           10  NI-VIVBB038-ELE     PIC S9(04)  COMP   OCCURS 14 TIMES.
        05  NI-VIVBB038            REDEFINES   NI-VIVBB038-TAB.
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
           10 NI-PARTEH-BETR-TYP       PIC S9(04)  COMP.
        05  DCL-VIVBB038.
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
           10 PARTEH-BETR-TYP          PIC X(001).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 20      *
      ******************************************************************
