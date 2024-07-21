      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB038.CPv  $
      *
      *   Rev 2.1   Mar 24 2009 14:20:48   t13ae27
      *AAS00813: Einheiten neuer Partner
      *
      *   Rev 2.0   Mar 12 2009 10:31:16   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB038                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 *
      *    DATUM:    22.04.1993                                       *
      *    STAND:    22.04.1993                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *    Version:  003 Verwendung der Tab. + COPY für PARTEH
      *****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  WB-VIVBB038                       *
      ******************************************************************
        05  WB-NI-VIVBB038-TAB.
           10  WB-NI-VIVBB038-ELE  PIC S9(04)  COMP   OCCURS  14 TIMES.
        05  WB-NI-VIVBB038           REDEFINES   WB-NI-VIVBB038-TAB.
           10 WB-NI-PARTEH-BETR        PIC S9(04)  COMP.
           10 WB-NI-PARTEH-UNTST       PIC S9(04)  COMP.
           10 WB-NI-PARTEH-MONAT       PIC S9(04)  COMP.
           10 WB-NI-KNR-WB-TYP         PIC S9(04)  COMP.
           10 WB-NI-KNR-WERTUNGS-TYP   PIC S9(04)  COMP.
           10 WB-NI-KTY-WB-TYP         PIC S9(04)  COMP.
           10 WB-NI-KTY-WERTUNGS-TYP   PIC S9(04)  COMP.
           10 WB-NI-PARTEH-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-PARTEH-EH          PIC S9(04)  COMP.
           10 WB-NI-PARTEH-EBENE       PIC S9(04)  COMP.
           10 WB-NI-PARTEH-KNOTEN      PIC S9(04)  COMP.
           10 WB-NI-PARTEH-URSPR-STRUK PIC S9(04)  COMP.
           10 WB-NI-PARTEH-DRUCK-KZ    PIC S9(04)  COMP.
           10 WB-NI-PARTEH-BETR-TYP    PIC S9(04)  COMP.
        05  WB-VIVBB038.
           10 WB-PARTEH-BETR           PIC S9(9)   COMP     VALUE ZERO.
           10 WB-PARTEH-UNTST          PIC S9(9)   COMP     VALUE ZERO.
           10 WB-PARTEH-MONAT          PIC X(10)            VALUE SPACE.
           10 WB-KNR-WB-TYP            PIC S9(4)   COMP     VALUE ZERO.
           10 WB-KNR-WERTUNGS-TYP      PIC S9(4)   COMP     VALUE ZERO.
           10 WB-KTY-WB-TYP            PIC S9(4)   COMP     VALUE ZERO.
           10 WB-KTY-WERTUNGS-TYP      PIC S9(4)   COMP     VALUE ZERO.
           10 WB-PARTEH-LAST-UPDATE    PIC X(26)            VALUE SPACE.
           10 WB-PARTEH-EH             PIC S9(9)V9 COMP-3   VALUE ZERO.
           10 WB-PARTEH-EBENE          PIC S9(4)   COMP     VALUE ZERO.
           10 WB-PARTEH-KNOTEN         PIC S9(04)  COMP.
           10 WB-PARTEH-URSPR-STRUK    PIC X(1).
           10 WB-PARTEH-DRUCK-KZ       PIC S9(04)  COMP.
           10 WB-PARTEH-BETR-TYP       PIC X(001).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
