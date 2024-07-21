      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA038.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 09:00:52   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Jan 26 2006 15:18:32   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA038                                      *
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
      * COBOL DECLARATION FOR TABLE  WB-VIVBA038                       *
      ******************************************************************
        05  WB-NI-VIVBA038-TAB.
           10  WB-NI-VIVBA038-ELE  PIC S9(04)  COMP   OCCURS  13 TIMES.
        05  WB-NI-VIVBA038           REDEFINES   WB-NI-VIVBA038-TAB.
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
        05  WB-VIVBA038.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
