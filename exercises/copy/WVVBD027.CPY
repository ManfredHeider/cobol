      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBD027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBD027.CPv  $
      *
      *   Rev 2.1   May 22 2014 10:10:04   t13ae13
      *ASL00509: FAP Dialogänderung Vb 31
      *
      *   Rev 2.0   Jan 21 2014 11:31:48   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBD027                           *
      ******************************************************************
        05  WB-NI-VIVBD027-TAB.
           10  WB-NI-VIVBD027-ELE  PIC S9(04)  COMP   OCCURS  17 TIMES.
        05  WB-NI-VIVBD027           REDEFINES   WB-NI-VIVBD027-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-FAPZ-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-FAPZ-VERS-NR     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-STF         PIC S9(04)  COMP.
           10 WB-NI-FAPZ-AMLVERS-SUM PIC S9(04)  COMP.
           10 WB-NI-FAPZ-NEU-KZ      PIC S9(04)  COMP.
           10 WB-NI-FAPZ-BEITRAG     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-SELBSTBET   PIC S9(04)  COMP.
           10 WB-NI-FAPZ-DVAG-STF    PIC S9(04)  COMP.
           10 WB-NI-FAPZ-UV-KZ       PIC S9(04)  COMP.
           10 WB-NI-FAPZ-AMLUV-SUM   PIC S9(04)  COMP.
           10 WB-NI-FAPZ-DVAGUV-SUM  PIC S9(04)  COMP.
           10 WB-NI-FAPZ-DVAGVER-SUM PIC S9(04)  COMP.
           10 WB-NI-FAPZ-BEITRAG-UV  PIC S9(04)  COMP.
           10 WB-NI-FAPZ-SELBSTB-UV  PIC S9(04)  COMP.
           10 WB-NI-FAPZ-TEILABL-KZ  PIC S9(04)  COMP.
        05  WB-VIVBD027.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-FAPZ-DAT-VON      PIC X(10)                VALUE SPACE.
           10 WB-FAPZ-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-FAPZ-VERS-NR      PIC S9(15)   COMP-3      VALUE ZERO.
           10 WB-FAPZ-STF          PIC X(1)                 VALUE SPACE.
           10 WB-FAPZ-AMLVERS-SUM  PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-NEU-KZ       PIC X(1).
           10 WB-FAPZ-BEITRAG      PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-SELBSTBET    PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-DVAG-STF     PIC X(1)                 VALUE SPACE.
           10 WB-FAPZ-UV-KZ        PIC X(1).
           10 WB-FAPZ-AMLUV-SUM    PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-DVAGUV-SUM   PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-DVAGVER-SUM  PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-BEITRAG-UV   PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-SELBSTB-UV   PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-TEILABL-KZ   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************

