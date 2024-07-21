      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA027.CPv  $
      *
      *   Rev 2.1   Sep 22 2004 11:20:10   t13ex57
      *ASL00187: FAP neue Versicherungssummen
      *
      *   Rev 2.0   Aug 05 2004 15:54:12   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA027                           *
      ******************************************************************
        05  WB-NI-VIVBA027-TAB.
           10  WB-NI-VIVBA027-ELE  PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  WB-NI-VIVBA027           REDEFINES   WB-NI-VIVBA027-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-FAPZ-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-FAPZ-VERS-NR     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-STF         PIC S9(04)  COMP.
           10 WB-NI-FAPZ-AMLVERS-SUM PIC S9(04)  COMP.
           10 WB-NI-FAPZ-NEU-KZ      PIC S9(04)  COMP.
        05  WB-VIVBA027.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-FAPZ-DAT-VON      PIC X(10)                VALUE SPACE.
           10 WB-FAPZ-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-FAPZ-VERS-NR      PIC S9(15)   COMP-3      VALUE ZERO.
           10 WB-FAPZ-STF          PIC X(1)                 VALUE SPACE.
           10 WB-FAPZ-AMLVERS-SUM  PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-NEU-KZ       PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
