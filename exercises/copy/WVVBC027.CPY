      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBC027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBC027.CPv  $
      *
      *   Rev 2.1   Dec 18 2013 08:24:46   t13ae13
      *ASL00490: Familienabsicherungsplan alle Vertriebsgesellschaf
      *ten
      *
      *   Rev 2.0   Nov 11 2013 13:47:34   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBC027                           *
      ******************************************************************
        05  WB-NI-VIVBC027-TAB.
           10  WB-NI-VIVBC027-ELE  PIC S9(04)  COMP   OCCURS  11 TIMES.
        05  WB-NI-VIVBC027           REDEFINES   WB-NI-VIVBC027-TAB.
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
        05  WB-VIVBC027.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

