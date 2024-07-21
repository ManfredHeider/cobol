      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB027.CPv  $
      *
      *   Rev 2.1   Aug 19 2008 11:33:48   t13ex13
      *ASL00304: Familien-Absicherungsplan
      *
      *   Rev 2.0   Jun 27 2008 13:35:38   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB027                           *
      ******************************************************************
        05  WB-NI-VIVBB027-TAB.
           10  WB-NI-VIVBB027-ELE  PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  WB-NI-VIVBB027           REDEFINES   WB-NI-VIVBB027-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-FAPZ-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-FAPZ-VERS-NR     PIC S9(04)  COMP.
           10 WB-NI-FAPZ-STF         PIC S9(04)  COMP.
           10 WB-NI-FAPZ-AMLVERS-SUM PIC S9(04)  COMP.
           10 WB-NI-FAPZ-NEU-KZ      PIC S9(04)  COMP.
           10 WB-NI-FAPZ-BEITRAG     PIC S9(04)  COMP.
        05  WB-VIVBB027.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-FAPZ-DAT-VON      PIC X(10)                VALUE SPACE.
           10 WB-FAPZ-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 WB-FAPZ-VERS-NR      PIC S9(15)   COMP-3      VALUE ZERO.
           10 WB-FAPZ-STF          PIC X(1)                 VALUE SPACE.
           10 WB-FAPZ-AMLVERS-SUM  PIC S999999999V99 USAGE COMP-3.
           10 WB-FAPZ-NEU-KZ       PIC X(1).
           10 WB-FAPZ-BEITRAG      PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************

