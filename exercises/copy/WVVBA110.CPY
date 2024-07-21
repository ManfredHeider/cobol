      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA110.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA110.CPv  $
      *
      *   Rev 2.1   02 Jan 2008 11:13:36   t13ae13
      *AVB00905: Allfinanz - Regelbetrieb für VB14 und VB22 (SWL)
      *
      *   Rev 2.0   11 Dec 2007 09:05:28   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA110                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    11.12.2007                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!11.12.07!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA110                           *
      ******************************************************************
        05  WB-NI-VIVBA110-TAB.
           10 WB-NI-VIVBA110-ELE      PIC S9(04)  COMP  OCCURS 31 TIMES.
        05  WB-NI-VIVBA110            REDEFINES   WB-NI-VIVBA110-TAB.
           10 WB-NI-AMUM-AM-AG-NR     PIC S9(04)  COMP.
           10 WB-NI-AMUM-LFD-NR       PIC S9(04)  COMP.
           10 WB-NI-AMUM-NACHNAME     PIC S9(04)  COMP.
           10 WB-NI-AMUM-VORNAME      PIC S9(04)  COMP.
           10 WB-NI-KTY-ANREDE        PIC S9(04)  COMP.
           10 WB-NI-KNR-ANREDE        PIC S9(04)  COMP.
           10 WB-NI-AMUM-AD-GES-TITEL PIC S9(04)  COMP.
           10 WB-NI-AMUM-BL-KZ        PIC S9(04)  COMP.
           10 WB-NI-AMUM-EU-REG-NR    PIC S9(04)  COMP.
           10 WB-NI-AMUM-PLZ          PIC S9(04)  COMP.
           10 WB-NI-AMUM-ORT          PIC S9(04)  COMP.
           10 WB-NI-AMUM-STR          PIC S9(04)  COMP.
           10 WB-NI-AMUM-INT-AG-NR    PIC S9(04)  COMP.
           10 WB-NI-AMUM-NAME         PIC S9(04)  COMP.
           10 WB-NI-AMUM-DIR          PIC S9(04)  COMP.
           10 WB-NI-AMUM-MSNR         PIC S9(04)  COMP.
           10 WB-NI-AMUM-STUFE        PIC S9(04)  COMP.
           10 WB-NI-AMUM-PRAXIS       PIC S9(04)  COMP.
           10 WB-NI-AMUM-AMAG-NR-B    PIC S9(04)  COMP.
           10 WB-NI-AMUM-NAME-B       PIC S9(04)  COMP.
           10 WB-NI-AMUM-STUFE-B      PIC S9(04)  COMP.
           10 WB-NI-AMUM-PRAXIS-B     PIC S9(04)  COMP.
           10 WB-NI-AMUM-STEUER-NR    PIC S9(04)  COMP.
           10 WB-NI-AMUM-KTO-NR       PIC S9(04)  COMP.
           10 WB-NI-AMUM-BLZ          PIC S9(04)  COMP.
           10 WB-NI-AMUM-BANK         PIC S9(04)  COMP.
           10 WB-NI-AMUM-GEB-DAT      PIC S9(04)  COMP.
           10 WB-NI-AMUM-EIN-DAT      PIC S9(04)  COMP.
           10 WB-NI-AMUM-UMSTEIG-KZ   PIC S9(04)  COMP.
           10 WB-NI-AMUM-EIGENREG-KZ  PIC S9(04)  COMP.
           10 WB-NI-AMUM-UMSTEIG-TYP  PIC S9(04)  COMP.
        05  WB-VIVBA110.
           10 WB-AMUM-AM-AG-NR       PIC S9(9)   COMP      VALUE ZERO.
           10 WB-AMUM-LFD-NR         PIC S9(4)   COMP      VALUE ZERO.
           10 WB-AMUM-NACHNAME       PIC X(40)             VALUE SPACE.
           10 WB-AMUM-VORNAME        PIC X(20)             VALUE SPACE.
           10 WB-KTY-ANREDE          PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-ANREDE          PIC S9(4)   COMP      VALUE ZERO.
           10 WB-AMUM-AD-GES-TITEL   PIC X(10)             VALUE SPACE.
           10 WB-AMUM-BL-KZ          PIC X(1)              VALUE SPACE.
           10 WB-AMUM-EU-REG-NR      PIC X(30)             VALUE SPACE.
           10 WB-AMUM-PLZ            PIC X(9)              VALUE SPACE.
           10 WB-AMUM-ORT            PIC X(40)             VALUE SPACE.
           10 WB-AMUM-STR            PIC X(40)             VALUE SPACE.
           10 WB-AMUM-INT-AG-NR      PIC S9(4)   COMP      VALUE ZERO.
           10 WB-AMUM-NAME           PIC X(40)             VALUE SPACE.
           10 WB-AMUM-DIR            PIC S9(04)  COMP      VALUE ZERO.
           10 WB-AMUM-MSNR           PIC S9(04)  COMP      VALUE ZERO.
           10 WB-AMUM-STUFE          PIC X(30)             VALUE SPACE.
           10 WB-AMUM-PRAXIS         PIC S9(04)  COMP      VALUE ZERO.
           10 WB-AMUM-AMAG-NR-B      PIC S9(9)   COMP      VALUE ZERO.
           10 WB-AMUM-NAME-B         PIC X(40)             VALUE SPACE.
           10 WB-AMUM-STUFE-B        PIC X(30)             VALUE SPACE.
           10 WB-AMUM-PRAXIS-B       PIC S9(04)  COMP      VALUE ZERO.
           10 WB-AMUM-STEUER-NR      PIC X(20)             VALUE SPACE.
           10 WB-AMUM-KONTO-NR       PIC X(20)             VALUE SPACE.
           10 WB-AMUM-BLZ            PIC X(11)             VALUE SPACE.
           10 WB-AMUM-BANK           PIC X(40)             VALUE SPACE.
           10 WB-AMUM-GEB-DAT        PIC X(10)             VALUE SPACE.
           10 WB-AMUM-EIN-DAT        PIC X(10)             VALUE SPACE.
           10 WB-AMUM-UMSTEIG-KZ     PIC X(1)              VALUE SPACE.
           10 WB-AMUM-EIGENREG-KZ    PIC X(1)              VALUE SPACE.
           10 WB-AMUM-UMSTEIG-TYP    PIC X(2)              VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *
      ******************************************************************
