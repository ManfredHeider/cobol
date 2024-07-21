      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0110.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0110.CPv  $
      *
      *   Rev 2.1   31 Oct 2007 12:05:26   t13ae13
      *AVB00905: Allfinanz Stufe 1 im VB-System: VB14, VB22, VB23 (
      *SWL)
      *
      *   Rev 2.0   25 Sep 2007 12:03:52   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0110                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    25.09.2007                                       *
      *    --------------------------------------------------------   *
      *    VERSION:  001                                              *
      *****************************************************************
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!25.09.07!2.1 ! NEU                       ! S. Walb  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0110                           *
      ******************************************************************
        05  WB-NI-VIVB0110-TAB.
           10 WB-NI-VIVB0110-ELE      PIC S9(04)  COMP  OCCURS 30 TIMES.
        05  WB-NI-VIVB0110            REDEFINES   WB-NI-VIVB0110-TAB.
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
        05  WB-VIVB0110.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
