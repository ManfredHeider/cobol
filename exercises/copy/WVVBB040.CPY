      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB040.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB040.CPv  $
      *
      *   Rev 2.1   30 Oct 2007 11:06:26   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.0   05 Jun 2007 15:59:14   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB040                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    05.06.2007                                       *
      *****************************************************************
      *    VERSION   1    05.06.07
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00905!05.06.07! 01 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB040                           *
      ******************************************************************
        05  WB-NI-VIVBB040-TAB.
           10  WB-NI-VIVBB040-ELE  PIC S9(04)  COMP   OCCURS 13  TIMES.
        05  WB-NI-VIVBB040         REDEFINES   WB-NI-VIVBB040-TAB.
           10 WB-NI-KL-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-KL-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-KL-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-KL-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KL-PROV-STZ-AP PIC S9(04)  COMP.
           10 WB-NI-KL-PROV-STZ-GP PIC S9(04)  COMP.
           10 WB-NI-KL-PROZ-STZ    PIC S9(04)  COMP.
           10 WB-NI-KL-RCK-STZ-DAT PIC S9(04)  COMP.
           10 WB-NI-KL-RCK-STZ     PIC S9(04)  COMP.
           10 WB-NI-MA-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-KNR-KL-TYP     PIC S9(04)  COMP.
           10 WB-NI-KTY-KL-TYP     PIC S9(04)  COMP.
           10 WB-NI-KL-AM-AGENT-NR PIC S9(04)  COMP.
        05  WB-VIVBB040.
           10 WB-KL-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KL-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KL-DAT-VON        PIC X(10)                VALUE SPACE.
           10 WB-KL-DAT-BIS        PIC X(10)                VALUE SPACE.
           10 WB-KL-PROV-STZ-AP    PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-PROV-STZ-GP    PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-PROZ-STZ       PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-KL-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-KL-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-KNR-KL-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KTY-KL-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KL-AM-AGENT-NR    PIC S9(9)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
