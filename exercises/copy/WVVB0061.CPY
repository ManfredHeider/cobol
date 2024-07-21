      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.07.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0061.CPv  $
      *
      *   Rev 2.1   Aug 17 2005 09:13:54   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   Jul 22 2005 10:45:24   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0061                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR TBVB0061 (CLPARTNER)                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    22.07.2005                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!22.07.05!2.1 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
           05  WB-NI-VIVB0061-TAB.
              10 WB-NI-VIVB0061-ELE  PIC S9(04) BINARY OCCURS 7 TIMES.
           05  WB-NI-VIVB0061        REDEFINES  WB-NI-VIVB0061-TAB.
              10 WB-NI-CLPAR-BETR-NR           PIC S9(04)       BINARY.
              10 WB-NI-KTY-CLB-TYP             PIC S9(04)       BINARY.
              10 WB-NI-KNR-CLB-TYP             PIC S9(04)       BINARY.
              10 WB-NI-CLPAR-UNTST-NR          PIC S9(04)       BINARY.
              10 WB-NI-UNTST-MS-NR             PIC S9(04)       BINARY.
              10 WB-NI-CLPAR-UNTST-EH          PIC S9(04)       BINARY.
              10 WB-NI-CLPAR-DAT-VON           PIC S9(04)       BINARY.

           05  WB-VIVB0061.
              10 WB-CLPAR-BETR-NR              PIC S9(9)    COMP.
              10 WB-KTY-CLB-TYP                PIC S9(4)    COMP.
              10 WB-KNR-CLB-TYP                PIC S9(4)    COMP.
              10 WB-CLPAR-UNTST-NR             PIC S9(9)    COMP.
              10 WB-UNTST-MS-NR                PIC S9(4)    COMP.
              10 WB-CLPAR-UNTST-EH             PIC S9(9)V9  COMP-3.
              10 WB-CLPAR-DAT-VON              PIC X(10)      .
