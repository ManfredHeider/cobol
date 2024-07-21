      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0108.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0108.CPv  $
      *
      *   Rev 2.1   03 Sep 2007 16:55:22   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:38:18   t13ae35
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *    COPY-MEMBER: WVVB0108                                       *
      *    --------------------------------------------------------    *
      *    WORKBEREICH FÜR VIVB0108 (SWNB)                             *
      *    --------------------------------------------------------    *
      *    ERSTELLT: J.Hering                                          *
      *    DATUM:    28.08.2007                                        *
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * ARW00041!28.08.07! 2.1! NEU                       ! J.Hering   *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0108                           *
      ******************************************************************
        05  WB-NI-VIVB0108-TAB.
           10 WB-NI-VIVB0108-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.
        05  WB-NI-VIVB0108         REDEFINES   WB-NI-VIVB0108-TAB.
           10 WB-NI-ST-NR                  PIC S9(04)       BINARY.
           10 WB-NI-SWNB-ERM-VOM           PIC S9(04)       BINARY.
           10 WB-NI-KNR-ERM-ART            PIC S9(04)       BINARY.
           10 WB-NI-KTY-ERM-ART            PIC S9(04)       BINARY.
           10 WB-NI-SWNB-MS-NR             PIC S9(04)       BINARY.
           10 WB-NI-SWNB-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG           PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG           PIC S9(04)       BINARY.
           10 WB-NI-SWNB-BETRAG            PIC S9(04)       BINARY.
           10 WB-NI-KTY-BERECH-STAT        PIC S9(04)       BINARY.
           10 WB-NI-KNR-BERECH-STAT        PIC S9(04)       BINARY.

        05  WB-VIVB0108.
           10 WB-ST-NR                PIC S9(9)    BINARY   VALUE ZERO.
           10 WB-SWNB-ERM-VOM         PIC X(10)             VALUE SPACE.
           10 WB-KNR-ERM-ART          PIC S9(4)    BINARY   VALUE ZERO.
           10 WB-KTY-ERM-ART          PIC S9(4)    BINARY   VALUE ZERO.
           10 WB-SWNB-MS-NR           PIC S9(4)    BINARY   VALUE ZERO.
           10 WB-SWNB-LAST-UPDATE     PIC X(26)    VALUE SPACE.
           10 WB-KTY-WAEHRUNG         PIC S9(4)    BINARY   VALUE ZERO.
           10 WB-KNR-WAEHRUNG         PIC S9(4)    BINARY   VALUE ZERO.
           10 WB-SWNB-BETRAG          PIC S9(9)V99 COMP-3   VALUE ZERO.
           10 WB-KTY-BERECH-STAT      PIC S9(4)    BINARY   VALUE ZERO.
           10 WB-KNR-BERECH-STAT      PIC S9(4)    BINARY   VALUE ZERO.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

