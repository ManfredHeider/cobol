      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0107.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0107.CPv  $
      *
      *   Rev 2.1   03 Sep 2007 16:55:36   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung
      *
      *   Rev 2.0   28 Aug 2007 15:38:34   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0107                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0107 (SWNS)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    28.08.2007                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!28.08.07! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0107                           *
      ******************************************************************
        05  WB-NI-VIVB0107-TAB.
           10 WB-NI-VIVB0107-ELE   PIC S9(04)  BINARY OCCURS  6 TIMES.
        05  WB-NI-VIVB0107         REDEFINES   WB-NI-VIVB0107-TAB.
           10 WB-NI-ST-NR                  PIC S9(04)       BINARY.
           10 WB-NI-KNR-AKZEPT-STAT        PIC S9(04)       BINARY.
           10 WB-NI-SWNS-DAT-BIS           PIC S9(04)       BINARY.
           10 WB-NI-KTY-AKZEPT-STAT        PIC S9(04)       BINARY.
           10 WB-NI-SWNS-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-SWNS-LAST-UPDATE       PIC S9(04)       BINARY.

        05  WB-VIVB0107.
           10 WB-ST-NR                PIC S9(9)   BINARY    VALUE ZERO.
           10 WB-KNR-AKZEPT-STAT      PIC S9(4)   BINARY    VALUE ZERO.
           10 WB-SWNS-DAT-BIS         PIC X(10)             VALUE SPACE.
           10 WB-KTY-AKZEPT-STAT      PIC S9(4)   BINARY    VALUE ZERO.
           10 WB-SWNS-DAT-VON         PIC X(26)   VALUE SPACE.
           10 WB-SWNS-LAST-UPDATE     PIC X(26)   VALUE SPACE.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************

