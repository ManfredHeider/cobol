      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0095.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0095.CPv  $
      *
      *   Rev 2.1   Oct 07 2004 07:51:08   t13ex43
      *ASL00183: PGM - Ermittlung MAP-Kandidaten für D-Leiter
      *
      *   Rev 2.0   Jul 02 2004 15:57:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0095                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0095 (MAPC)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    12.07.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!12.07.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0095                          *
      *****************************************************************
        05  WB-NI-VIVB0095-TAB.
           10 WB-NI-VIVB0095-ELE   PIC S9(04)  BINARY OCCURS   8 TIMES.
        05  WB-NI-VIVB0095         REDEFINES   WB-NI-VIVB0095-TAB.
           10 WB-NI-MAPC-BETR-NR          PIC S9(04)       BINARY.
           10 WB-NI-MAPC-MONAT            PIC S9(04)       BINARY.
           10 WB-NI-MAPC-UNTST-NR         PIC S9(04)       BINARY.
           10 WB-NI-MAPC-VB-SEIT          PIC S9(04)       BINARY.
           10 WB-NI-MAPC-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-MAPC-EH-ZUGANG        PIC S9(04)       BINARY.
           10 WB-NI-MAPC-EH-ABGANG        PIC S9(04)       BINARY.
           10 WB-NI-MAPC-EH-GESAMT        PIC S9(04)       BINARY.

        05  WB-VIVB0095.
           10 WB-MAPC-BETR-NR                PIC S9(09)       BINARY.
           10 WB-MAPC-MONAT                  PIC X(10).
           10 WB-MAPC-UNTST-NR               PIC S9(09)       BINARY.
           10 WB-MAPC-VB-SEIT                PIC X(10).
           10 WB-MAPC-LAST-UPDATE            PIC X(26).
           10 WB-MAPC-EH-ZUGANG              PIC S9(09)V9(01) COMP-3.
           10 WB-MAPC-EH-ABGANG              PIC S9(09)V9(01) COMP-3.
           10 WB-MAPC-EH-GESAMT              PIC S9(09)V9(01) COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  8      *
      ******************************************************************
