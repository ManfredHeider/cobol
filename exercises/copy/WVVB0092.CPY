      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0092.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0092.CPv  $
      *
      *   Rev 2.1   Oct 07 2004 07:50:52   t13ex43
      *ASL00183: PGM - Ermittlung MAP-Kandidaten für D-Leiter
      *
      *   Rev 2.0   Jul 02 2004 15:56:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0092                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0092 (MAPB)                            *
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
      * COBOL DECLARATION FOR TABLE VIVB0092                          *
      *****************************************************************
        05  WB-NI-VIVB0092-TAB.
           10 WB-NI-VIVB0092-ELE   PIC S9(04)  BINARY OCCURS   9 TIMES.
        05  WB-NI-VIVB0092         REDEFINES   WB-NI-VIVB0092-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-MAPB-MONAT            PIC S9(04)       BINARY.
           10 WB-NI-MAPB-STUFE-AKT        PIC S9(04)       BINARY.
           10 WB-NI-MAPB-STUFE-NEXT       PIC S9(04)       BINARY.
           10 WB-NI-MAPB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-MAPB-EH-ZUGANG        PIC S9(04)       BINARY.
           10 WB-NI-MAPB-EH-ABGANG        PIC S9(04)       BINARY.
           10 WB-NI-MAPB-EH-GESAMT        PIC S9(04)       BINARY.
           10 WB-NI-MAPB-EQF-GESAMT       PIC S9(04)       BINARY.

        05  WB-VIVB0092.
           10 WB-MA-ST-NR                    PIC S9(09)       BINARY.
           10 WB-MAPB-MONAT                  PIC X(10).
           10 WB-MAPB-STUFE-AKT              PIC X(04).
           10 WB-MAPB-STUFE-NEXT             PIC X(04).
           10 WB-MAPB-LAST-UPDATE            PIC X(26).
           10 WB-MAPB-EH-ZUGANG              PIC S9(09)V9(01) COMP-3.
           10 WB-MAPB-EH-ABGANG              PIC S9(09)V9(01) COMP-3.
           10 WB-MAPB-EH-GESAMT              PIC S9(09)V9(01) COMP-3.
           10 WB-MAPB-EQF-GESAMT             PIC S9(03)V9(02) COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS  9      *
      ******************************************************************
