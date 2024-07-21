      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0312.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0312.CPv  $
      *
      *   Rev 2.1   Jul 07 2011 15:49:14   t13ae30
      *AAS00964: Zustrukturierung von Partnern
      *
      *   Rev 2.0   May 11 2011 17:07:40   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0312                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0312 (DIRW    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    11.05.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00964!11.05.11!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0312                           *
      *****************************************************************

        05 WB-NI-VIVB0312-TAB.
           10 WB-NI-VIVB0312-ELE   PIC S9(04)  BINARY OCCURS   6.
        05 WB-NI-VIVB0312          REDEFINES   WB-NI-VIVB0312-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-DIR-ALT               PIC S9(04)       BINARY.
           10 WB-NI-DIR-NEU               PIC S9(04)       BINARY.
           10 WB-NI-DIRW-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-DIRW-DAT-BIS          PIC S9(04)       BINARY.
           10 WB-NI-DIRW-LAST-UPDATE      PIC S9(04)       BINARY.

        05 WB-VIVB0312.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-DIR-ALT                  PIC S9(04)       BINARY.
           10 WB-DIR-NEU                  PIC S9(04)       BINARY.
           10 WB-DIRW-DAT-VON             PIC  X(26).
           10 WB-DIRW-DAT-BIS             PIC  X(10).
           10 WB-DIRW-LAST-UPDATE         PIC  X(26).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   6                    *
      *****************************************************************
