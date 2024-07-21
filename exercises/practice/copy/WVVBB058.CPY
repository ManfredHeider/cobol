      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBB058.CPv  $
      *
      *   Rev 2.1   Dec 21 2017 10:37:08   t13ae30
      *AVB01474: 3. Statistik-Kategorie Folge Neu - Programme
      *
      *   Rev 2.0   Dec 13 2017 11:33:00   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB058                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVBB058 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    13.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01474!13.12.17!2.1 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB058                           *
      *****************************************************************

        05 WB-NI-VIVBB058-TAB.
           10 WB-NI-VIVBB058-ELE   PIC S9(04)  BINARY OCCURS  32.
        05 WB-NI-VIVBB058          REDEFINES   WB-NI-VIVBB058-TAB.
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.
           10 WB-NI-PSEV-MONAT            PIC S9(04)       BINARY.
           10 WB-NI-KNR-PROD-SPARTE       PIC S9(04)       BINARY.
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-KTY-PROD-SPARTE       PIC S9(04)       BINARY.
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-GS-ZU-EG      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-GS-ZU-GR      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-GS-AB-EG      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-GS-AB-GR      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-PV-GS-NT-EG      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-PV-GS-NT-GR      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-PV-FG-NT-EG      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-PV-FG-NT-GR      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-FG-ZU-EG      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-FG-ZU-GR      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-FG-AB-EG      PIC S9(04)       BINARY.
           10 WB-NI-PSEV-EH-FG-AB-GR      PIC S9(04)       BINARY.
           10 WB-NI-KTY-UST-KZ            PIC S9(04)       BINARY.
           10 WB-NI-KNR-UST-KZ            PIC S9(04)       BINARY.
           10 WB-NI-PV-GS-NT-EG-USTANT    PIC S9(04)       BINARY.
           10 WB-NI-PV-GS-NT-GR-USTANT    PIC S9(04)       BINARY.
           10 WB-NI-PV-FG-NT-EG-USTANT    PIC S9(04)       BINARY.
           10 WB-NI-PV-FG-NT-GR-USTANT    PIC S9(04)       BINARY.
           10 WB-NI-PV-NEUR-NT-EG         PIC S9(04)       BINARY.
           10 WB-NI-PV-NEUR-NT-GR         PIC S9(04)       BINARY.
           10 WB-NI-PV-NEUR-NT-EG-UST     PIC S9(04)       BINARY.
           10 WB-NI-PV-NEUR-NT-GR-UST     PIC S9(04)       BINARY.
           10 WB-NI-EH-NEUR-ZU-EG         PIC S9(04)       BINARY.
           10 WB-NI-EH-NEUR-ZU-GR         PIC S9(04)       BINARY.
           10 WB-NI-EH-NEUR-AB-EG         PIC S9(04)       BINARY.
           10 WB-NI-EH-NEUR-AB-GR         PIC S9(04)       BINARY.

        05 WB-VIVBB058.
           10 WB-ST-NR                    PIC S9(09)       BINARY.
           10 WB-PSEV-MONAT               PIC  X(10).
           10 WB-KNR-PROD-SPARTE          PIC S9(04)       BINARY.
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-KTY-PROD-SPARTE          PIC S9(04)       BINARY.
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 WB-PSEV-EH-GS-ZU-EG         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-EH-GS-ZU-GR         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-EH-GS-AB-EG         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-EH-GS-AB-GR         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-PV-GS-NT-EG         PIC S9(09)V9(02) COMP-3.
           10 WB-PSEV-PV-GS-NT-GR         PIC S9(09)V9(02) COMP-3.
           10 WB-PSEV-PV-FG-NT-EG         PIC S9(09)V9(02) COMP-3.
           10 WB-PSEV-PV-FG-NT-GR         PIC S9(09)V9(02) COMP-3.
           10 WB-PSEV-EH-FG-ZU-EG         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-EH-FG-ZU-GR         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-EH-FG-AB-EG         PIC S9(09)V9(01) COMP-3.
           10 WB-PSEV-EH-FG-AB-GR         PIC S9(09)V9(01) COMP-3.
           10 WB-KTY-UST-KZ               PIC S9(04)       BINARY.
           10 WB-KNR-UST-KZ               PIC S9(04)       BINARY.
           10 WB-PV-GS-NT-EG-USTANT       PIC S9(09)V9(02) COMP-3.
           10 WB-PV-GS-NT-GR-USTANT       PIC S9(09)V9(02) COMP-3.
           10 WB-PV-FG-NT-EG-USTANT       PIC S9(09)V9(02) COMP-3.
           10 WB-PV-FG-NT-GR-USTANT       PIC S9(09)V9(02) COMP-3.
           10 WB-PV-NEUR-NT-EG            PIC S9(09)V9(02) COMP-3.
           10 WB-PV-NEUR-NT-GR            PIC S9(09)V9(02) COMP-3.
           10 WB-PV-NEUR-NT-EG-UST        PIC S9(09)V9(02) COMP-3.
           10 WB-PV-NEUR-NT-GR-UST        PIC S9(09)V9(02) COMP-3.
           10 WB-EH-NEUR-ZU-EG            PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NEUR-ZU-GR            PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NEUR-AB-EG            PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NEUR-AB-GR            PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  32                    *
      *****************************************************************

