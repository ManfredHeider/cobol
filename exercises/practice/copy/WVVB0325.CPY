      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  10.01.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0325.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0325.CPv  $
      *
      *   Rev 2.1   Jan 17 2018 14:15:02   t13ae30
      *AAS01225: Programme - TWI
      *
      *   Rev 2.0   Jan 10 2018 14:16:38   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0325                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0325 (DETAILST)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    10.01.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS01225!10.01.18!2.0 ! NEU                       ! TH.WILLIG *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0325                           *
      *****************************************************************

        05 WB-NI-VIVB0325-TAB.
           10 WB-NI-VIVB0325-ELE   PIC S9(04)  BINARY OCCURS   9.
        05 WB-NI-VIVB0325          REDEFINES   WB-NI-VIVB0325-TAB.
           10 WB-NI-DETAILST-BETR         PIC S9(04)       BINARY.
           10 WB-NI-DETAILST-UNTST        PIC S9(04)       BINARY.
           10 WB-NI-DETAILST-JAHR         PIC S9(04)       BINARY.
           10 WB-NI-DETAILST-MONAT        PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-DETAILST-LAST-UP      PIC S9(04)       BINARY.
           10 WB-NI-DETAILST-EH           PIC S9(04)       BINARY.
           10 WB-NI-DETAILST-BIS          PIC S9(04)       BINARY.

        05 WB-VIVB0325.
           10 WB-DETAILST-BETR            PIC S9(09)       BINARY.
           10 WB-DETAILST-UNTST           PIC S9(09)       BINARY.
           10 WB-DETAILST-JAHR            PIC S9(04)       BINARY.
           10 WB-DETAILST-MONAT           PIC  X(10).
           10 WB-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 WB-DETAILST-LAST-UP         PIC  X(26).
           10 WB-DETAILST-EH              PIC S9(09)V9(01) COMP-3.
           10 WB-DETAILST-BIS             PIC  X(10).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   9                    *
      *****************************************************************

