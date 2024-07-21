      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0329.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0329.CPv  $
      *
      *   Rev 2.1   Nov 07 2019 11:47:02   u130213
      *AVB01580: Einführung neues ZUSTRUK-Verfahren
      *
      *   Rev 2.0   Nov 06 2019 12:32:44   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0329                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0329 (SWABZ   )                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U130213                                          *
      *    DATUM:    06.11.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580!06.11.19!2.1 ! NEU                       ! U130213   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0329                          *
      *****************************************************************
 
        05 WB-NI-VIVB0329-TAB.
           10 WB-NI-VIVB0329-ELE   PIC S9(04)  BINARY OCCURS   8.
        05 WB-NI-VIVB0329          REDEFINES   WB-NI-VIVB0329-TAB.
           10 WB-NI-SWABZ-BETR-NR         PIC S9(04)       BINARY.
           10 WB-NI-SWABZ-MONAT           PIC S9(04)       BINARY.
           10 WB-NI-KTY-UST-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KNR-UST-TYP           PIC S9(04)       BINARY.
           10 WB-NI-KTY-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-WB-TYP            PIC S9(04)       BINARY.
           10 WB-NI-SWABZ-EH-ABZUG        PIC S9(04)       BINARY.
           10 WB-NI-SWABZ-LAST-UPDATE     PIC S9(04)       BINARY.
 
        05 WB-VIVB0329.
           10 WB-SWABZ-BETR-NR            PIC S9(09)       BINARY.
           10 WB-SWABZ-MONAT              PIC  X(10).
           10 WB-KTY-UST-TYP              PIC S9(04)       BINARY.
           10 WB-KNR-UST-TYP              PIC S9(04)       BINARY.
           10 WB-KTY-WB-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-WB-TYP               PIC S9(04)       BINARY.
           10 WB-SWABZ-EH-ABZUG           PIC S9(09)V9(01) COMP-3.
           10 WB-SWABZ-LAST-UPDATE        PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
