      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.01.2021
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVBA121.CPv  $
      *
      *   Rev 2.1   Jan 20 2021 14:55:46   t13ae56
      *AVB01644: PGM - Tabelle BEFINFO erweitern
      *
      *   Rev 2.0   Jan 20 2021 10:52:18   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA121                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVBA121 (BEFINFO)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.01.2021                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-1004!20.01.21!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA121                          *
      *****************************************************************
 
        05 WB-NI-VIVBA121-TAB.
           10 WB-NI-VIVBA121-ELE   PIC S9(04)  BINARY OCCURS  16.
        05 WB-NI-VIVBA121          REDEFINES   WB-NI-VIVBA121-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-MS-NR                 PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-DAT-VON       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-LAST-UPD      PIC S9(04)       BINARY.
           10 WB-NI-KTY-BEFBED-TYP        PIC S9(04)       BINARY.
           10 WB-NI-KNR-BEFBED-TYP        PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-E-PR-ALT      PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-S-PR-ALT      PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-E-PR-07       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-S-PR-07       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-E-PR-11       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-S-PR-11       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-E-PR-15       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-S-PR-15       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-E-PR-21       PIC S9(04)       BINARY.
           10 WB-NI-BEFINFO-S-PR-21       PIC S9(04)       BINARY.
 
        05 WB-VIVBA121.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-MS-NR                    PIC S9(04)       BINARY.
           10 WB-BEFINFO-DAT-VON          PIC  X(10).
           10 WB-BEFINFO-LAST-UPD         PIC  X(26).
           10 WB-KTY-BEFBED-TYP           PIC S9(04)       BINARY.
           10 WB-KNR-BEFBED-TYP           PIC S9(04)       BINARY.
           10 WB-BEFINFO-E-PR-ALT         PIC  X(01).
           10 WB-BEFINFO-S-PR-ALT         PIC  X(01).
           10 WB-BEFINFO-E-PR-07          PIC  X(01).
           10 WB-BEFINFO-S-PR-07          PIC  X(01).
           10 WB-BEFINFO-E-PR-11          PIC  X(01).
           10 WB-BEFINFO-S-PR-11          PIC  X(01).
           10 WB-BEFINFO-E-PR-15          PIC  X(01).
           10 WB-BEFINFO-S-PR-15          PIC  X(01).
           10 WB-BEFINFO-E-PR-21          PIC  X(01).
           10 WB-BEFINFO-S-PR-21          PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  16                    *
      *****************************************************************

