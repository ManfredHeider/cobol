      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.02.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0127.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0127.CPv  $
      *
      *   Rev 2.1   31 Mar 2020 11:46:16   t13ae56
      *AVB01574: PGM - Nachfolgeregelung Praxis (VB61)
      *
      *   Rev 2.0   Feb 04 2020 14:33:26   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0127                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FUER VIVB0127 (NRP)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.02.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-670 !04.02.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0127                          *
      *****************************************************************
 
        05 WB-NI-VIVB0127-TAB.
           10 WB-NI-VIVB0127-ELE   PIC S9(04)  BINARY OCCURS  20.
        05 WB-NI-VIVB0127          REDEFINES   WB-NI-VIVB0127-TAB.
           10 WB-NI-NRP-ABGEB-NR          PIC S9(04)       BINARY.
           10 WB-NI-NRP-AUFNEHM-NR        PIC S9(04)       BINARY.
           10 WB-NI-KTY-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-NRP-DAT-VON           PIC S9(04)       BINARY.
           10 WB-NI-NRP-LAST-UPDATE       PIC S9(04)       BINARY.
           10 WB-NI-NRP-ZAHLUNG1          PIC S9(04)       BINARY.
           10 WB-NI-NRP-BETRAG1           PIC S9(04)       BINARY.
           10 WB-NI-NRP-ZAHLUNG2          PIC S9(04)       BINARY.
           10 WB-NI-NRP-BETRAG2           PIC S9(04)       BINARY.
           10 WB-NI-NRP-ZAHLUNG3          PIC S9(04)       BINARY.
           10 WB-NI-NRP-BETRAG3           PIC S9(04)       BINARY.
           10 WB-NI-NRP-ZAHLUNG4          PIC S9(04)       BINARY.
           10 WB-NI-NRP-BETRAG4           PIC S9(04)       BINARY.
           10 WB-NI-NRP-ZAHLUNG5          PIC S9(04)       BINARY.
           10 WB-NI-NRP-BETRAG5           PIC S9(04)       BINARY.
           10 WB-NI-NRP-DAUER             PIC S9(04)       BINARY.
           10 WB-NI-NRP-ABSCHL-VON        PIC S9(04)       BINARY.
           10 WB-NI-NRP-ABSCHL-BIS        PIC S9(04)       BINARY.
           10 WB-NI-NRP-ABSCHL-ZAHLG      PIC S9(04)       BINARY.
 
        05 WB-VIVB0127.
           10 WB-NRP-ABGEB-NR             PIC S9(09)       BINARY.
           10 WB-NRP-AUFNEHM-NR           PIC S9(09)       BINARY.
           10 WB-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 WB-NRP-DAT-VON              PIC  X(10).
           10 WB-NRP-LAST-UPDATE          PIC  X(26).
           10 WB-NRP-ZAHLUNG1             PIC  X(10).
           10 WB-NRP-BETRAG1              PIC S9(09)V9(02) COMP-3.
           10 WB-NRP-ZAHLUNG2             PIC  X(10).
           10 WB-NRP-BETRAG2              PIC S9(09)V9(02) COMP-3.
           10 WB-NRP-ZAHLUNG3             PIC  X(10).
           10 WB-NRP-BETRAG3              PIC S9(09)V9(02) COMP-3.
           10 WB-NRP-ZAHLUNG4             PIC  X(10).
           10 WB-NRP-BETRAG4              PIC S9(09)V9(02) COMP-3.
           10 WB-NRP-ZAHLUNG5             PIC  X(10).
           10 WB-NRP-BETRAG5              PIC S9(09)V9(02) COMP-3.
           10 WB-NRP-DAUER                PIC S9(04)       BINARY.
           10 WB-NRP-ABSCHL-VON           PIC  X(10).
           10 WB-NRP-ABSCHL-BIS           PIC  X(10).
           10 WB-NRP-ABSCHL-ZAHLG         PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  20                    *
      *****************************************************************
