      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0128.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/WVVB0128.CPv  $
      *
      *   Rev 2.3   Dec 07 2018 14:31:16   t13ex18
      *AVB01456: Nachfolgeregelungen Zugriiffsmodul TBVB0128
      *
      *   Rev 2.2   Aug 16 2018 12:53:04   t13ex18
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.1   02 Aug 2018 16:19:46   t13ae13
      *AVB01456: Nachfolgeregelungen Tabellen NRB, NRP, NRPF
      *
      *   Rev 2.0   Dec 01 2017 12:59:38   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0128                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0128 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.12.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01456!04.12.17!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0128                           *
      *****************************************************************
 
        05 WB-NI-VIVB0128-TAB.
           10 WB-NI-VIVB0128-ELE   PIC S9(04)  BINARY OCCURS  45.
        05 WB-NI-VIVB0128          REDEFINES   WB-NI-VIVB0128-TAB.
           10 WB-NI-NRPF-ABGEB-NR         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-AUFNEHM-NR       PIC S9(04)       BINARY.
           10 WB-NI-KTY-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-KNR-NR-TYP            PIC S9(04)       BINARY.
           10 WB-NI-NRPF-DAT-VON          PIC S9(04)       BINARY.
           10 WB-NI-KTY-NRPF-TYP          PIC S9(04)       BINARY.
           10 WB-NI-KNR-NRPF-TYP          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ZAHLUNG1         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-BETRAG1          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ZAHLUNG2         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-BETRAG2          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ZAHLUNG3         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-BETRAG3          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ZAHLUNG4         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-BETRAG4          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ZAHLUNG5         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-BETRAG5          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-DAUER            PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ABSCHL-VON       PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ABSCHL-BIS       PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ABSCHL-ZAHLG     PIC S9(04)       BINARY.
           10 WB-NI-NRPF-P-ABGEB          PIC S9(04)       BINARY.
           10 WB-NI-NRPF-ERHANSPR         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-P-AUFNEHM        PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PSTUFE-1         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-RECH-1      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-TATS-1      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-FAKTOR-1         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-VERL-1      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PSTUFE-2         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-RECH-2      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-TATS-2      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-FAKTOR-2         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-VERL-2      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PSTUFE-3         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-RECH-3      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-TATS-3      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-FAKTOR-3         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-PDAT-VERL-3      PIC S9(04)       BINARY.
           10 WB-NI-NRPF-VB-EIGEN         PIC S9(04)       BINARY.
           10 WB-NI-NRPF-VB-GRUPPE        PIC S9(04)       BINARY.
           10 WB-NI-ZUGANG                PIC S9(04)       BINARY.
           10 WB-NI-VON                   PIC S9(04)       BINARY.
           10 WB-NI-BIS                   PIC S9(04)       BINARY.
 
        05 WB-VIVB0128.
           10 WB-NRPF-ABGEB-NR            PIC S9(09)       BINARY.
           10 WB-NRPF-AUFNEHM-NR          PIC S9(09)       BINARY.
           10 WB-KTY-NR-TYP               PIC S9(04)       BINARY.
           10 WB-KNR-NR-TYP               PIC S9(04)       BINARY.
           10 WB-NRPF-DAT-VON             PIC  X(10).
           10 WB-KTY-NRPF-TYP             PIC S9(04)       BINARY.
           10 WB-KNR-NRPF-TYP             PIC S9(04)       BINARY.
           10 WB-NRPF-LAST-UPDATE         PIC  X(26).
           10 WB-NRPF-ZAHLUNG1            PIC  X(10).
           10 WB-NRPF-BETRAG1             PIC S9(09)V9(02) COMP-3.
           10 WB-NRPF-ZAHLUNG2            PIC  X(10).
           10 WB-NRPF-BETRAG2             PIC S9(09)V9(02) COMP-3.
           10 WB-NRPF-ZAHLUNG3            PIC  X(10).
           10 WB-NRPF-BETRAG3             PIC S9(09)V9(02) COMP-3.
           10 WB-NRPF-ZAHLUNG4            PIC  X(10).
           10 WB-NRPF-BETRAG4             PIC S9(09)V9(02) COMP-3.
           10 WB-NRPF-ZAHLUNG5            PIC  X(10).
           10 WB-NRPF-BETRAG5             PIC S9(09)V9(02) COMP-3.
           10 WB-NRPF-DAUER               PIC S9(04)       BINARY.
           10 WB-NRPF-ABSCHL-VON          PIC  X(10).
           10 WB-NRPF-ABSCHL-BIS          PIC  X(10).
           10 WB-NRPF-ABSCHL-ZAHLG        PIC S9(09)V9(02) COMP-3.
           10 WB-NRPF-P-ABGEB             PIC S9(04)       BINARY.
           10 WB-NRPF-ERHANSPR            PIC  X(01).
           10 WB-NRPF-P-AUFNEHM           PIC S9(04)       BINARY.
           10 WB-NRPF-PSTUFE-1            PIC S9(04)       BINARY.
           10 WB-NRPF-PDAT-RECH-1         PIC  X(10).
           10 WB-NRPF-PDAT-TATS-1         PIC  X(10).
           10 WB-NRPF-FAKTOR-1            PIC S9(03)V9(02) COMP-3.
           10 WB-NRPF-PDAT-VERL-1         PIC  X(10).
           10 WB-NRPF-PSTUFE-2            PIC S9(04)       BINARY.
           10 WB-NRPF-PDAT-RECH-2         PIC  X(10).
           10 WB-NRPF-PDAT-TATS-2         PIC  X(10).
           10 WB-NRPF-FAKTOR-2            PIC S9(03)V9(02) COMP-3.
           10 WB-NRPF-PDAT-VERL-2         PIC  X(10).
           10 WB-NRPF-PSTUFE-3            PIC S9(04)       BINARY.
           10 WB-NRPF-PDAT-RECH-3         PIC  X(10).
           10 WB-NRPF-PDAT-TATS-3         PIC  X(10).
           10 WB-NRPF-FAKTOR-3            PIC S9(03)V9(02) COMP-3.
           10 WB-NRPF-PDAT-VERL-3         PIC  X(10).
           10 WB-NRPF-VB-EIGEN            PIC S9(09)       BINARY.
           10 WB-NRPF-VB-GRUPPE           PIC S9(09)       BINARY.
           10 WB-ZUGANG                   PIC  X(26).
           10 WB-VON                      PIC  X(26).
           10 WB-BIS                      PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  45                    *
      *****************************************************************
 
 
 

