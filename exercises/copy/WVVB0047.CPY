      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0047.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0047.CPv  $
      *
      *   Rev 2.1   Jun 25 2015 09:21:54   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - Änderung BVSA und
      * BVSP
      *
      *   Rev 2.0   Jun 18 2015 12:47:36   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0047                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0047 (BVSA    )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    18.06.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01332!18.06.15!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0047                           *
      *****************************************************************

        05 WB-NI-VIVB0047-TAB.
           10 WB-NI-VIVB0047-ELE   PIC S9(04)  BINARY OCCURS  48.
        05 WB-NI-VIVB0047          REDEFINES   WB-NI-VIVB0047-TAB.
           10 WB-NI-VS-ZEITRAUM           PIC S9(04)       BINARY.
           10 WB-NI-VB-NR                 PIC S9(04)       BINARY.
           10 WB-NI-ERSTELL-DATUM         PIC S9(04)       BINARY.
           10 WB-NI-STUFE-AKTUELL         PIC S9(04)       BINARY.
           10 WB-NI-STUFE-NEU             PIC S9(04)       BINARY.
           10 WB-NI-STUFE-BEF-DAT         PIC S9(04)       BINARY.
           10 WB-NI-IST-ZPKT-VON-NEU      PIC S9(04)       BINARY.
           10 WB-NI-IST-ZPKT-BIS-NEU      PIC S9(04)       BINARY.
           10 WB-NI-ZIEL-ZPKT-VON-NEU     PIC S9(04)       BINARY.
           10 WB-NI-ZIEL-ZPKT-BIS-NEU     PIC S9(04)       BINARY.
           10 WB-NI-IST-ZPKT-VON-VBP      PIC S9(04)       BINARY.
           10 WB-NI-IST-ZPKT-BIS-VBP      PIC S9(04)       BINARY.
           10 WB-NI-ZIEL-ZPKT-VON-VBP     PIC S9(04)       BINARY.
           10 WB-NI-ZIEL-ZPKT-BIS-VBP     PIC S9(04)       BINARY.
           10 WB-NI-EH-EIG-PZTR-NEU       PIC S9(04)       BINARY.
           10 WB-NI-EQF-EIG-NEU           PIC S9(04)       BINARY.
           10 WB-NI-EH-GRU-PZTR-NEU       PIC S9(04)       BINARY.
           10 WB-NI-B-EH-GRU-PZTR-NEU     PIC S9(04)       BINARY.
           10 WB-NI-EQF-GRU-NEU           PIC S9(04)       BINARY.
           10 WB-NI-EH-GES-PZTR-NEU       PIC S9(04)       BINARY.
           10 WB-NI-B-EH-GES-PZTR-NEU     PIC S9(04)       BINARY.
           10 WB-NI-EQF-GES-NEU           PIC S9(04)       BINARY.
           10 WB-NI-EH-EG-MX-PZTR-NEU     PIC S9(04)       BINARY.
           10 WB-NI-EH-GES-VERBL-NEU      PIC S9(04)       BINARY.
           10 WB-NI-ANZ-PARTNER-NEU       PIC S9(04)       BINARY.
           10 WB-NI-HPT-ANZ-PARTN-NEU     PIC S9(04)       BINARY.
           10 WB-NI-ALP-ANZ-VM-OD-NEU     PIC S9(04)       BINARY.
           10 WB-NI-VBP-AKTUELL           PIC S9(04)       BINARY.
           10 WB-NI-VBP-VORHER            PIC S9(04)       BINARY.
           10 WB-NI-VBP-BEF-DAT           PIC S9(04)       BINARY.
           10 WB-NI-BEF-DAT-EH-EG-VBP     PIC S9(04)       BINARY.
           10 WB-NI-VBP-NEU               PIC S9(04)       BINARY.
           10 WB-NI-EH-EIG-PZTR-VBP       PIC S9(04)       BINARY.
           10 WB-NI-WEIT-EH-EIG-VBP       PIC S9(04)       BINARY.
           10 WB-NI-EQF-EIG-VBP           PIC S9(04)       BINARY.
           10 WB-NI-ANZ-HB-PARTN-ERS      PIC S9(04)       BINARY.
           10 WB-NI-ANZPART-AB-RGS        PIC S9(04)       BINARY.
           10 WB-NI-EH-GS-NT-GES-NEU      PIC S9(04)       BINARY.
           10 WB-NI-VBP-EH-ERSTM-KZ       PIC S9(04)       BINARY.
           10 WB-NI-EH-EIG-PZTR-11        PIC S9(04)       BINARY.
           10 WB-NI-EQF-EIG-11            PIC S9(04)       BINARY.
           10 WB-NI-EH-GES-PZTR-11        PIC S9(04)       BINARY.
           10 WB-NI-B-EH-GES-PZTR-11      PIC S9(04)       BINARY.
           10 WB-NI-EQF-GES-11            PIC S9(04)       BINARY.
           10 WB-NI-EH-EG-MX-PZTR-11      PIC S9(04)       BINARY.
           10 WB-NI-EH-GES-VERBL-11       PIC S9(04)       BINARY.
           10 WB-NI-EH-NEU-EIG-NP         PIC S9(04)       BINARY.
           10 WB-NI-B-EH-NEU-EIG-NP       PIC S9(04)       BINARY.

        05 WB-VIVB0047.
           10 WB-VS-ZEITRAUM              PIC S9(04)       BINARY.
           10 WB-VB-NR                    PIC S9(09)       BINARY.
           10 WB-ERSTELL-DATUM            PIC  X(10).
           10 WB-STUFE-AKTUELL            PIC S9(04)       BINARY.
           10 WB-STUFE-NEU                PIC S9(04)       BINARY.
           10 WB-STUFE-BEF-DAT            PIC  X(10).
           10 WB-IST-ZPKT-VON-NEU         PIC  X(10).
           10 WB-IST-ZPKT-BIS-NEU         PIC  X(10).
           10 WB-ZIEL-ZPKT-VON-NEU        PIC  X(10).
           10 WB-ZIEL-ZPKT-BIS-NEU        PIC  X(10).
           10 WB-IST-ZPKT-VON-VBP         PIC  X(10).
           10 WB-IST-ZPKT-BIS-VBP         PIC  X(10).
           10 WB-ZIEL-ZPKT-VON-VBP        PIC  X(10).
           10 WB-ZIEL-ZPKT-BIS-VBP        PIC  X(10).
           10 WB-EH-EIG-PZTR-NEU          PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-EIG-NEU              PIC S9(03)V9(02) COMP-3.
           10 WB-EH-GRU-PZTR-NEU          PIC S9(09)V9(01) COMP-3.
           10 WB-B-EH-GRU-PZTR-NEU        PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-GRU-NEU              PIC S9(03)V9(02) COMP-3.
           10 WB-EH-GES-PZTR-NEU          PIC S9(09)V9(01) COMP-3.
           10 WB-B-EH-GES-PZTR-NEU        PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-GES-NEU              PIC S9(03)V9(02) COMP-3.
           10 WB-EH-EG-MX-PZTR-NEU        PIC S9(09)V9(01) COMP-3.
           10 WB-EH-GES-VERBL-NEU         PIC S9(09)V9(01) COMP-3.
           10 WB-ANZ-PARTNER-NEU          PIC S9(04)       BINARY.
           10 WB-HPT-ANZ-PARTN-NEU        PIC S9(04)       BINARY.
           10 WB-ALP-ANZ-VM-OD-NEU        PIC S9(04)       BINARY.
           10 WB-VBP-AKTUELL              PIC S9(04)       BINARY.
           10 WB-VBP-VORHER               PIC S9(04)       BINARY.
           10 WB-VBP-BEF-DAT              PIC  X(10).
           10 WB-BEF-DAT-EH-EG-VBP        PIC  X(10).
           10 WB-VBP-NEU                  PIC S9(04)       BINARY.
           10 WB-EH-EIG-PZTR-VBP          PIC S9(09)V9(01) COMP-3.
           10 WB-WEIT-EH-EIG-VBP          PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-EIG-VBP              PIC S9(03)V9(02) COMP-3.
           10 WB-ANZ-HB-PARTN-ERS         PIC S9(04)       BINARY.
           10 WB-ANZPART-AB-RGS           PIC S9(04)       BINARY.
           10 WB-EH-GS-NT-GES-NEU         PIC S9(09)V9(01) COMP-3.
           10 WB-VBP-EH-ERSTM-KZ          PIC  X(01).
           10 WB-EH-EIG-PZTR-11           PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-EIG-11               PIC S9(03)V9(02) COMP-3.
           10 WB-EH-GES-PZTR-11           PIC S9(09)V9(01) COMP-3.
           10 WB-B-EH-GES-PZTR-11         PIC S9(09)V9(01) COMP-3.
           10 WB-EQF-GES-11               PIC S9(03)V9(02) COMP-3.
           10 WB-EH-EG-MX-PZTR-11         PIC S9(09)V9(01) COMP-3.
           10 WB-EH-GES-VERBL-11          PIC S9(09)V9(01) COMP-3.
           10 WB-EH-NEU-EIG-NP            PIC S9(09)V9(01) COMP-3.
           10 WB-B-EH-NEU-EIG-NP          PIC S9(09)V9(01) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  48                    *
      *****************************************************************

