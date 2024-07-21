      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1291.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1291.CPv  $
      *
      *   Rev 2.1   Nov 19 2020 07:54:20   t13ae56
      *AVB01644: PGM - KS2021: Anpassung Beförderungsdialog VB22
      *
      *   Rev 2.0   Sep 29 2020 14:49:46   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1291                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0047                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    29.09.2020                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VLA-1004!29.09.20!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1291 - ERMITTELN SATZ AUS TABELLE BVSA (ZR)   *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1291.
          05 PI-DRVB1291-DATEN.
             10 PI-DRVB1291-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 PI-DRVB1291-VB-NR              PIC S9(09)       BINARY.
 
       01 PO-DRVB1291.
         03 PO-DRVB1291-RC                     PIC  X(03).
             88 PO-DRVB1291-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1291-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1291-DATEN.
          05 PO-DRVB1291-NI-VIVB0047.
             10 NI-DRVB1291-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 NI-DRVB1291-VB-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1291-ERSTELL-DATUM      PIC S9(04)       BINARY.
             10 NI-DRVB1291-STUFE-AKTUELL      PIC S9(04)       BINARY.
             10 NI-DRVB1291-STUFE-NEU          PIC S9(04)       BINARY.
             10 NI-DRVB1291-STUFE-BEF-DAT      PIC S9(04)       BINARY.
             10 NI-DRVB1291-IST-ZPKT-VON-NEU   PIC S9(04)       BINARY.
             10 NI-DRVB1291-IST-ZPKT-BIS-NEU   PIC S9(04)       BINARY.
             10 NI-DRVB1291-ZIEL-ZPKT-VON-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-ZIEL-ZPKT-BIS-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-IST-ZPKT-VON-VBP   PIC S9(04)       BINARY.
             10 NI-DRVB1291-IST-ZPKT-BIS-VBP   PIC S9(04)       BINARY.
             10 NI-DRVB1291-ZIEL-ZPKT-VON-VBP  PIC S9(04)       BINARY.
             10 NI-DRVB1291-ZIEL-ZPKT-BIS-VBP  PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-EIG-PZTR-NEU    PIC S9(04)       BINARY.
             10 NI-DRVB1291-EQF-EIG-NEU        PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-GRU-PZTR-NEU    PIC S9(04)       BINARY.
             10 NI-DRVB1291-B-EH-GRU-PZTR-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-EQF-GRU-NEU        PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-GES-PZTR-NEU    PIC S9(04)       BINARY.
             10 NI-DRVB1291-B-EH-GES-PZTR-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-EQF-GES-NEU        PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-EG-MX-PZTR-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-GES-VERBL-NEU   PIC S9(04)       BINARY.
             10 NI-DRVB1291-ANZ-PARTNER-NEU    PIC S9(04)       BINARY.
             10 NI-DRVB1291-HPT-ANZ-PARTN-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-ALP-ANZ-VM-OD-NEU  PIC S9(04)       BINARY.
             10 NI-DRVB1291-VBP-AKTUELL        PIC S9(04)       BINARY.
             10 NI-DRVB1291-VBP-VORHER         PIC S9(04)       BINARY.
             10 NI-DRVB1291-VBP-BEF-DAT        PIC S9(04)       BINARY.
             10 NI-DRVB1291-BEF-DAT-EH-EG-VBP  PIC S9(04)       BINARY.
             10 NI-DRVB1291-VBP-NEU            PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-EIG-PZTR-VBP    PIC S9(04)       BINARY.
             10 NI-DRVB1291-WEIT-EH-EIG-VBP    PIC S9(04)       BINARY.
             10 NI-DRVB1291-EQF-EIG-VBP        PIC S9(04)       BINARY.
             10 NI-DRVB1291-ANZ-HB-PARTN-ERS   PIC S9(04)       BINARY.
             10 NI-DRVB1291-ANZPART-AB-RGS     PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-GS-NT-GES-NEU   PIC S9(04)       BINARY.
             10 NI-DRVB1291-VBP-EH-ERSTM-KZ    PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-EIG-PZTR-11     PIC S9(04)       BINARY.
             10 NI-DRVB1291-EQF-EIG-11         PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-GES-PZTR-11     PIC S9(04)       BINARY.
             10 NI-DRVB1291-B-EH-GES-PZTR-11   PIC S9(04)       BINARY.
             10 NI-DRVB1291-EQF-GES-11         PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-EG-MX-PZTR-11   PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-GES-VERBL-11    PIC S9(04)       BINARY.
             10 NI-DRVB1291-EH-NEU-EIG-NP      PIC S9(04)       BINARY.
             10 NI-DRVB1291-B-EH-NEU-EIG-NP    PIC S9(04)       BINARY.
          05 PO-DRVB1291-DCL-VIVB0047.
             10 PO-DRVB1291-VS-ZEITRAUM        PIC S9(04)       BINARY.
             10 PO-DRVB1291-VB-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1291-ERSTELL-DATUM      PIC  X(10).
             10 PO-DRVB1291-STUFE-AKTUELL      PIC S9(04)       BINARY.
             10 PO-DRVB1291-STUFE-NEU          PIC S9(04)       BINARY.
             10 PO-DRVB1291-STUFE-BEF-DAT      PIC  X(10).
             10 PO-DRVB1291-IST-ZPKT-VON-NEU   PIC  X(10).
             10 PO-DRVB1291-IST-ZPKT-BIS-NEU   PIC  X(10).
             10 PO-DRVB1291-ZIEL-ZPKT-VON-NEU  PIC  X(10).
             10 PO-DRVB1291-ZIEL-ZPKT-BIS-NEU  PIC  X(10).
             10 PO-DRVB1291-IST-ZPKT-VON-VBP   PIC  X(10).
             10 PO-DRVB1291-IST-ZPKT-BIS-VBP   PIC  X(10).
             10 PO-DRVB1291-ZIEL-ZPKT-VON-VBP  PIC  X(10).
             10 PO-DRVB1291-ZIEL-ZPKT-BIS-VBP  PIC  X(10).
             10 PO-DRVB1291-EH-EIG-PZTR-NEU    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EQF-EIG-NEU        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1291-EH-GRU-PZTR-NEU    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-B-EH-GRU-PZTR-NEU  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EQF-GRU-NEU        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1291-EH-GES-PZTR-NEU    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-B-EH-GES-PZTR-NEU  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EQF-GES-NEU        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1291-EH-EG-MX-PZTR-NEU  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EH-GES-VERBL-NEU   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-ANZ-PARTNER-NEU    PIC S9(04)       BINARY.
             10 PO-DRVB1291-HPT-ANZ-PARTN-NEU  PIC S9(04)       BINARY.
             10 PO-DRVB1291-ALP-ANZ-VM-OD-NEU  PIC S9(04)       BINARY.
             10 PO-DRVB1291-VBP-AKTUELL        PIC S9(04)       BINARY.
             10 PO-DRVB1291-VBP-VORHER         PIC S9(04)       BINARY.
             10 PO-DRVB1291-VBP-BEF-DAT        PIC  X(10).
             10 PO-DRVB1291-BEF-DAT-EH-EG-VBP  PIC  X(10).
             10 PO-DRVB1291-VBP-NEU            PIC S9(04)       BINARY.
             10 PO-DRVB1291-EH-EIG-PZTR-VBP    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-WEIT-EH-EIG-VBP    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EQF-EIG-VBP        PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1291-ANZ-HB-PARTN-ERS   PIC S9(04)       BINARY.
             10 PO-DRVB1291-ANZPART-AB-RGS     PIC S9(04)       BINARY.
             10 PO-DRVB1291-EH-GS-NT-GES-NEU   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-VBP-EH-ERSTM-KZ    PIC  X(01).
             10 PO-DRVB1291-EH-EIG-PZTR-11     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EQF-EIG-11         PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1291-EH-GES-PZTR-11     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-B-EH-GES-PZTR-11   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EQF-GES-11         PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1291-EH-EG-MX-PZTR-11   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EH-GES-VERBL-11    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-EH-NEU-EIG-NP      PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1291-B-EH-NEU-EIG-NP    PIC S9(09)V9(01) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1291   --------------------------*
      *---------------------------------------------------------------*
