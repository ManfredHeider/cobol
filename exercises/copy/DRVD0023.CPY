      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0023.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRVD0023.CPv  $
      *
      *   Rev 2.6   Dec 21 2020 17:47:48   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.5   Jun 03 2015 12:39:16   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - Änderung BVSA und
      * BVSP
      *
      *   Rev 2.4   Sep 30 2011 09:17:20   t13ae13
      *AVB01185: Maßnahmenpaket - Teil 1
      *
      *   Rev 2.3   Nov 05 2008 09:44:16   t13ae13
      *AVB00996: Allfinanz - Aufstiegsbedingungen für VB mit ÜV - VBI
      *
      *   Rev 2.2   16 Aug 2007 09:49:08   t13ex22
      *AVD00044: VBI Release 4.10 Beförderungsvorausschau
      *
      *   Rev 2.1   Sep 23 2005 11:18:10   t13ex22
      *AVD00008: Projekt VBI: SST Beförderungsprüfung
      *
      *   Rev 2.0   Mar 07 2005 15:42:26   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * SATZAUFBAU ZUR TABELLE BVSA - TBVB0047
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!16.02.15!2.5 ! Neue Felder für Allfinanz;! S. Walb  *
      *         !        !    ! Wegfall Felder ...-ALT    !          *
      * VLA-988 !12.10.20!2.6 ! WEITERE ATTRIBUTE         ! T13AE30  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
011008*03  DCL-TBTP0008.
011008 03  DCL-TBVB0047.
           05  WB-VS-ZEITRAUM                 PIC S9(4) COMP.
           05  WB-VB-NR                       PIC S9(9) COMP.
           05  WB-ERSTELL-DATUM               PIC X(10).
           05  WB-STUFE-AKTUELL               PIC S9(4) COMP.
           05  NI-STUFE-AKTUELL               PIC X(01) .
           05  WB-STUFE-NEU                   PIC S9(4) COMP.
           05  NI-STUFE-NEU                   PIC X(01) .
           05  WB-STUFE-BEFOERD-DAT           PIC X(10).
           05  NI-STUFE-BEFOERD-DAT           PIC X(01) .
           05  WB-IST-ZPKT-VON-NEU            PIC X(10).
           05  NI-IST-ZPKT-VON-NEU            PIC X(01) .
           05  WB-IST-ZPKT-BIS-NEU            PIC X(10).
           05  NI-IST-ZPKT-BIS-NEU            PIC X(01) .
           05  WB-ZIEL-ZPKT-VON-NEU           PIC X(10).
           05  NI-ZIEL-ZPKT-VON-NEU           PIC X(01) .
           05  WB-ZIEL-ZPKT-BIS-NEU           PIC X(10).
           05  NI-ZIEL-ZPKT-BIS-NEU           PIC X(01) .
           05  WB-IST-ZPKT-VON-VBP            PIC X(10).
           05  NI-IST-ZPKT-VON-VBP            PIC X(01) .
           05  WB-IST-ZPKT-BIS-VBP            PIC X(10).
           05  NI-IST-ZPKT-BIS-VBP            PIC X(01) .
           05  WB-ZIEL-ZPKT-VON-VBP           PIC X(10).
           05  NI-ZIEL-ZPKT-VON-VBP           PIC X(01) .
           05  WB-ZIEL-ZPKT-BIS-VBP           PIC X(10).
           05  NI-ZIEL-ZPKT-BIS-VBP           PIC X(01) .
160215*    05  WB-EH-EIG-PZTR-ALT             PIC S9(9)V9(1) COMP-3.
160215*    05  NI-EH-EIG-PZTR-ALT             PIC X(01) .
160215*    05  WB-EQF-EIG-ALT                 PIC S9(3)V9(2) COMP-3.
160215*    05  NI-EQF-EIG-ALT                 PIC X(01) .
160215*    05  WB-EH-GRU-PZTR-ALT             PIC S9(9)V9(1) COMP-3.
160215*    05  NI-EH-GRU-PZTR-ALT             PIC X(01) .
160215*    05  WB-B-EH-GRU-PZTR-ALT           PIC S9(9)V9(1) COMP-3.
160215*    05  NI-B-EH-GRU-PZTR-ALT           PIC X(01) .
160215*    05  WB-EQF-GRU-ALT                 PIC S9(3)V9(2) COMP-3.
160215*    05  NI-EQF-GRU-ALT                 PIC X(01) .
160215*    05  WB-EH-GES-PZTR-ALT             PIC S9(9)V9(1) COMP-3.
160215*    05  NI-EH-GES-PZTR-ALT             PIC X(01) .
160215*    05  WB-B-EH-GES-PZTR-ALT           PIC S9(9)V9(1) COMP-3.
160215*    05  NI-B-EH-GES-PZTR-ALT           PIC X(01) .
160215*    05  WB-EQF-GES-ALT                 PIC S9(3)V9(2) COMP-3.
160215*    05  NI-EQF-GES-ALT                 PIC X(01) .
160215*    05  WB-EH-EG-MX-PZTR-ALT           PIC S9(9)V9(1) COMP-3.
160215*    05  NI-EH-EG-MX-PZTR-ALT           PIC X(01) .
160215*    05  WB-EH-GES-VERBL-ALT            PIC S9(9)V9(1) COMP-3.
160215*    05  NI-EH-GES-VERBL-ALT            PIC X(01) .
160215*    05  WB-ANZ-PARTNER-ALT             PIC S9(4) COMP.
160215*    05  NI-ANZ-PARTNER-ALT             PIC X(01) .
           05  WB-EH-EIG-PZTR-NEU             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-EIG-PZTR-NEU             PIC X(01) .
           05  WB-EQF-EIG-NEU                 PIC S9(3)V9(2) COMP-3.
           05  NI-EQF-EIG-NEU                 PIC X(01) .
           05  WB-EH-GRU-PZTR-NEU             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-GRU-PZTR-NEU             PIC X(01) .
           05  WB-B-EH-GRU-PZTR-NEU           PIC S9(9)V9(1) COMP-3.
           05  NI-B-EH-GRU-PZTR-NEU           PIC X(01) .
           05  WB-EQF-GRU-NEU                 PIC S9(3)V9(2) COMP-3.
           05  NI-EQF-GRU-NEU                 PIC X(01) .
           05  WB-EH-GES-PZTR-NEU             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-GES-PZTR-NEU             PIC X(01) .
           05  WB-B-EH-GES-PZTR-NEU           PIC S9(9)V9(1) COMP-3.
           05  NI-B-EH-GES-PZTR-NEU           PIC X(01) .
           05  WB-EQF-GES-NEU                 PIC S9(3)V9(2) COMP-3.
           05  NI-EQF-GES-NEU                 PIC X(01) .
           05  WB-EH-EG-MX-PZTR-NEU           PIC S9(9)V9(1) COMP-3.
           05  NI-EH-EG-MX-PZTR-NEU           PIC X(01) .
           05  WB-EH-GES-VERBL-NEU            PIC S9(9)V9(1) COMP-3.
           05  NI-EH-GES-VERBL-NEU            PIC X(01) .
           05  WB-ANZ-PARTNER-NEU             PIC S9(4) COMP.
           05  NI-ANZ-PARTNER-NEU             PIC X(01) .
           05  WB-HPT-ANZ-PARTN-NEU           PIC S9(4) COMP.
           05  NI-HPT-ANZ-PARTN-NEU           PIC X(01) .
           05  WB-ALP-ANZ-VM-OD-NEU           PIC S9(4) COMP.
           05  NI-ALP-ANZ-VM-OD-NEU           PIC X(01) .
           05  WB-VBP-AKTUELL                 PIC S9(4) COMP.
           05  NI-VBP-AKTUELL                 PIC X(01) .
           05  WB-VBP-VORHER                  PIC S9(4) COMP.
           05  NI-VBP-VORHER                  PIC X(01) .
           05  WB-VBP-BEF-DAT                 PIC X(10).
           05  NI-VBP-BEF-DAT                 PIC X(01) .
180805     05  WB-BEF-DAT-EH-EIG-VBP          PIC X(10).
180805     05  NI-BEF-DAT-EH-EIG-VBP          PIC X(01) .
           05  WB-VBP-NEU                     PIC S9(4) COMP.
           05  NI-VBP-NEU                     PIC X(01) .
           05  WB-EH-EIG-PZTR-VBP             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-EIG-PZTR-VBP             PIC X(01) .
           05  WB-WEIT-EH-EIG-VBP             PIC S9(9)V9(1) COMP-3.
           05  NI-WEIT-EH-EIG-VBP             PIC X(01) .
           05  WB-EQF-EIG-VBP                 PIC S9(3)V9(2) COMP-3.
           05  NI-EQF-EIG-VBP                 PIC X(01) .
010807     05  WB-HPT-ANZ-PARTN-ERS           PIC S9(4) COMP.
010807     05  NI-HPT-ANZ-PARTN-ERS           PIC X(01) .
011008     05  WB-ANZ-PART-AB-RGS             PIC S9(4) COMP.
011008     05  NI-ANZ-PART-AB-RGS             PIC X(01) .
011008     05  WB-EH-GS-NT-GES-NEU            PIC S9(9)V9(1) COMP-3.
011008     05  NI-EH-GS-NT-GES-NEU            PIC X(01) .
011008     05  WB-VBP-EH-ERSTM-KZ             PIC X(01).
011008     05  NI-VBP-EH-ERSTM-KZ             PIC X(01).
210911     05  WB-EH-EIG-PZTR-11              PIC S9(9)V9(1) COMP-3.
210911     05  NI-EH-EIG-PZTR-11              PIC X(01) .
210911     05  WB-EQF-EIG-11                  PIC S9(3)V9(2) COMP-3.
210911     05  NI-EQF-EIG-11                  PIC X(01) .
210911     05  WB-EH-GES-PZTR-11              PIC S9(9)V9(1) COMP-3.
210911     05  NI-EH-GES-PZTR-11              PIC X(01) .
210911     05  WB-B-EH-GES-PZTR-11            PIC S9(9)V9(1) COMP-3.
210911     05  NI-B-EH-GES-PZTR-11            PIC X(01) .
210911     05  WB-EQF-GES-11                  PIC S9(3)V9(2) COMP-3.
210911     05  NI-EQF-GES-11                  PIC X(01) .
210911     05  WB-EH-EG-MX-PZTR-11            PIC S9(9)V9(1) COMP-3.
210911     05  NI-EH-EG-MX-PZTR-11            PIC X(01) .
210911     05  WB-EH-GES-VERBL-11             PIC S9(9)V9(1) COMP-3.
210911     05  NI-EH-GES-VERBL-11             PIC X(01) .
160215     05  WB-EH-NEU-EIG-NP-PZ            PIC S9(9)V9(1) COMP-3.
160215     05  NI-EH-NEU-EIG-NP-PZ            PIC X(01) .
160215     05  WB-B-EH-NEU-EIG-NP-PZ          PIC S9(9)V9(1) COMP-3.
160215     05  NI-B-EH-NEU-EIG-NP-PZ          PIC X(01) .
121020     05  WB-EH-EIG-PZTR-21              PIC S9(9)V9(1) COMP-3.
121020     05  NI-EH-EIG-PZTR-21              PIC X(01) .
121020     05  WB-EQF-EIG-21                  PIC S9(3)V9(2) COMP-3.
121020     05  NI-EQF-EIG-21                  PIC X(01) .
121020     05  WB-EH-GES-PZTR-21              PIC S9(9)V9(1) COMP-3.
121020     05  NI-EH-GES-PZTR-21              PIC X(01) .
121020     05  WB-B-EH-GES-PZTR-21            PIC S9(9)V9(1) COMP-3.
121020     05  NI-B-EH-GES-PZTR-21            PIC X(01) .
121020     05  WB-EQF-GES-21                  PIC S9(3)V9(2) COMP-3.
121020     05  NI-EQF-GES-21                  PIC X(01) .
121020     05  WB-EH-EG-MX-PZTR-21            PIC S9(9)V9(1) COMP-3.
121020     05  NI-EH-EG-MX-PZTR-21            PIC X(01) .
121020     05  WB-EH-GES-VERBL-21             PIC S9(9)V9(1) COMP-3.
121020     05  NI-EH-GES-VERBL-21             PIC X(01) .
121020     05  WB-EH-NEU-EIG-NP-21            PIC S9(9)V9(1) COMP-3.
121020     05  NI-EH-NEU-EIG-NP-21            PIC X(01) .
121020     05  WB-B-EH-NEU-EIG-NP-21          PIC S9(9)V9(1) COMP-3.
121020     05  NI-B-EH-NEU-EIG-NP-21          PIC X(01) .



