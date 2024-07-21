      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0024.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRVD0024.CPv  $
      *
      *   Rev 2.9   Dec 21 2020 17:47:54   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.8   Aug 26 2016 12:31:52   t13ae13
      *AVB01409: Berechnung Einheitenabzug Betreuerwechsel
      *
      *   Rev 2.7   Jun 03 2015 12:39:20   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - Änderung BVSA und
      * BVSP
      *
      *   Rev 2.6   Sep 30 2011 09:17:28   t13ae13
      *AVB01185: Maßnahmenpaket - Teil 1
      *
      *   Rev 2.5   Nov 05 2008 09:44:20   t13ae13
      *AVB00996: Allfinanz - Aufstiegsbedingungen für VB mit ÜV - VBI
      *
      *   Rev 2.4   Jul 30 2008 12:06:20   t13ex22
      *
      *   Rev 2.3   Jul 10 2008 07:35:16   t13ex22
      *
      *   Rev 2.2   26 May 2006 13:39:12   t13ex22
      *AVD00030: Host-Änderungen für VBI Release 3.20
      *
      *   Rev 2.1   Sep 23 2005 11:18:18   t13ex22
      *AVD00008: Projekt VBI: SST Beförderungsprüfung
      *
      *   Rev 2.0   Mar 07 2005 15:42:40   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * SATZAUFBAU ZUR TABELLE BVSP - TBVB0053
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!16.02.15!2.7 ! Neue Felder für Allfinanz;! S. Walb  *
      *         !        !    ! Wegfall Felder ...-ALT    !          *
      * AVB01409!14.07.16!2.8 ! Neues Feld für BW         ! S. Walb  *
      * VLA-988 !12.10.20!2.9 ! WEITERE ATTRIBUTE         ! T13AE30  *
      * VLA-3566!20.10.21! WEITERE ATTRIBUTE              ! T13AE30  *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       03  DCL-TBVB0053.
           05  WB-VS-ZEITRAUM                 PIC S9(4) COMP.
           05  WB-VB-NR                       PIC S9(9) COMP.
           05  WB-VB-BEZUGS-NR                PIC S9(9) COMP.
           05  WB-BETR-DAT-VON                PIC X(10).
           05  WB-BETR-DAT-BIS                PIC X(10).
           05  NI-BETR-DAT-BIS                PIC X(01) .
           05  WB-ERSTELL-DATUM               PIC X(10).
           05  WB-EH-GES-PZTR-NEU             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-GES-PZTR-NEU             PIC X(01) .
           05  WB-KTY-EREIGNIS-ANF            PIC S9(04) BINARY.
           05  NI-KTY-EREIGNIS-ANF            PIC X(01) .
           05  WB-KNR-EREIGNIS-ANF            PIC S9(04) BINARY.
           05  NI-KNR-EREIGNIS-ANF            PIC X(01) .
           05  WB-KTY-EREIGNIS-ENDE           PIC S9(04) BINARY.
           05  NI-KTY-EREIGNIS-ENDE           PIC X(01) .
           05  WB-KNR-EREIGNIS-ENDE           PIC S9(04) BINARY.
           05  NI-KNR-EREIGNIS-ENDE           PIC X(01) .
           05  WB-FOLGE-EH-DAT-BIS            PIC X(10).
           05  NI-FOLGE-EH-DAT-BIS            PIC X(01) .
           05  WB-EH-ZUSCHLAG                 PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ZUSCHLAG                 PIC X(01) .
           05  WB-ABZPROZ-REGEL-NEU            PIC S9(03)V99 COMP-3.
           05  NI-ABZPROZ-REGEL-NEU            PIC X(01) .
           05  WB-EH-SOLLANTEIL-NEU           PIC S9(9)V9(1) COMP-3.
           05  NI-EH-SOLLANTEIL-NEU           PIC X(01) .
           05  WB-EH-ABZUG-PROZ-NEU           PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ABZUG-PROZ-NEU           PIC X(01) .
           05  WB-EH-ABZUG-BETR-W             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ABZUG-BETR-W             PIC X(01).
           05  WB-BETRW-EH-DAT-VON            PIC X(10).
           05  NI-BETRW-EH-DAT-VON            PIC X(01).
           05  WB-BETRW-EH-DAT-BIS            PIC X(10).
           05  NI-BETRW-EH-DAT-BIS            PIC X(01).
           05  WB-EH-PARTNER                  PIC S9(9)V9(1) COMP-3.
           05  NI-EH-PARTNER                  PIC X(01).
           05  WB-PART-WERTEN                 PIC X(01).
           05  NI-PART-WERTEN                 PIC X(01).
           05  WB-PART-WERTEN-RGS             PIC X(01).
           05  NI-PART-WERTEN-RGS             PIC X(01).
           05  WB-EH-GES-PZTR-11              PIC S9(9)V9(1) COMP-3.
           05  NI-EH-GES-PZTR-11              PIC X(01) .
           05  WB-ABZPROZ-REGEL-11            PIC S9(03)V99 COMP-3.
           05  NI-ABZPROZ-REGEL-11            PIC X(01) .
           05  WB-EH-SOLLANTEIL-11            PIC S9(9)V9(1) COMP-3.
           05  NI-EH-SOLLANTEIL-11            PIC X(01) .
           05  WB-EH-ABZUG-PROZ-11            PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ABZUG-PROZ-11            PIC X(01) .
           05  WB-PART-EBENE                  PIC X(01).
           05  NI-PART-EBENE                  PIC X(01).
           05  WB-EH-EIG-PZTR-NEU             PIC S9(9)V9(1) COMP-3.
           05  NI-EH-EIG-PZTR-NEU             PIC X(01) .
           05  WB-BW-DATUM                    PIC X(10).
           05  NI-BW-DATUM                    PIC X(01).
           05  WB-EH-GES-PZTR-21              PIC S9(9)V9(1) COMP-3.
           05  NI-EH-GES-PZTR-21              PIC X(01) .
           05  WB-ABZPROZ-REGEL-21            PIC S9(03)V99 COMP-3.
           05  NI-ABZPROZ-REGEL-21            PIC X(01) .
           05  WB-EH-SOLLANTEIL-21            PIC S9(9)V9(1) COMP-3.
           05  NI-EH-SOLLANTEIL-21            PIC X(01) .
           05  WB-EH-ABZUG-PROZ-21            PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ABZUG-PROZ-21            PIC X(01) .
           05  WB-EH-ZUSCHLAG-21              PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ZUSCHLAG-21              PIC X(01) .
           05  WB-EH-ABZ-BETR-W-21            PIC S9(9)V9(1) COMP-3.
           05  NI-EH-ABZ-BETR-W-21            PIC X(01) .

