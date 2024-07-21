      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0022.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVD0022.CPv  $
      *
      *   Rev 2.3   Dec 21 2020 17:47:40   t13ae30
      *AVB01642: Anpassung PMVD0010
      *
      *   Rev 2.2   Jun 03 2015 12:40:36   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.1   Sep 23 2005 11:18:02   t13ex22
      *AVD00008: Projekt VBI: SST Beförderungsprüfung
      *
      *   Rev 2.0   Feb 11 2005 09:46:42   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * SATZAUFBAU ZUR TABELLE EHNBP - TBVB0052
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!26.02.15!2.2 ! Neue Verwendung:          ! S. Walb  *
      *         !        !    ! Satzaufbau für TBVB0052   !          *
      * VLA-988 !12.10.20!2.0 ! ERW. UM                   ! T13AE30  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       03  DCL-TBVB0052.
           05  WB-EHNPB-VS-ZEITRAUM           PIC S9(4) COMP.
           05  WB-EHNPB-BETR                  PIC S9(9) COMP.
           05  WB-EHNPB-UNTST                 PIC S9(9) COMP.
           05  WB-EHNPB-EBENE                 PIC X(01).
           05  WB-EHNPB-DAT-VON               PIC X(10).
           05  WB-EHNPB-DAT-BIS               PIC X(10).
           05  WB-EHNPB-ERSTELL-DAT           PIC X(10).
           05  WB-EHNPB-EH-NEU-EIG            PIC S9(9)V9(1) COMP-3.
           05  WB-EHNPB-NP-VON                PIC X(10).
           05  WB-EHNPB-NP-BIS                PIC X(10).
           05  WB-EHNPB-ABZPROZ-KL            PIC S9(03)V99 COMP-3.
           05  NI-EHNPB-ABZPROZ-KL            PIC X(01).
           05  WB-EHNPB-EH-SOLLANT            PIC S9(9)V9(1) COMP-3.
           05  NI-EHNPB-EH-SOLLANT            PIC X(01).
           05  WB-EHNPB-EH-ABZUG-KL           PIC S9(9)V9(1) COMP-3.
           05  NI-EHNPB-EH-ABZUG-KL           PIC X(01).
           05  WB-EHNPB-BESTUEB-R-K           PIC X(01).
121020     05  WB-EHNPB-NW-UEL-BESTU          PIC X(01).

