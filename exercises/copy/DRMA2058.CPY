      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2058.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2058.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:27:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Stornobegleitliste aufbereitet
      *
      *     SOURCE-NAME      :  DRMA2058
      *     DMR-NAME         :  DR-OV2058
      *
      *     AUTOR            :  WALBERT, GABY
      *     GENERIERUNG      :  15.12.10/16:23
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA2058.
              07 VBNR                   PIC 9(7).
              07 DIR-NR                 PIC 9(4).
              07 ZUNAME-VB              PIC X(40).
              07 VORNAME-VB             PIC X(20).
              07 VERTRAGS-ID            PIC X(10).
              07 VERTRAGS-ID-AUFBER     PIC X(15).
              07 NAME-VN                PIC X(25).
              07 VORN-VN                PIC X(20).
              07 STR-VN                 PIC X(25).
              07 PLZ-VN                 PIC X(5).
              07 ORT-VN                 PIC X(40).
              07 GESKZ                  PIC 99.
              07 PRODSCHL               PIC 999.
              07 TARIFBEZ               PIC X(10).
              07 TARIFKZ-LANG           PIC X(20).
              07 BEWART                 PIC 9(6).
              07 BEWART-LANG            PIC X(20).
              07 INKST                  PIC X(10).
              07 PROVBETRAG-VB          PIC S9(11)V99  PACKED-DECIMAL.
              07 AE-VB                  PIC S9(10)V9   PACKED-DECIMAL.
              07 BUCHDAT                PIC X(10).
              07 D-BETREUER-VBNR        PIC 9(7).
              07 PROVBETRAG-BETREUER    PIC S9(11)V99  PACKED-DECIMAL.
              07 TARIFKZ                PIC 99.
      *
      **************************************************************
