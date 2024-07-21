      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0005.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRVD0005.CPv  $
      *
      *   Rev 2.6   Jun 05 2012 11:08:02   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.5   Jun 05 2012 09:48:04   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.4   Jan 19 2005 08:24:14   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.3   Jan 11 2005 09:10:16   t13ex49
      *AKI00422: VBI-DWH Schnittstelle Abweichung Produktnummer
      *
      *   Rev 2.2   Jan 03 2005 11:28:58   t13ex49
      *AKI00422: VBI-DWH Schnittstelle Abweichung Produktnummer
      *
      *   Rev 2.1   Nov 23 2004 16:48:02   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Jun 22 2004 09:27:24   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!16.06.04!    ! Neu                       ! P. Pohl   *
      * --------------------------------------------------------------*
      * AKI00422!23.12.04!    ! Unbenutzte Felder durch   ! P. Pohl   *
      *         !        !    ! Filler ersetzt            !           *
      *         !        !    !                           !           *
      * AKI01124!27.02.12!    ! erw. um Vertragsablauf,   ! B. Stoldt *
      *         !        !    ! Beitrag gem. Zahlweise    !           *
      *****************************************************************
      *
      * Daten aus Vertrag und VertragRolleBez und KundenInteressent
      *
      * Satzlänge 320 Byte
      *
       01 DRVD0005.

      * aus Vertrag/VertragRolleBez
           05  DRVD0005-KDPERSONENID              PIC X(13).
           05  DRVD0005-VBPERSONENID              PIC X(13).
           05  DRVD0005-VERTRAGSID                PIC X(13).
           05  DRVD0005-VRBKOMMBETRKZ             PIC X(01).
           05  DRVD0005-VERPARTGESELLSCHID        PIC 99.
           05  DRVD0005-VERVERTRAGSNR             PIC X(15).
           05  DRVD0005-VERVERTRAGSNR-NI          PIC X(01).
           05  DRVD0005-VERVERTRZUSTANDID         PIC X(02).
           05  DRVD0005-VERVERTRZUSTANDID-NI      PIC X(01).
           05  DRVD0005-VERVERTRAGSBEGINN         PIC X(10).
           05  DRVD0005-VERVERTRAGSBEGINN-NI      PIC X(01).
           05  DRVD0005-VEREIGENVERMKZ            PIC X(01).
           05  DRVD0005-VERVERTRAGSABLAUF         PIC X(10).
           05  DRVD0005-VERVERTRAGSABLAUF-NI      PIC X(01).


      * aus Vertrag
           05  DRVD0005-VERABSCHLUSSVBNR          PIC X(07).
           05  DRVD0005-VERABSCHLUSSVBNR-NI       PIC X(01).
           05  DRVD0005-VERGESAMTBEITRAG          PIC S9(9)V9(2) COMP-3.
           05  DRVD0005-VERGESAMTBEITRAG-NI       PIC X(01).
           05  DRVD0005-VERGESBEITRWKZ            PIC X(01).
           05  DRVD0005-VERZAHLUNGSWEISEID        PIC S9(4) COMP.
           05  DRVD0005-VERZAHLUNGSWEISEID-NI     PIC X(01).

      * aus KundenIntVBBEZ
           05  DRVD0005-KVBKUNDEBETREUUNG         PIC X(01).
           05  DRVD0005-KVBKUNDEBETREUUNG-NI      PIC X(01).

      * aus KundenInteressent
           05  DRVD0005-KIKUNDENSTATUSID          PIC X(01).
           05  DRVD0005-KIKUNDENNUMMER            PIC X(10).

      * aus Mitarbeiter/AussendienstMA
           05  DRVD0005-MITARBEITERNR             PIC X(07).
           05  DRVD0005-MITARBEITERNR-NI          PIC X(01).
           05  DRVD0005-ADMAAUSTRITTSDATUM        PIC X(10).
           05  DRVD0005-ADMAAUSTRITTSDATUM-NI     PIC X(01).
           05  DRVD0005-ADMSTRUKTURSTUFE          PIC X(03).
           05  DRVD0005-ADMSTRUKTURSTUFE-NI       PIC X(01).

           05  DRVD0005-MITARBEITERNR-V           PIC X(07).
           05  DRVD0005-MITARBEITERNR-V-NI        PIC X(01).
           05  DRVD0005-ADMAAUSTRITTSDA-V         PIC X(10).
           05  DRVD0005-ADMAAUSTRITTSDA-V-NI      PIC X(01).

      * Anzahl versicherte Personen bei Central / AMV-Unfall
           05  DRVD0005-ANZVP                     PIC 9(03).

      * 10 Kennzeichen
           05  DRVD0005-AML-DYN-ERH               PIC X(01).
           05  DRVD0005-AML-BEITRAGSFREI          PIC X(01).
           05  DRVD0005-VWL                       PIC X(01).
           05  DRVD0005-KENNZ4-frei               PIC X(01).
           05  DRVD0005-KENNZ5-frei               PIC X(01).
           05  DRVD0005-KENNZ6-frei               PIC X(01).
           05  DRVD0005-KENNZ7-frei               PIC X(01).
           05  DRVD0005-KENNZ8-frei               PIC X(01).
           05  DRVD0005-KENNZ9-frei               PIC X(01).
           05  DRVD0005-KENNZ10-frei              PIC X(01).

      * Betreuungsverhältniss
           05  DRVD0005-ANZ-Betreuer              PIC 9(03).
      * Ermittelte Kennzeichen
           05  DRVD0005-Kundenart-VB              PIC X(01).
           05  DRVD0005-Betreuung-DVAG            PIC X(01).
           05  DRVD0005-Betreuung-VB              PIC X(01).


           05  Filler                             PIC X(05).
           05  DRVD0005-PRODLFDNR                 PIC S9(4) COMP.
           05  DRVD0005-GHPEIGENVERMITTELT        PIC X(01).
           05  DRVD0005-GHPABSCHLUSSVBNR          PIC X(07).
           05  Filler                             PIC X(01).
           05  DRVD0005-GHPOFFNUNGSDATUM          PIC X(10).
           05  DRVD0005-GHPSCHLIESSDATUM          PIC X(10).
           05  Filler                             PIC X(10).
           05  FILLER                             PIC X(26).


      *---------------------------------------------------------

      * Ermittelt aus verschiedenen Tabellen
           05  DRVD0005-PRODUKTID                 PIC 9(04).
      *
           05  DRVD0005-Vers-Summe                PIC S9(9)V9(2) COMP-3.
      *        AML, Badenia                 -> Versicherungssumme
      *        DIT, III, SEB Inv,
      *        SEB Immo, DGI, DB (Depot)    -> Depotwert
      *        DB (nicht Depot)             -> Guthaben
      *        sonst null
           05  DRVD0005-Vers-Summe-WKZ            PIC X(1).


           05  DRVD0005-Vers-Summe-DYN            PIC S9(9)V9(2) COMP-3.
      *        nur AML
           05  DRVD0005-Vers-Summe-DYN-WKZ        PIC X(1).


           05  DRVD0005-JBTG-Brutto               PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und Vers.Steuer
           05  DRVD0005-JBTG-Netto1               PIC S9(9)V9(2) COMP-3.
      *        ohne Ratenzuschlag und ohne Vers.Steuer
      *        nur AML, AMV, Central, AdvoCard, DMB
           05  DRVD0005-JBTG-Netto2               PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und ohne Vers.Steuer
           05  DRVD0005-BTG-GEMZW                 PIC S9(9)V9(2) COMP-3.
      *        Beitrag gem. Zahlweise
           05  DRVD0005-JBTG-WKZ                  PIC X(1).


           05  DRVD0005-JDYNBTG-Brutto            PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und Vers.Steuer
           05  DRVD0005-JDYNBTG-Netto1            PIC S9(9)V9(2) COMP-3.
      *        ohne Ratenzuschlag und ohne Vers.Steuer
      *        nur AML
           05  DRVD0005-JDYNBTG-Netto2            PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und ohne Vers.Steuer
           05  DRVD0005-JDYNBTG-WKZ               PIC X(1).



           05  FILLER                             PIC X(083).


      *----------- Ende DRVD0005 ---------------------------
      *