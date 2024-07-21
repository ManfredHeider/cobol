      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2936.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRKI2936.CPv  $
      *
      *   Rev 2.11   Mar 27 2017 09:02:28   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.10   Mar 03 2016 09:48:34   t13ex15
      *AKI01414: VBI SST G50 Ausschl Reisevers
      *
      *   Rev 2.9   Feb 18 2016 09:37:46   t13ex15
      *AKI01414: VBI SST G50 Ausschl Reisevers
      *
      *   Rev 2.8   Feb 08 2016 14:27:58   t13ex15
      *AKI01414: VBI SST G50 Ausschl Reisevers
      *
      *   Rev 2.7   Mar 12 2015 13:32:40   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.6   Dec 03 2014 13:29:24   t13ex15
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD ** Uwes***
      *
      *   Rev 2.5   Nov 10 2014 11:36:56   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.4   Sep 29 2014 14:14:34   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.3   Sep 25 2014 12:05:06   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.2   Aug 29 2014 14:42:24   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.1   Aug 20 2014 17:29:46   t13ex20
      *AKI01312: Anp. AS-Club/VBI-SST wg FVD
      *
      *   Rev 2.0   Aug 04 2014 15:42:40   t13ex20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2936                                      *
      *    --------------------------------------------------------   *
      *    SST Für UPROs zur Beitragsermittlung für AS-Club und VBI   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Uwe Zimmermann                                   *
      *    DATUM:    04.08.2014                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01312!04.08.14! 01 ! NEUERSTELLUNG             ! UZI      *
      * -------------------------------------------------------------*
180215* AKI01337!18.02.15!2.6 ! KLZ Anpassungen           ! SWZ      *
      * -------------------------------------------------------------*
190116* AKI01414!19.01.16!2.20! Ausschluss Reiseversich.  ! SWZ      *
      * --------+--------+----+---------------------------+----------*
      * FKI01544!22.03.17!    ! III entfernt              ! HHU      *
      *         !        !    !                           !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
      * Daten aus Vertrag und VertragRolleBez und KundenInteressent
      *
      * Satzlänge 320 Byte
      *
       01 DRKI2936.
 
      * aus Vertrag/VertragRolleBez
           05  DRKI2936-KDPERSONENID              PIC X(13).
           05  DRKI2936-VBPERSONENID              PIC X(13).
           05  DRKI2936-VERTRAGSID                PIC X(13).
           05  DRKI2936-VRBKOMMBETRKZ             PIC X(01).
           05  DRKI2936-VERPARTGESELLSCHID        PIC 99.
           05  DRKI2936-VERVERTRAGSNR             PIC X(15).
           05  DRKI2936-VERVERTRAGSNR-NI          PIC X(01).
           05  DRKI2936-VERVERTRZUSTANDID         PIC X(02).
           05  DRKI2936-VERVERTRZUSTANDID-NI      PIC X(01).
           05  DRKI2936-VERVERTRAGSBEGINN         PIC X(10).
           05  DRKI2936-VERVERTRAGSBEGINN-NI      PIC X(01).
           05  DRKI2936-VEREIGENVERMKZ            PIC X(01).
           05  DRKI2936-VERVERTRAGSABLAUF         PIC X(10).
           05  DRKI2936-VERVERTRAGSABLAUF-NI      PIC X(01).
 
 
      * aus Vertrag
           05  DRKI2936-VERABSCHLUSSVBNR          PIC X(07).
           05  DRKI2936-VERABSCHLUSSVBNR-NI       PIC X(01).
           05  DRKI2936-VERGESAMTBEITRAG          PIC S9(9)V9(2) COMP-3.
           05  DRKI2936-VERGESAMTBEITRAG-NI       PIC X(01).
           05  DRKI2936-VERGESBEITRWKZ            PIC X(01).
           05  DRKI2936-VERZAHLUNGSWEISEID        PIC S9(4) COMP.
           05  DRKI2936-VERZAHLUNGSWEISEID-NI     PIC X(01).
 
      * aus KundenIntVBBEZ
           05  DRKI2936-KVBKUNDEBETREUUNG         PIC X(01).
           05  DRKI2936-KVBKUNDEBETREUUNG-NI      PIC X(01).
 
      * aus KundenInteressent
           05  DRKI2936-KIKUNDENSTATUSID          PIC X(01).
           05  DRKI2936-KIKUNDENNUMMER            PIC X(10).
 
      * aus Mitarbeiter/AussendienstMA
           05  DRKI2936-MITARBEITERNR             PIC X(07).
           05  DRKI2936-MITARBEITERNR-NI          PIC X(01).
           05  DRKI2936-ADMAAUSTRITTSDATUM        PIC X(10).
           05  DRKI2936-ADMAAUSTRITTSDATUM-NI     PIC X(01).
           05  DRKI2936-ADMSTRUKTURSTUFE          PIC X(03).
           05  DRKI2936-ADMSTRUKTURSTUFE-NI       PIC X(01).
 
           05  DRKI2936-MITARBEITERNR-V           PIC X(07).
           05  DRKI2936-MITARBEITERNR-V-NI        PIC X(01).
           05  DRKI2936-ADMAAUSTRITTSDA-V         PIC X(10).
           05  DRKI2936-ADMAAUSTRITTSDA-V-NI      PIC X(01).
 
      * Anzahl versicherte Personen bei Central / AMV-Unfall
           05  DRKI2936-ANZVP                     PIC 9(03).
 
      * 10 Kennzeichen
           05  DRKI2936-AML-DYN-ERH               PIC X(01).
           05  DRKI2936-AML-BEITRAGSFREI          PIC X(01).
           05  DRKI2936-VWL                       PIC X(01).
           05  DRKI2936-KENNZ4-frei               PIC X(01).
           05  DRKI2936-KENNZ5-frei               PIC X(01).
           05  DRKI2936-KENNZ6-frei               PIC X(01).
           05  DRKI2936-KENNZ7-frei               PIC X(01).
           05  DRKI2936-KENNZ8-frei               PIC X(01).
           05  DRKI2936-KENNZ9-frei               PIC X(01).
           05  DRKI2936-KENNZ10-frei              PIC X(01).
 
      * Betreuungsverhältniss
           05  DRKI2936-ANZ-Betreuer              PIC 9(03).
      * Ermittelte Kennzeichen
           05  DRKI2936-Kundenart-VB              PIC X(01).
           05  DRKI2936-Betreuung-DVAG            PIC X(01).
           05  DRKI2936-Betreuung-VB              PIC X(01).
 
 
           05  Filler                             PIC X(05).
           05  DRKI2936-PRODLFDNR                 PIC S9(4) COMP.
           05  DRKI2936-GHPEIGENVERMITTELT        PIC X(01).
           05  DRKI2936-GHPABSCHLUSSVBNR          PIC X(07).
           05  Filler                             PIC X(01).
           05  DRKI2936-GHPOFFNUNGSDATUM          PIC X(10).
           05  DRKI2936-GHPSCHLIESSDATUM          PIC X(10).
           05  Filler                             PIC X(10).
           05  FILLER                             PIC X(26).
 
 
      *---------------------------------------------------------
 
      * Ermittelt aus verschiedenen Tabellen
           05  DRKI2936-PRODUKTID                 PIC 9(04).
      *
           05  DRKI2936-Vers-Summe                PIC S9(9)V9(2) COMP-3.
      *        AML, Badenia                 -> Versicherungssumme
      *        DIT, SEB Inv,
      *        SEB Immo, DGI, DB (Depot)    -> Depotwert
      *        DB (nicht Depot)             -> Guthaben
      *        sonst null
           05  DRKI2936-Vers-Summe-WKZ            PIC X(1).
 
 
           05  DRKI2936-Vers-Summe-DYN            PIC S9(9)V9(2) COMP-3.
      *        nur AML
           05  DRKI2936-Vers-Summe-DYN-WKZ        PIC X(1).
 
 
           05  DRKI2936-JBTG-Brutto               PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und Vers.Steuer
           05  DRKI2936-JBTG-Netto1               PIC S9(9)V9(2) COMP-3.
      *        ohne Ratenzuschlag und ohne Vers.Steuer
      *        nur AML, AMV, Central, AdvoCard, DMB
           05  DRKI2936-JBTG-Netto2               PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und ohne Vers.Steuer
           05  DRKI2936-BTG-GEMZW                 PIC S9(9)V9(2) COMP-3.
      *        Beitrag gem. Zahlweise
           05  DRKI2936-JBTG-WKZ                  PIC X(1).
 
 
           05  DRKI2936-JDYNBTG-Brutto            PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und Vers.Steuer
           05  DRKI2936-JDYNBTG-Netto1            PIC S9(9)V9(2) COMP-3.
      *        ohne Ratenzuschlag und ohne Vers.Steuer
      *        nur AML
           05  DRKI2936-JDYNBTG-Netto2            PIC S9(9)V9(2) COMP-3.
      *        mit Ratenzuschlag und ohne Vers.Steuer
           05  DRKI2936-JDYNBTG-WKZ               PIC X(1).
 
           05  DRKI2936-GESGUTHAB               PIC  S9(9)V9(2) COMP-3.
           05  DRKI2936-MON-BETRAG              PIC  S9(9)V9(2) COMP-3.
 
           05  DRKI2936-GHPDEPOTART               PIC  X(001).
           05  DRKI2936-TOPZINS-KZ                PIC  X(001).
           05  DRKI2936-JBTG-Netto1-ASC           PIC S9(9)V9(2) COMP-3.
      *        ohne Ratenzuschlag und ohne Vers.Steuer
      *        - AML
190116*        - CKV Ausschluss ReiseKV
180215     05  DRKI2936-JBTG-Netto1-OEZ           PIC S9(9)V9(2) COMP-3.
      *        ohne Einmalzahlungen
180215     05  DRKI2936-JBTG-Netto1-EZ           PIC S9(9)V9(2) COMP-3.
      *        ohne Einmalzahlungen
190116     05  DRKI2936-WERTUNG                  PIC X(1).
      *        Wertung CKV ReiseKV
 
180215*    05  FILLER                             PIC X(063).
180215*    05  FILLER                             PIC X(051).
190116     05  FILLER                             PIC X(050).
 
 
      *----------- Ende DRKI2936 ---------------------------
      *
 
 
 
 

