      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1736.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1736.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 16:50:28   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Dec 11 2013 15:28:22   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Migration
      *
      *   Rev 2.1   Aug 21 2013 14:22:06   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Migration
      *
      *   Rev 2.0   Mar 01 2013 16:35:52   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1736  DATEN AUS PROVIS FÜR MIGRATION AML
      *    IM RAHMEN KALYPSO PROJEKT
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!        !2.1  ! NEU KALYPSO MIGRATION    ! RAHNER   *
      * APA01526!        !2.2  ! KENNZ. AUS VORSCHALTPROG ! RAHNER   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  BESCHREIBUNG DER AUS DEM PROVIS-SYSTEM BEREITGESTELLEN DATEN*
      *  DER GESELLSCHAFT AM-LEBEN UND AM-PENSIONSKASSE FÜR DIE      *
      *  MIGRATION IN DAS DAISY-SYSTEM FÜR ALLE VERTRIEBS-           *
      *  GESESCHAFTEN.                                               *
      *  EINE DATEI ENHÄLT DIE DATEN EINER VERTIEBSGESELLSCHAFT      *
      *  ZU AM-LEBEN ODER PENKA.                                     *
      *  DATEI AUFBAU 1 VORSATZ (A-SATZ)                             *
      *               N DATENSÄTZE (C-SÄTZE)                         *
      *               1 NACHSATZ (E-SATZ)                            *
      ****************************************************************
       01  DRPA1736.
           10  MIGRATION-VORSATZ.
      *
               20  MIG-A-SATZART         PIC X(001).
      *            SATZART = 'A'
               20  MIG-A-ABSENDER        PIC X(016).
      *            ABSENDER = 'PENSIONNSKASSE' ODER 'AML'
               20  MIG-A-EMPFAENGER      PIC X(004).
      *            VERTREIBSGESELLSCHAFT = 'DVAG' / 'ALLF' / 'FVD'
               20  MIG-A-INHALT          PIC X(021).
      *            INHALT = 'MIGRATION'
               20  MIG-A-DATUM           PIC X(010).
      *            DATUM = TT.MM.JHJJ
               20  FILLER                PIC X(1448).
      *
           10  MIGRATION-SATZ REDEFINES  MIGRATION-VORSATZ.
      *
               20  MIG-C-SATZART         PIC  X(001).
      *            SATZART        = 'C'
      *        ALLGEMEIN/ VERTRAG
               20  MIG-C-PG-KZ           PIC  X(002).
      *            20 (AML) 21 (64,PK)
               20  MIG-C-VERMGES         PIC  9(002).
      *
               20  MIG-C-ORDBEGR         PIC  9(008).
      *            = VS-NUMMER
               20  MIG-C-UNNUMMER        PIC  9(003).
      *            = UNTERNUMMER
               20  MIG-C-TARIF-KZ        PIC  9(002).
      *
               20  MIG-C-PROV-SCHICHT-NR PIC  9(003).

               20  MIG-C-PROV-BEG        PIC  9(008).
      *            VERTRAGSBEGINN JJJJMMTT
               20  MIG-C-PROV-ABL        PIC  9(008).
      *            VERTRAGSABLAUF JJJJMMTT
               20  MIG-C-URS-PRPOL-DAT   PIC  9(008).
      *            DATUM ZU DEM POLICIERUNG WIRKSAM WURDE JJJJMMTT
               20  MIG-C-PROV-DATUM      PIC  9(008).
      *            FÄLLIGKEIT DER PROVISION JJJJMMTT
               20  MIG-C-PROD-SCHL       PIC  9(003).
      *
               20  MIG-C-BEARB-DATUM     PIC  9(008).
      *            KOMPLEMENTÄR-BEARBEITUNGSDATUM 99999999 - TAGESDATUM
               20  MIG-C-BEARB-ZEIT-NR   PIC  9(007).
      *            KOMPLEMENTÄR-ZEITSTEMPEL ALS REIHENFOLGENUMMER
               20  MIG-C-PIF             PIC  X(007).
      *            AKTUELLER BETREUER
               20  MIG-C-VERFUEG-ART     PIC  9(003).
      *
               20  MIG-C-FEIV-MIG-KZN    PIC  X(001).
      *
               20  MIG-C-VN-NAME         PIC  X(025).
      *            NACHNAME VERSICHERUNGSNEHMER
               20  MIG-C-VN-VNAME        PIC  X(020).
      *            VORNAME VERSICHERUNGSNEHMER
               20  MIG-C-VN-STRHNR       PIC  X(035).
      *            STRASSE + HAUSNUMMER VERSICHERUNGSNEHMER
               20  MIG-C-VN-PLZ          PIC  X(005).
      *            POSTLEITZAHL VERSICHERUNGSNEHMER
               20  MIG-C-VN-ORT          PIC  X(035).
      *            WOHNORT VERSICHERUNGSNEHMER
               20  MIG-C-BZ-NAME         PIC  X(025).
      *            NACHNAME BEITRAGSZAHLER
               20  MIG-C-BZ-VNAME        PIC  X(020).
      *            VORNAME BEITRAGSZAHLER
               20  MIG-C-VT-NAME         PIC  X(025).
      *            NACHNAME VERSICHERTE PERSON
               20  MIG-C-VT-VNAME        PIC  X(020).
      *            VORNAME VERSICHERTE PERSON
               20  MIG-C-UMW-GESAMT      PIC  9(013).
      *
               20  MIG-C-AVERM           PIC  X(010).
      *            ABSCHLUSSVERMITTLER
               20  MIG-C-AVERM-VBAS-KZN  PIC  X(001).
      *            J = AVERM IST VBAS SONST SPACE
               20  MIG-C-AVERM-KDBETR-KZ PIC  X(001).
      *            KENNZEICHEN ZUR BESTIMMUNG EMPFÄNGERTYP
               20  MIG-C-TARIF-BEZ       PIC  X(015).
      *            FÜR DVAG PRODUKT ZU SCHLÜSSELN
               20  MIG-C-BUZTAR          PIC  9(004).
      *
               20  MIG-C-ZAHLW           PIC  9(002).
      *            ZAHLWEISE
               20  MIG-C-INKST-DATUM     PIC  9(008).
      *            INKASSOSTANDDATUM JJJJMMTT
               20  MIG-C-IA-ART          PIC  9(002).
      *            INKASSOART
               20  MIG-C-SAM-NR          PIC  9(009).
      *            KENNZEICHNUNG VON KOLLEKTIVVERTRÄGEN VOR RABATSTUFEN
               20  MIG-C-RABSTUFE        PIC  X(003).
      *            RABATIERUNGSSTUFE
               20  MIG-C-VVG-KZN         PIC  X(001).
      *            KENNZ. VERSICHERUNGSVERTRAGSGESETZ AUSP. J ODER SPACE
               20  MIG-C-DEPOT-KZ        PIC  X(001).
      *            AUSPRÄGUNG 1;4;5;6;8
               20  MIG-C-PROV-SCHL       PIC  X(005).
      *            PROVISIONSSCHLÜSSEL
               20  MIG-C-BEW-DATUM       PIC  9(008).
      *            BEWEGUNGSDATUM (BEGINN DER BEWEGUNG) JJJJMMTT
               20  MIG-C-POL-DATUM       PIC  9(008).
      *            JJJJMMTT
               20  MIG-C-BESVV           PIC  9(001).
      *
               20  MIG-C-TARIF-SCHL-GRP  PIC  9(003).
      *            TARIFSCHL + TARIFSCHGRP FÜHREN ZUM PRODUKTSCHLÜSSEL
               20  MIG-C-TARIF-SCHL      PIC  9(005).
      *
               20  MIG-C-PROD-ART        PIC  9(001).
      *            PRODART DES PRODUKTGEBERS
               20  MIG-C-BEW-HKFT        PIC  X(001).
      *            BEWEGUNGSSCHLÜSSEL
               20  MIG-C-BEW-ART         PIC  9(006).
      *            BEWEGUNGSSCHLÜSSEL
               20  MIG-C-HZR-BEG         PIC  9(008).
      *            BEGINN DER HAFTUNGSZEIT IM VERHÄLTINIS AML-DVAG
               20  MIG-C-HZR-END         PIC  9(008).
      *            ENDE DER HAFTUNGSZEIT IM VERHÄLTINIS AML-DVAG
               20  MIG-C-DYN-KZ1         PIC  9(001).
      *
               20  MIG-C-DYN-KZ2         PIC  9(001).
      *            AUSPRÄGUNG 0; 1; 3; 4
               20  MIG-C-JAHRES-BTG          PIC S9(007)V99.
      *            PROVISIONSPFLICHTIGER JAHRESBEITRAG JE TARIFTEIL
               20  MIG-C-RATEN-BTG           PIC S9(007)V99.
      *            RATENBEITRAG JE SCHICHT ABHÄNGIG VON ZAHLWEISE
               20  MIG-C-NEUSTO-KZ           PIC  X(001).
      *            AUSPRÄGUNG J; D; N ; SPACE
               20  MIG-C-BZ-DAUER            PIC  9(004).
      *            BEITRAGSZAHLUNGSDAUER DES JEW. TARIFTEILS
               20  MIG-C-DAUER               PIC  9(004).
      *            VERSICHERUNGSDAUER (BEGINN/ABLAUF) IN MONATEN
               20  MIG-C-PRO-RATA-FAKT-1BIS3 PIC  9(003).
      *            ZÄHLER FÜR EINHEITEN VOLLSTÄNDIG ZU BELASTEN
               20  MIG-C-PRO-RATA-FAKT-4BIS6 PIC  9(003).
      *            NENNER FÜR EINHEITEN VOLLSTÄNDIG ZU BELASTEN
               20  MIG-C-PRO-RATA-SU         PIC S9(007)V99.
      *            HÖHE PRO-RATA-SUMME
               20  MIG-C-PR-PFL-SU           PIC S9(007)V99.
      *            PROVISIONSPFLICHTIGE SUMME
               20  MIG-C-PR-PFL-BTG          PIC S9(007)V99.
      *            PROVISIONSPFLICHTIGER BEITRAG (WAVE)
               20  MIG-C-GESAMT-PROVISION    PIC S9(007)V99.
      *
               20  MIG-C-RWERTSU             PIC S9(009).
      *            REDUZIERTE BEWERTUNGSSUMME
               20  MIG-C-BWERTSU             PIC S9(009).
      *            NORMALE BEWERTUNGSSUMME
               20  MIG-C-RWERTBTG            PIC S9(007)V99.
      *            REDUZIERTER BEWERTUNGSBEITRAG (WAVE)
               20  MIG-C-PROV-VERDIENT       PIC  9(004).
      *            ANTEIL VERDIENSTE PROVISION
               20  MIG-C-VS-BETRAG           PIC S9(009).
      *            VERSICHERUNGSSUMMEN, VERSICHERUNGSBETRAG JE TARIFTEIL
               20  MIG-C-TARGR               PIC  9(003).
      *
               20  MIG-C-PROD-ART-VG         PIC  9(001).
      *
               20  MIG-C-BUCH-DAT            PIC  9(008).
      *            BUCHUNGS/ABRECHNUNGSDATUM JJJJMMTT
               20  MIG-C-AE                  PIC S9(006)V9.
      *            ABSCHLUSSEINHEITEN
               20  MIG-C-GRUND-PROV          PIC S9(003).
      *
      *        STRUKTURZAEHLER PE
               20  MIG-C-ANZ-PE              PIC  9(002).

      *        PROVISIONSEMPFÄNGERSTRUKTUR
               20  MIG-C-PROV-EMPF-STRUK OCCURS 20.

                   30 MIG-C-STRUKTUR-PE-MANR      PIC  X(010).
      *               VB-NUMMER INKL. PRÜFZIFFER
                   30 MIG-C-STRUKTUR-PE-VERF      PIC  X(008).
      *               PROVISIONSVERFAHREN JE PROVISIONSEMPFÄNGER (%)
                   30 MIG-C-STRUKTUR-PE-VGSATZ    PIC S9(003)V99.
      *               PROVISIONSVERGÜTUNGSSATZ JE PROVISIONSEMPFÄNGER(%)
                   30 MIG-C-STRUKTUR-PE-PROV-KZ   PIC  X(001).
      *               PROVISIONSKENNZEICHEN
                   30 MIG-C-STRUKTUR-PE-PROVISION PIC S9(007)V99.
      *               PROVISIONSBETRAG  JE PROVISIONSEMPFÄNGER
                   30 MIG-C-STRUKTUR-PE-AE        PIC S9(006)V9.
      *               ABSCHLUSSEINHEITEN JE PROVISIONSEMPFÄNGER
      *
201113         20  MIG-C-PROD-SCHL-GEAENDERT      PIC  X(001).
201113*        20  FILLER                         PIC  X(149).
201113         20  FILLER                         PIC  X(148).
      *            RESERVE
      *
           10  MIGRATION-NACHSATZ REDEFINES  MIGRATION-VORSATZ.
      *
               20  MIG-E-SATZART         PIC X(001).
      *            SATZART = 'E'
               20  MIG-E-ANZ-SAETZE      PIC 9(009).
      *            ANZAHL SÄTZE
               20  FILLER                PIC X(1490).

