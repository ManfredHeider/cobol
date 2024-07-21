      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2447.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2447.CPv  $
      *
      *   Rev 2.2   Mar 29 2011 12:51:56   t13ex39
      *AKI00994: AllianzGI: Bereinigung von DAGI-Bestandsdepots
      *
      *   Rev 2.1   Mar 29 2011 12:47:40   t13ex39
      *AKI00994: AllianzGI: Bereinigung von DAGI-Bestandsdepots
      *
      *   Rev 2.0   Mar 29 2011 12:41:38   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU DEN AUFBEREITUNGSPROGRAMMEN KI-NEU           *
      *    - DTA AGI VERTRAGSDATEN UND VERTRAGSDATEN-FEHLERDATEI       *
      *                                                                *
      *    SATZLÄNGE   255                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *29.03.11!NEUERSTELLUNG                           ! who     ! 01 *
      *AKI00884!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
CCC009*    SKIP1
       01  DRKI2447.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  AGI-VORSATZ.
      *
               15  AGI-V-SA                           PIC  X(001).
      *            SATZART = 'A'
               15  AGI-V-ABSENDER                     PIC  X(008).
      *            ABSENDER = 'AGI,HOF'
               15  AGI-V-DATUM                        PIC  9(008).
      *            ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
               15  AGI-V-EMPFAENGER                   PIC  X(004).
      *            EMPFAENGER = 'DVAG
               15  AGI-V-TEXT                         PIC  X(010).
      *            INHALTSANGABE:
      *            Tägliche Updatelieferung DASAGI  'SPARDE'
      *            Komplettbestand          DASAGI  'SPARDEKP'
      *            Tägliche Updatelieferung DFDB    'SPARAP'
      *            Komplettbestand          DFDB    'SPARAPKP'

               15  AGI-V-WKZ                          PIC  X(003).
      *            WÄHRUNGSKENNZEICHEN
               15  FILLER                             PIC  X(221).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AGI-SATZ     REDEFINES      AGI-VORSATZ.

               15  AGI-SA                             PIC  X(001).
      *            SATZART        = 'C'
               15  AGI-GES                            PIC  9(002).
      *            GESELLSCHAFT   = '40'
               15  AGI-KEY.
                   20  AGI-VNR                        PIC  9(013).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   20  AGI-UNTER-NR                   PIC  9(002).
      *            Depotunternummer
                   20  AGI-LFDNR                      PIC  9(004).
      *            laufende Nummer (Eindeutige Nummer - kurz )
                   20  AGI-WKN                        PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
                   20  AGI-ISIN                       PIC  X(012).
      *            ISIN, sofern vorhanden

               15  AGI-SPARRATE                       PIC  9(007)V99.
      *            SPARLEISTUNG FUER SPARPLAENE
               15  AGI-DYNAM                          PIC  9(002)V99.
      *            DYNAMISIERUNG FUER SPARPLAENE in %
               15  AGI-ZAHLWEISE                      PIC  9(001).
      *            ZAHLWEISE......***0 : ZAHLWEISE FEHLT
      *            ZAHLWEISE........*1 : JAEHRLICH
      *            ZAHLWEISE........*2 : HALBJAEHRLICH
      *            ZAHLWEISE........*4 : VIERTELJAEHRLICH
      *            ZAHLWEISE........*5 : 2-MONATLICH
      *            ZAHLWEISE........*6 : MONATLICH
      *            ZAHLWEISE.........8 : EINMALPRAEMIE
               15  AGI-TAG                            PIC  9(002).
      *            Tag im Monat für die Einzahlung
               15  AGI-BUCHSCHL-EIN                   PIC  9(002).
      *            Buchungsschluessel für Einzahlung
               15  AGI-DATUM-EROEFFNUNG.
                   25  JAHR-EROEFFNUNG                PIC  9(004).
                   25  MONAT-EROEFFNUNG               PIC  9(002).
                   25  TAG-EROEFFNUNG                 PIC  9(002).
      *            DATUM SPARPLANERÖFFNUNG JHJJMMTT
               15  AGI-DATUM-SCHLIESSUNG.
                   25  JAHR-SCHLIESSUNG               PIC  9(004).
                   25  MONAT-SCHLIESSUNG              PIC  9(002).
                   25  TAG-SCHLIESSUNG                PIC  9(002).
      *            DATUM VERTRAGSENDE JHJJMMTT
               15  AGI-BANKVERBINDUNG                 PIC  X(020).
      *            Name des Instituts
               15  AGI-BLZ                            PIC  9(011).
      *            Bankleitzahl
               15  AGI-KONTONUMMER                    PIC  9(018).
      *            KONTO-NUMMER
               15  AGI-ABW-KONTOINHABER               PIC  X(027).
      *            Name des abweichenden Kontoinhabers
               15  AGI-DATUM-VERTRAGSENDE.
                   25  JAHR-VERTRAGSENDE              PIC  9(004).
                   25  MONAT-VERTRAGSENDE             PIC  9(002).
                   25  TAG-VERTRAGSENDE               PIC  9(002).
      *            Ende der Einzahlungsfrist bei VL-Verträgen JHJJMMTT
               15  AGI-VERTRAGSTYP                    PIC  X(002).
      *            PM = Premium Mandant
      *            PP = Aufbauplan Plus
      *            SV = Aufbauplan VL
      *            ST = Standard
      *            VL = Vermögenswirksame Leistungen
      *            VW = Versorgungswerk
      *            AV = AGI-VL-DEPOT
      *            AP = AGI AS-Vorsorgeplan
      *            AS = AGI Altersvorsorge-Sprarplan
      *            VP = AGI Vorsorgeplan
      *            IF = AGI Fondsdepot
      *
      *            IF,PM,ST werden nochmal unterschieden in
      *            IF = AGI Fondsdepot           Einmalzahlung
      *            PM = Premium Mandant          Einmalzahlung
      *            ST = Standard                 Einmalzahlung
      *
      *            IE = AGI Fondsdepot           Einzahlplan
      *            PE = Premium Mandant          Einzahlplan
      *            SE = Standard                 Einzahlplan
      *
      *            IA = AGI Fondsdepot           Auszahlplan
      *            PA = Premium Mandant          Auszahlplan
      *            SA = Standard                 Auszahlplan
      *
               15  AGI-BEGINN-RATENAUSSETZUNG         PIC  9(008).
      *            Beginndatum der Ratenaussetzung JHJJMMTT,
      *            wird bei einer erneuten Aussetzung überschrieben
               15  AGI-ENDE-RATENAUSSETZUNG           PIC  9(008).
      *            Endedatum der Ratenaussetzung JHJJMMTT,
      *            wird bei einer erneuten Aussetzung überschrieben
               15  AGI-DEL-KZ                         PIC  X(001).
      *            Delete-Kennzeichen
               15  AGI-KOSTEN-PROZ                    PIC  9(001)V99.
      *            Kundenentgelt in % auf Basis Ursprungsrate
               15  AGI-URSPR-GES-SPAR-LEIST           PIC  9(006)V99.
      *            Gesamtsparrate auf Basis Ursprungsrate in EUR
               15  AGI-URSPR-SPAR-RATE                PIC  9(004)V99.
      *            Ursprüngliche Sparrate in EUR
               15  AGI-AUSG-AUFSCHL-URSPR             PIC  9(002)V99.
      *            Ausgabeaufschlag des ursprünglich gewählten Fonds
               15  AGI-AUSG-AUFSCHL-AKTUELL           PIC  9(002)V99.
      *            Ausgabeaufschlag des aktuell gewählten Fonds
               15  AGI-VERTR-LAUFZEIT                 PIC  9(002).
      *            Vertragslaufzeit in Jahren
               15  AGI-BEG-DYNAM                      PIC  9(008).
      *            JHJJMMTT JAHR, AB WANN BZW. WIEDER DIE
      *            DYNAMISIERUNG DURCHGEFÜHRT WIRD
               15  AGI-ZAEHLER-LASTSCHR-RUECK         PIC  X(003).
      *          **MD1 = 1. LASTSCHRIFTRÜCKGABE MANGELD DECKUNG
      *          **MD2 = 2. LASTSCHRIFTRÜCKGABE MANGELD DECKUNG
      *          **MD3 = FRISTABLAUF FÜR STORNO WEGEN LASTSCHRIFTRÜCK-
      *          **      GABE MANGELD DECKUNG
      *          **WW1 = 1. LASTSCHRIFTRÜCKGABE WEGEN WIDERSPRUCH
      *          **WW2 = 2. LASTSCHRIFTRÜCKGABE WEGEN WIDERSPRUCH
      *          **WW3 = FRISTABLAUF FÜR STORNO WEGEN LASTSCHRIFT-
      *          **      RÜCKGABE WEGEN WIDERSPRUCH
               15  AGI-KND-MERKMAL                    PIC  X(001).
      *            K ODER LEER
               15  AGI-DAT-VERTR-AUFLOESUNG           PIC  9(008).
      *            DATUM VERTRAGSAUFLÖSUNG  BZW. VORZEITIGE
      *            VERTRAGSKÜNDIGUNG JHJJMMTT
               15  AGI-DYNAMISIERUNG                  PIC  X(001).
      *            'D' = DYNAMISIERUNG
               15  AGI-EINDEUTIGE-SPARPL-NR           PIC  9(010).
      *            SPARPLAN-NR, EINDEUTIG ÜBERS GANZE SYSTEM
               15  AGI-KURZE-SPARPL-NR                PIC  9(004).
      *            4-STELLIGE SPARPLANIDENTIFIKATION
               15  AGI-HIST-DATUM-EROEFFNUNG          PIC  9(008).
      *            DATUM DER URSPRÜNGL. SPARPLANERÖFFNUNG (AUS DEM
      *               ALTSYSTEM)-WENN VORHANDEN,DANN GÜLTIG

      *        15  AGI-RESERVE                        PIC  X(016).
               15  AGI-RESERVE                        PIC  X(008).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  AGI-NACHSATZ    REDEFINES     AGI-VORSATZ.
      *
               15  AGI-N-SA                           PIC  X(001).
      *            SATZART             'E'
               15  AGI-N-ANZ                          PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               15  FILLER                             PIC  X(247).
      *            RESERVE


