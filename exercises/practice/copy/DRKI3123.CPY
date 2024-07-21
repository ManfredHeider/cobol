      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3123.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3123.CPv  $
      *
      *   Rev 2.3   Nov 18 2019 14:04:40   t13ex39
      *AKI01733: FoDB Verarbeitung "urspr. Sparrate des Sparplan"
      *
      *   Rev 2.2   Apr 11 2017 09:37:22   t13ex78
      *AKI01509: G40 - Bereinigung Fondsdepotbank Sparpläne
      *
      *   Rev 2.1   Apr 10 2017 09:58:12   t13ex78
      *AKI01480: Anpassung der Verarbeitung Fondsdepotbank-Sparplän
      *e ISIN-Wechsel
      *
      *   Rev 2.0   Mar 02 2017 07:16:54   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER Sparplan KSDS der Fondsdepotbank                *
      *                                                                *
      *    SATZLÄNGE   255                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!---------!----*
      *01.03.17! Neu - ersetzt DRKI1935 wegen neuem Key ! tfl    !     *
      *AKI01480!                                        !         !    *
      *--------!----------------------------------------!---------!----*
      * AKI01733!05.11.19! uprsprüngliche Soarrate jetzt  ! WHO      *
      *         !        ! S9(07)V99                      !          *
      *         !        !                                !          *
      *
CCC009*    SKIP1
       01  DRKI3123.
 
               15  KSDS-SA                            PIC  X(001).
      *            SATZART        = 'C'
               15  KSDS-GES                           PIC  9(002).
      *            GESELLSCHAFT   = '40'
               15  KSDS-KEY.
160109             20  KSDS-VNR                       PIC  9(013).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
160109             20  KSDS-UNTER-NR                  PIC  9(002).
      *            Depotunternummer
                   20  KSDS-LFDNR                     PIC  9(004).
      *            laufende Nummer (Eindeutige Nummer - kurz )
               15  KSDS-WKN                           PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               15  KSDS-ISIN                          PIC  X(012).
      *            ISIN, sofern vorhanden
160109         15  KSDS-VERTRAGSTYP                   PIC  X(002).
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
 
               15  KSDS-SPARRATE                      PIC  9(007)V99.
      *            SPARLEISTUNG FUER SPARPLAENE
               15  KSDS-DYNAM                         PIC  9(002)V99.
      *            DYNAMISIERUNG FUER SPARPLAENE in %
               15  KSDS-ZAHLWEISE                     PIC  9(001).
      *            ZAHLWEISE......***0 : ZAHLWEISE FEHLT
      *            ZAHLWEISE........*1 : JAEHRLICH
      *            ZAHLWEISE........*2 : HALBJAEHRLICH
      *            ZAHLWEISE........*4 : VIERTELJAEHRLICH
      *            ZAHLWEISE........*5 : 2-MONATLICH
      *            ZAHLWEISE........*6 : MONATLICH
      *            ZAHLWEISE.........8 : EINMALPRAEMIE
               15  KSDS-TAG                           PIC  9(002).
      *            Tag im Monat für die Einzahlung
               15  KSDS-BUCHSCHL-EIN                  PIC  9(002).
      *            Buchungsschluessel für Einzahlung
               15  KSDS-DATUM-EROEFFNUNG.
                   25  JAHR-EROEFFNUNG               PIC   9(004).
                   25  MONAT-EROEFFNUNG              PIC   9(002).
                   25  TAG-EROEFFNUNG                PIC   9(002).
      *            DATUM SPARPLANERÖFFNUNG JHJJMMTT
               15  KSDS-DATUM-SCHLIESSUNG.
                   25  JAHR-SCHLIESSUNG              PIC   9(004).
                   25  MONAT-SCHLIESSUNG             PIC   9(002).
                   25  TAG-SCHLIESSUNG               PIC   9(002).
      *            DATUM VERTRAGSENDE JHJJMMTT
               15  KSDS-BANKVERBINDUNG                PIC  X(020).
      *            Name des Instituts
230913*        15  KSDS-BLZ                           PIC  9(011).
      *            Bankleitzahl
230913*        15  KSDS-KONTONUMMER                   PIC  9(018).
      *            KONTO-NUMMER
230913*        15  KSDS-ABW-KONTOINHABER              PIC  X(027).
      *            Name des abweichenden Kontoinhabers
230913         15  KSDS-BIC                           PIC  X(011).
      *            BIC
230913         15  KSDS-IBAN                          PIC  X(034).
      *            IBAN
               15  KSDS-DATUM-VERTRAGSENDE.
                   25  JAHR-VERTRAGSENDE             PIC   9(004).
                   25  MONAT-VERTRAGSENDE            PIC   9(002).
                   25  TAG-VERTRAGSENDE              PIC   9(002).
      *            Ende der Einzahlungsfrist bei VL-Verträgen JHJJMMTT
               15  KSDS-BEGINN-RATENAUSSETZUNG        PIC  9(008).
      *            Beginndatum der Ratenaussetzung JHJJMMTT,
      *            wird bei einer erneuten Aussetzung überschrieben
               15  KSDS-ENDE-RATENAUSSETZUNG          PIC  9(008).
      *            Endedatum der Ratenaussetzung JHJJMMTT,
      *            wird bei einer erneuten Aussetzung überschrieben
               15  KSDS-DEL-KZ                        PIC  X(001).
      *            Delete-Kennzeichen
               15  KSDS-KOSTEN-PROZ                   PIC  9(001)V99.
      *            Kundenentgelt in % auf Basis Ursprungsrate
               15  KSDS-URSPR-GES-SPAR-LEIST          PIC  9(006)V99.
      *            Gesamtsparrate auf Basis Ursprungsrate in EUR
               15  KSDS-URSPR-SPAR-RATE               PIC  9(007)V99.
      *            Ursprüngliche Sparrate in EUR
               15  KSDS-AUSG-AUFSCHL-URSPR            PIC  9(002)V99.
      *            Ausgabeaufschlag des ursprünglich gewählten Fonds
               15  KSDS-AUSG-AUFSCHL-AKTUELL          PIC  9(002)V99.
      *            Ausgabeaufschlag des aktuell gewählten Fonds
               15  KSDS-VERTR-LAUFZEIT                PIC  9(002).
      *            Vertragslaufzeit in Jahren
               15  KSDS-BEG-DYNAM                     PIC  9(008).
      *            JHJJMMTT Jahr, ab wann bzw. wieder die
      *            Dynamisierung durchgeführt wird
               15  KSDS-ZAEHLER-LASTSCHR-RUECK        PIC  X(003).
      *          **MD1 = 1. LASTSCHRIFTRÜCKGABE MANGELD DECKUNG
      *          **MD2 = 2. LASTSCHRIFTRÜCKGABE MANGELD DECKUNG
      *          **MD3 = FRISTABLAUF FÜR STORNO WEGEN LASTSCHRIFTRÜCK-
      *          **      GABE MANGELD DECKUNG
      *          **WW1 = 1. LASTSCHRIFTRÜCKGABE WEGEN WIDERSPRUCH
      *          **WW2 = 2. LASTSCHRIFTRÜCKGABE WEGEN WIDERSPRUCH
      *          **WW3 = FRISTABLAUF FÜR STORNO WEGEN LASTSCHRIFT-
      *          **      RÜCKGABE WEGEN WIDERSPRUCH
               15  KSDS-KND-MERKMAL                   PIC  X(001).
      *            K oder leer
               15  KSDS-DAT-VERTR-AUFLOESUNG          PIC  9(008).
      *            DATUM VERTRAGSAUFLÖSUNG  BZW. VORZEITIGE
      *            VERTRAGSKÜNDIGUNG JHJJMMTT
               15  KSDS-DYNAMISIERUNG                 PIC  X(001).
      *            'D' = DYNAMISIERUNG
               15  KSDS-EINDEUTIGE-SPARPL-NR          PIC  9(010).
      *            SPARPLAN-NR, EINDEUTIG ÜBERS GANZE SYSTEM
               15  KSDS-KURZE-SPARPL-NR               PIC  9(004).
      *            4-STELLIGE SPARPLANIDENTIFIKATION
150808         15  KSDS-HIST-DATUM-EROEFFNUNG         PIC  9(008).
150808*            DATUM DER URSPRÜNGL. SPARPLANERÖFFNUNG (AUS DEM
150808*               ALTSYSTEM)-WENN VORHANDEN,DANN GÜLTIG
 
150808*        15  KSDS-RESERVE                       PIC  X(016).
150808*        15  KSDS-RESERVE                       PIC  X(008).
230913         15  KSDS-RESERVE                       PIC  X(016).
      *            RESERVE
 


