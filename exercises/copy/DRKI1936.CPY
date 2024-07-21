      ******************************************************************
      *                                                                *
      *    COPY-MEMBER ZU DEN AUFBEREITUNGSPROGRAMMEN KI-NEU           *
      *    - VERÄNDERUNG AGI SPARPLANDATEN                             *
      *                                                                *
      *    SATZLÄNGE   350                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME      !VER-*
      *AUFTRAG !                                       !          !SION*
      *--------!---------------------------------------!----------!----*
      *25.07.06!NEUERSTELLUNG                          !Misgeld   ! 01 *
      *AKI00611!                                       !          !    *
      *--------!---------------------------------------!----------!----*
      *08.10.07!DIT ERSETZT DURCH AGI. ZAHLWEISE VON X !SCHLEICHER!2.2 *
      *AKI00611!                       AUF 9 GEÄNDERT  !          !    *
      *        !DIE ALTE ISIN UND KONTOART MITNEHMEN   !          !    *
      *--------!---------------------------------------!----------!----*
160109*AKI00740! Neuer Aufbau Primary Key              ! WHO      !2.4 *
      *16.01.09!                                       !          !    *
      *--------!---------------------------------------!----------!----*
230913*AKI01204! Anpassungen SEPA                      ! SWZ      !2.5 *
      *--------!---------------------------------------!----------!----*
      *AKI01733!05.11.19! uprsprüngliche Soarrate jetzt! WHO      !    *
      *        !        ! S9(07)V99                    !          !    *
      *        !        !                              !          !    *
      *--------!---------------------------------------!----------!----*
160322*DBO-9823!16.03.22!Risikoklasse aufgenommen.     !U131661   !    *
160322*        !        !                              !(B.Huhn)  !    *
250522*--------!---------------------------------------!----------!----*
250522*        !25.05.22!Logzeilen entfernt            ! U131661  !    *
250522*        !                                       !(B.Huhn)  !    *
      *--------!---------------------------------------!----------!----*
CCC009*    SKIP1
       01  DRKI1936.
 
               05  BNK-SA                             PIC  X(001).
      *            SATZART        = 'C'
               05  BNK-GES                            PIC  9(002).
      *            GESELLSCHAFT   = '40'
               05  BNK-KEY.
160109             10  BNK-VNR                        PIC  9(013).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
160109             10  BNK-UNTER-NR                   PIC  9(002).
      *            Depotunternummer
                   10  BNK-LFDNR                      PIC  9(004).
      *            eindeutige identifikationsnummer
               05  BNK-VERARB-KZ                      PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
 
               05  BNK-WKN                            PIC  X(006).
      *            WERTPAPIERKENN-NUMMER
               05  BNK-AEND-WKN-KZ                    PIC  X(001).
      *            J = WERTPAPIERKENN-NUMMER ÄNDERUNG
               05  BNK-ISIN                           PIC  X(012).
      *            ISIN, sofern vorhanden
               05  BNK-AEND-ISIN-KZ                   PIC  X(001).
      *            J = ISIN ÄNDERUNG
               05  BNK-SPARRATE                       PIC  9(007)V99.
      *            SPARLEISTUNG FUER SPARPLAENE
               05  BNK-AEND-SPARRATE-KZ               PIC  X(001).
      *            J = SPARLEISTUNG ÄNDERUNG
               05  BNK-DYNAM                          PIC  9(002)V99.
      *            DYNAMISIERUNG FUER SPARPLAENE in %
               05  BNK-AEND-DYNAM-KZ                  PIC  X(001).
      *            J = DYNAMISIERUNG ÄNDERUNG
               05  BNK-ZAHLWEISE                      PIC  9(001).
      *            ZAHLWEISE......***0 : ZAHLWEISE FEHLT
      *            ZAHLWEISE........*1 : JAEHRLICH
      *            ZAHLWEISE........*2 : HALBJAEHRLICH
      *            ZAHLWEISE........*4 : VIERTELJAEHRLICH
      *            ZAHLWEISE........*5 : 2-MONATLICH
      *            ZAHLWEISE........*6 : MONATLICH
      *            ZAHLWEISE.........8 : EINMALPRAEMIE
               05  BNK-AEND-ZAHLWEISE-KZ              PIC  X(001).
      *            J = ZAHLWEISE ÄNDERUNG
               05  BNK-TAG                            PIC  9(002).
      *            Tag im Monat für die Einzahlung
               05  BNK-AEND-TAG-KZ                    PIC  X(001).
      *            J = EINZAHLUNG ÄNDERUNG
               05  BNK-BUCHSCHL-EIN                   PIC  9(002).
      *            Buchungsschluessel für Einzahlung
               05  BNK-AEND-BUCHSCHL-EIN-KZ           PIC  X(001).
      *            J = BUCHUNGSSCHLUESSEL ÄNDERUNG
               05  BNK-DATUM-EROEFFNUNG.
                   10  JAHR-EROEFFNUNG                PIC  9(004).
                   10  MONAT-EROEFFNUNG               PIC  9(002).
                   10  TAG-EROEFFNUNG                 PIC  9(002).
      *            DATUM SPARPLANERÖFFNUNG JHJJMMTT
               05  BNK-AEND-DATUM-EROEFFNUNG-KZ       PIC  X(001).
      *            J = DATUM VERTRAGSBEGINN ÄNDERUNG
               05  BNK-DATUM-SCHLIESSUNG.
                   10  JAHR-SCHLIESSUNG               PIC  9(004).
                   10  MONAT-SCHLIESSUNG              PIC  9(002).
                   10  TAG-SCHLIESSUNG                PIC  9(002).
      *            DATUM VERTRAGSENDE JHJJMMTT
               05  BNK-AEND-DATUM-SCHLIESSUNG-KZ      PIC  X(001).
      *            J = DATUM VERTRAGSENDE ÄNDERUNG
               05  BNK-BANKVERBINDUNG                 PIC  X(020).
      *            Name des Instituts
               05  BNK-AEND-BANKVERBINDUNG-KZ         PIC  X(001).
      *            J = NAME DES INSTITUTS ÄNDERUNG
230913*        05  BNK-BLZ                            PIC  9(011).
      *            Bankleitzahl
230913*        05  BNK-AEND-BLZ-KZ                    PIC  X(001).
      *            J = BANKLEITZAHL ÄNDERUNG
230913*        05  BNK-KONTONUMMER                    PIC  9(018).
      *            KONTO-NUMMER
230913*        05  BNK-AEND-KONTONUMMER-KZ            PIC  X(001).
      *            J = KONTO-NUMMER ÄNDERUNG
230913*        05  BNK-ABW-KONTOINHABER               PIC  X(027).
      *            Name des abweichenden Kontoinhabers
230913*        05  BNK-AEND-ABW-KONTOINHABER-KZ       PIC  X(001).
      *            J = NAME DES ABWEICHENDEN KONTOINHABERS ÄNDERUNG
230913         05  BNK-BIC                            PIC  X(011).
      *            BIC
230913         05  BNK-AEND-BIC-KZ                    PIC  X(001).
      *            J = BIC  ÄNDERUNG
230913         05  BNK-IBAN                           PIC  X(034).
      *            IBAN
230913         05  BNK-AEND-IBAN-KZ                   PIC  X(001).
      *            J = IBAN ÄNDERUNG
               05  BNK-DATUM-VERTRAGSENDE.
                   10  JAHR-VERTRAGSENDE              PIC  9(004).
                   10  MONAT-VERTRAGSENDE             PIC  9(002).
                   10  TAG-VERTRAGSENDE               PIC  9(002).
      *            Ende der Einzahlungsfrist bei VL-Verträgen JHJJMMTT
               05  BNK-AEND-DATUM-VERTRAGSENDE-KZ     PIC  X(001).
      *            J = ENDE DER EINZAHLUNGSFRIST ÄNDERUNG
               05  BNK-VERTRAGSTYP                    PIC  X(002).
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
               05  BNK-AEND-VERTRAGSTYP-KZ            PIC  X(001).
      *            J = VERTRAGSTYP ÄNDERUNG
               05  BNK-BEGINN-RATENAUSSETZUNG         PIC  9(008).
      *            Beginndatum der Ratenaussetzung JHJJMMTT,
      *            wird bei einer erneuten Aussetzung überschrieben
               05  BNK-AEND-BEG-RATENAUS-KZ           PIC  X(001).
      *            J = BEGINNDATUM DER RATENAUSSETZUNG ÄNDERUNG
               05  BNK-ENDE-RATENAUSSETZUNG           PIC  9(008).
      *            Endedatum der Ratenaussetzung JHJJMMTT,
      *            wird bei einer erneuten Aussetzung überschrieben
               05  BNK-AEND-ENDE-RATENAUS-KZ          PIC  X(001).
      *            J = ENDEDATUM DER RATENAUSSETZUNG ÄNDERUNG
               05  BNK-DEL-KZ                         PIC  X(001).
      *            Delete-Kennzeichen
               05  BNK-AEND-DEL-KZ-KZ                 PIC  X(001).
      *            J = DELETE-KENNZEICHEN ÄNDERUNG
               05  BNK-KOSTEN-PROZ                    PIC  9(001)V99.
      *            Kundenentgelt in % auf Basis Ursprungsrate
               05  BNK-AEND-KOSTEN-PROZ-KZ            PIC  X(001).
      *            J = KUNDENENTGELT ÄNDERUNG
               05  BNK-URSPR-GES-SPAR-LEIST           PIC  9(006)V99.
      *            GESAMTSPARRATE AUF BASIS URSPRUNGSRATE IN EUR
               05  BNK-AEND-URSPR-GES-SPAR-LE-KZ      PIC  X(001).
      *            J = GESAMTSPARRATE ÄNDERUNG
               05  BNK-URSPR-SPAR-RATE                PIC  9(007)V99.
      *            Ursprüngliche Sparrate in EUR
               05  BNK-AEND-URSPR-SPAR-RATE-KZ        PIC  X(001).
      *            J = URSPRÜNGLICHE SPARRATE ÄNDERUNG
               05  BNK-AUSG-AUFSCHL-URSPR             PIC  9(002)V99.
      *            Ausgabeaufschlag des ursprünglich gewählten Fonds
               05  BNK-AEND-AUSG-AUFSCHL-URSP-KZ      PIC  X(001).
      *            J = AUSGABEAUFSCHLAG URSPR. FOND ÄNDERUNG
               05  BNK-AUSG-AUFSCHL-AKTUELL           PIC  9(002)V99.
      *            Ausgabeaufschlag des aktuell gewählten Fonds
               05  BNK-AEND-AUSG-AUFSCHL-AKTU-KZ      PIC  X(001).
      *            J = AUSGABEAUFSCHLAG AKT. FOND ÄNDERUNG
               05  BNK-VERTR-LAUFZEIT                 PIC  9(002).
      *            Vertragslaufzeit in Jahren
               05  BNK-AEND-VERTR-LAUFZEIT-KZ         PIC  X(001).
      *            J = VERTRAGSLAUFZEIT ÄNDERUNG
               05  BNK-BEG-DYNAM                      PIC  9(008).
      *            JHJJMMTT Jahr, ab wann bzw. wieder die
      *            Dynamisierung durchgeführt wird
               05  BNK-AEND-BEG-DYNAM-KZ              PIC  X(001).
      *            J = BEGINN DYNAMISIERUNG ÄNDERUNG
               05  BNK-ZAEHLER-LASTSCHR-RUECK         PIC  X(003).
      *          **MD1 = 1. LASTSCHRIFTRÜCKGABE MANGELD DECKUNG
      *          **MD2 = 2. LASTSCHRIFTRÜCKGABE MANGELD DECKUNG
      *          **MD3 = FRISTABLAUF FÜR STORNO WEGEN LASTSCHRIFTRÜCK-
      *          **      GABE MANGELD DECKUNG
      *          **WW1 = 1. LASTSCHRIFTRÜCKGABE WEGEN WIDERSPRUCH
      *          **WW2 = 2. LASTSCHRIFTRÜCKGABE WEGEN WIDERSPRUCH
      *          **WW3 = FRISTABLAUF FÜR STORNO WEGEN LASTSCHRIFT-
      *          **      RÜCKGABE WEGEN WIDERSPRUCH
               05  BNK-AEND-ZAEHLER-LASTSCHR-KZ       PIC  X(001).
      *            J = LASTSCHRIFT-RÜCKGABE ÄNDERUNG
               05  BNK-KND-MERKMAL                    PIC  X(001).
      *            K oder leer
               05  BNK-AEND-KND-MERKMAL-KZ            PIC  X(001).
      *            J = KÜNDIGUNGS-KZ ÄNDERUNG
               05  BNK-DAT-VERTR-AUFLOESUNG           PIC  9(008).
      *            Datum Vertragsauflösung  bzw. vorzeitige
      *            Vertragskündigung JHJJMMTT
               05  BNK-AEND-DAT-VERTR-AUFLOES-KZ      PIC  X(001).
      *            J = DATUM VORZ. KÜNDIGUNG ÄNDERUNG
               05  BNK-DYNAMISIERUNG                  PIC  X(001).
      *            'D' = Dynamisierung
               05  BNK-AEND-DYNAMISIERUNG-KZ          PIC  X(001).
      *            J = DYNAMISIERUNG ÄNDERUNG
               05  BNK-ISIN-ALT                       PIC  X(012).
      *            ALTE ISIN UM SATZ GGF LÖSCHEN ZU KÖNNEN.
               05  BNK-WKN-ALT                        PIC  X(006).
      *            ALTE WERTPAPIERKENNUMMER (AGI BRAUCHT SIE NOCH)
               05  BNK-VERTRAGSTYP-ALT                PIC  X(002).
      *            ALTER VERTRAGSTYP UM SATZ GGF LÖSCHEN ZU KÖNNEN.
               05  BNK-LETZT-AENDDAT                  PIC  9(008).
      *            Datum der letzten Änderung (dat aus vorlaufsatz)
160322         05  BNK-RISIKOKLASSE                   PIC  9(001).
160322*            Risikoklasse 0 - 9                  
      *
 
230913*        05  BNK-RESERVE                        PIC  X(067).
230913*        05  BNK-RESERVE                        PIC  X(076).
160322         05  BNK-RESERVE                        PIC  X(075).
      *            RESERVE
 
 


