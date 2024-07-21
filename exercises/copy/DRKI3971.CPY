      *****************************************************************
      *    COPY-MEMBER: DRKI3971                                      *
      *                                                               *
      *    G79 BESTANDSDATEN GOLDRENTE (AUSGABEDATEI ERM/AUFB)        *
      *                                (EINGABEDATEI AKTUALISIERUNG)  *
      *                                                               *
      *    Satzlänge 850 Bytes                                        *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-10295  !02.09.22! NEUERSTELLUNG                ! ATAR     *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01  'XX'-AEND-SATZ.
      *
      * Aktualisierungskennzeichen
      * 'I' - Insert
      * 'U' - Update
      * 'D' - Delete
      *
           05  'XX'-AEND-AKTU-KNZ                  PIC  X(001).
      *
      * Gesellschaft - immer '79'
           05  'XX'-AEND-GES                       PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           05  'XX'-AEND-VNR                       PIC  X(015).
      *
      * Bezeichnung des verkauften Produktes in Form einer Fake ISIN
      * bspw. 'GEIGERG05000' für GOLDRENTE
           05  'XX'-AEND-PRODUKT                   PIC  X(012).

      *
      *====================================================
      * Daten für TCKI0357 / VERTRAG
      *====================================================
      * Vertragszustand
      *   'A' - aktiv
      *   'D' - abgelaufen/storniert
           05  'XX'-AEND-VTRZUSTAND                PIC  X(001).
           05  'XX'-AEND-VTRZUSTAND-KZN            PIC  X(001).
      *
      * Vertragsbeginndatum im Format JHJJMMTT
           05  'XX'-AEND-BEGINNDAT                 PIC  9(008).
           05  'XX'-AEND-BEGINNDAT-KZN             PIC  X(001).
      *
      * Vertragsendedatum im Format JHJJMMTT
           05  'XX'-AEND-ENDEDAT                   PIC  9(008).
           05  'XX'-AEND-ENDEDAT-KZN               PIC  X(001).
 
      * VB-Nummer
           05  'XX'-AEND-VBNR                      PIC  9(007).
      *
      *====================================================
      * Daten für TCKI0022 / KUNDE
      *====================================================
      *
           05  'XX'-AEND-BEIDEKUNDEN.
               10  'XX'-AEND-KDE    OCCURS 2.
      *
      * Nachname des Kunden
                   15  'XX'-AEND-NACHNAME          PIC  X(030).
                   15  'XX'-AEND-NACHNAME-KZN      PIC  X(001).
      *
      * Vorname des Kunden
                   15  'XX'-AEND-VORNAME           PIC  X(030).
                   15  'XX'-AEND-VORNAME-KZN       PIC  X(001).
      *
      * Nachname des Kunden
                   15  'XX'-AEND-NACHNAME-ALT      PIC  X(030).
                   15  'XX'-AEND-NACHNAME-ALT-KZN  PIC  X(001).
      *
      * Vorname des Kunden
                   15  'XX'-AEND-VORNAME-ALT       PIC  X(030).
                   15  'XX'-AEND-VORNAME-ALT-KZN   PIC  X(001).
      *
      * Geburtsdatum im Format JHJJMMTT
                   15  'XX'-AEND-GEBDAT            PIC  9(008).
                   15  'XX'-AEND-GEBDAT-KZN        PIC  X(001).
      *
      * Anredeschlüssel
      *  1 - Herr
      *  2 - Frau
      *  5 - Firma
                   15  'XX'-AEND-ANREDE            PIC  9(001).
                   15  'XX'-AEND-ANREDE-KZN        PIC  X(001).
      *
      * Anschrift -Straße und Hausnummer
                   15  'XX'-AEND-STRHNR            PIC  X(040).
                   15  'XX'-AEND-STRHNR-KZN        PIC  X(001).
      *
      * Postleitzahl
                   15  'XX'-AEND-PLZ               PIC  X(009).
                   15  'XX'-AEND-PLZ-KZN           PIC  X(001).
      *
      * Wohnort des Kunden
                   15  'XX'-AEND-ORT               PIC  X(030).
                   15  'XX'-AEND-ORT-KZN           PIC  X(001).
      *
      * ISO-Ländercode, bspw. 'DE ' für Deutschland
                   15  'XX'-AEND-LAND              PIC  X(003).
                   15  'XX'-AEND-LAND-KZN          PIC  X(001).
      *
      * Emailadresse  -> geht nach TCKI0315 / GUTHABENPRODUKT
                   15  'XX'-AEND-EMAIL             PIC  X(040).
                   15  'XX'-AEND-EMAIL-KZN         PIC  X(001).

      *
      *==========================
      * Sparplanspezifische Daten
      *==========================
      *
      * Sparrate   - FOZZAHLUNG - TCKI0316
           05  'XX'-AEND-SPARRATE                  PIC  9(007)V9(002).
           05  'XX'-AEND-SPARRATE-KZN              PIC  X(001).
      *
      * Zahlweise  -> FOZZAHLUNGSWEISEID - TCKI0316
      *  0 - Zahlweise fehlt
      *  1 - jährlich
      *  2 - halbjährlich
      *  4 - vierteljährlich
      *  6 - monatlich
           05  'XX'-AEND-ZAHLWEISE                 PIC  9(001).
           05  'XX'-AEND-ZAHLWEISE-KZN             PIC  X(001).
      *
      * Abbuchungstermin -> FOZEINSAUSDATUM - TCKI0316
      *  0 - kein Sparplan läuft
      *  1 - 01. eines Monats
      *  2 - 08. eines Monats
      *  3 - 15. eines Monats
      *  4 - 22. eines Monats
           05  'XX'-AEND-TERMIN                    PIC  9(001).
           05  'XX'-AEND-TERMIN-KZN                PIC  X(001).
      *
      * IBAN des Abbuchungskontos -> BZSIBAN - TCKI0459
           05  'XX'-AEND-IBAN                      PIC  X(034).
           05  'XX'-AEND-IBAN-KZN                  PIC  X(001).
      *
      * BIC des Abbuchungskontos -> BZSBIC   - TCKI0459
           05  'XX'-AEND-BIC                       PIC  X(012).
           05  'XX'-AEND-BIC-KZN                   PIC  X(001).
      *
      * Vor- und Nachname des Kontoinhabers
      * Wenn gefüllt nach BZSNACHMAME und dann ein 'J' nach
      * BZSABWBTGZAHLER
           05  'XX'-AEND-ABWNAME                   PIC  X(040).
           05  'XX'-AEND-ABWNAME-KZN               PIC  X(001).

      * Rate Beginn im Format JJJJMMTT -> TCKI0316 FOZDATBEGINNAUS
           05  'XX'-AEND-RATEOEFFDAT               PIC  9(008).
           05  'XX'-AEND-RATEOEFFDAT-KZN           PIC  X(001).
      *
      * Endedatum der Aussetzung im Fomat JHJJMMTT
      * FOZDATENDEAUS - TCKI0316
           05  'XX'-AEND-ENDEAUS                   PIC  9(008).
           05  'XX'-AEND-ENDEAUS-KZN               PIC  X(001).

      * Rate Ende im Format JJJJMMTT -> TCKI0316 FOZDATENDEAUS
           05  'XX'-AEND-RATESCHLDAT               PIC  9(008).
           05  'XX'-AEND-RATESCHLDAT-KZN           PIC  X(001).

      * Dynamik in %  -> FOZPROZSATZDYNAMIS - TCKI0316
      *
           05  'XX'-AEND-DYNAMIK                   PIC  9(003)V99.
           05  'XX'-AEND-DYNAMIK-KZN               PIC  X(001).

      * Sparraten LFDNR 
      * Hier steht die LFDNR, die selber ermittelt wird. 
      * TCKI0316 Teilschlüssel von FOZLFDNR
      * Die Sparraten-ID vom PG steht nur in der TT.
           05  'XX'-AEND-SPARRATELFDNR             PIC  9(005).
      *
      * RABATTMERKMAL
      * FOZVERTRAGSTYP - TCKI0316
           05  'XX'-AEND-RABATT                    PIC  X(001).
           05  'XX'-AEND-RABATT-KZN                PIC  X(001).

      *===============================
      * Bestandsdatenspezifische Daten
      *===============================
      *
      * Gramm Gold im Depot -> GHAGUTHABENANT - TCKI0313
           05  'XX'-AEND-BESTGRAMM                 PIC  9(007)V9(004).
           05  'XX'-AEND-BESTGRAMM-KZN             PIC  X(001).
      *
      * Depotwert in EUR -> GHAGUTHABEN - TCKI0313
           05  'XX'-AEND-BESTEURO                  PIC  9(007)V9(002).
           05  'XX'-AEND-BESTEURO-KZN              PIC  X(001).
      *
      * Datum der letzten Buchung im Format JHJJMMTT
      * GHALIEFERDATUM - TCKI0313
           05  'XX'-AEND-BESTDATUM                 PIC  9(008).
           05  'XX'-AEND-BESTDATUM-KZN             PIC  X(001).
      *
      * Rücknahmepreis für 1g Gold -> GHARUECKNAHMEPR - TCKI0313
           05  'XX'-AEND-1GRAMMGOLD                PIC  9(004)V9(002).
           05  'XX'-AEND-1GRAMMGOLD-KZN            PIC  X(001).
      *
      * kum. Einzahlungen  -> GHAKUMULEINZAHLUNG - TCKI0313
           05  'XX'-AEND-SUMMEEINZ                 PIC  9(007)V9(002).
           05  'XX'-AEND-SUMMEEINZ-KZN             PIC  X(001).
      *
      * kum. Auszahlungen  -> GHAKUMULAUSZAHLUNG - TCKI0313
           05  'XX'-AEND-SUMMEAUSZ                 PIC  9(007)V9(002).
           05  'XX'-AEND-SUMMEAUSZ-KZN             PIC  X(001).
      *
      *==============================================================
      * Datenfelder die in die Zusatztabelle GEIGERVERTRZUSATZ kommen
      *==============================================================
      * Kunden-Rabatt auf die Vermittlerprovision für Kunden in %
      * '0' = 0 %
      * '1' = 25 %
      * '2' = 50 % 
      * '3' = 75 %
      * '4' = 100 %
           05  'XX'-AEND-KNDRABATT                 PIC  X(001).
           05  'XX'-AEND-KNDRABATT-KZN             PIC  X(001).
      * Kunden ID vom PG
           05  'XX'-AEND-KUNDENIDEXT               PIC  9(008).
           05  'XX'-AEND-KUNDENIDEXT-KZN           PIC  X(001).
      * Flatrate
      * '0' = Einzelvertrag    zu der Kunden-ID
      * '1' = "Depot-Flatrate" zu der Kunden-ID
           05  'XX'-AEND-FLATRATE                  PIC  X(001).
           05  'XX'-AEND-FLATRATE-KZN              PIC  X(001).
      *
      * Reserve
           05  FILLER                              PIC  X(055).
      *****************************************************************
      *---------------------- ENDE DRKI3971 --------------------------*
      *****************************************************************