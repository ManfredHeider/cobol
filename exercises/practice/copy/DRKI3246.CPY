      *****************************************************************
      *                                                               *
      *    G79 - Geiger Edelmetalle                                   *
      *    Bestandsdaten - DTA                                        *
      *                                                               *
      *    Satzlänge 749 Bytes                                        *
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01565!24.01.18!      Neu                         !tfl       *
      *--------!-------------------------------------------!----------*
      *AKI01638!16.10.18!      NEUES FELD: RABATT          !SCHUMACHER*
      *DBO10080!10.08.22!neue Datenfelder für Geiger       !u131774   *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01  'XX'-DTA-SATZ.
      *
      *****************************************************************
      * Vorsatz
      *****************************************************************
      *
           05  'XX'-VORSATZ.
      *
      * Satzart - immer 'A'
               10  'XX'-V-SA                       PIC  X(001).
      *
      * Absender - 'GEIGER  '
               10  'XX'-V-ABSENDER                 PIC  X(008).
      *
      * Datum der Erstellung des DTA im Format  JHJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *
      * Welche Art Lieferung? 'D' - Delta oder 'K' - Komplett
               10  'XX'-V-LIEFERUNG                PIC  X(001).
      *
      * Reserve
               10  FILLER                          PIC  X(727).
      *
      *****************************************************************
      * Hauptsatz
      *****************************************************************
      *
           05  'XX'-HAUPTSATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'C'
               10  'XX'-SA                         PIC  X(001).
      *
      * Gesellschaft - immer '79'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * Bezeichnung des verkauften Produktes in Form einer Fake ISIN
      * bspw. 'GEIGERG00050' für den 50g Goldbarren
               10  'XX'-PRODUKT                    PIC  X(012).
      *
      * Vertragszustand
      *   'A' - aktiv
      *   'D' - abgelaufen/storniert
               10  'XX'-VERTRAGSZUSTAND            PIC  X(001).
      *
      * Vertragsbeginndatum im Format JHJJMMTT
               10  'XX'-BEGINNDATUM                PIC  9(008).
      *
      * Vertragsendedatum im Format JHJJMMTT
               10  'XX'-ENDEDATUM                  PIC  9(008).
      *
      * VB-Nummer
               10  'XX'-VBNR                       PIC  9(007).
      *
      * RABATTMERKMAL
      * SPACE: KEIN RABATT AUF VERTRAG
      * 'R'  : VERTRAG IST RABATTFÄHIG
               10  'XX'-RABATT                     PIC  X(001).
      *
      * Rabatt auf die Vermittlerprovision für Kunden
      *  '0' = 0 %
      *  '1' = 25 %
      *  '2' = 50 %
      *  '3' = 75 %
      *  '4' = 100 %
               10  'XX'-KNDRABATT                  PIC  X(001).
      *
      * 8-stellige Geiger Kunden-ID, numerisch
               10  'XX'-KUNDENIDEXT                PIC  9(008).
      *
      * Flatrate
      *  '0' = Einzelvertrag zu dieser Kunden-ID
      *  '1' = "Depot-Flatrate" zu dieser Kunden-ID
               10  'XX'-FLATRATE                   PIC  X(001).         
      *
      * Reserve
               10  FILLER                          PIC  X(019).
      *
      *====================================================
      * kundenspezifische Daten  - bis zu 2 Vertragsinhaber
      *====================================================
      *
               10  'XX'-BEIDEKUNDEN.
      *
                   15  FILLER   OCCURS 2.
                     20  'XX'-KUNDENDATEN.
      *
      * Nachname des Kunden
                       25  'XX'-NACHNAME            PIC  X(030).
      *
      * Vorname des Kunden
                       25  'XX'-VORNAME             PIC  X(030).
      *
      * Geburtsdatum im Format JHJJMMTT
                       25  'XX'-GEBDATUM            PIC  9(008).
      *
      * Anredeschlüssel
      *  1 - Herr
      *  2 - Frau
      *  5 - Firma
      *  9 - unbekannt
                       25  'XX'-ANREDE             PIC  9(001).
      *
      * Anschrift -Straße und Hausnummer
                       25  'XX'-STRASSE            PIC  X(040).
      *
      * Postleitzahl
                       25  'XX'-PLZ                PIC  X(009).
      *
      * Wohnort des Kunden
                       25  'XX'-ORT                PIC  X(030).
      *
      * ISO-Ländercode, bspw. 'DE ' für Deutschland
                       25  'XX'-LAND               PIC  X(003).
      *
      * Emailadresse
                       25  'XX'-EMAIL              PIC  X(040).
      *
      * Reserve innerhalb des Blockes Kundendaten
      * Nicht innerhalb der obigen 20er Stufe, da im PMKI1370
      * über die "length of" Funktion die Länge des sich daraus
      * ergebenden VARCHAR ermittelt wird.
                     20 FILLER                     PIC  X(030).
 
      *
      *==========================
      * sparplanspezifische Daten
      *==========================
      *
      * Sparrate
               10  'XX'-SPARRATE                   PIC  9(007)V9(002).
      *
      * Zahlweise
      *  0 - Zahlweise fehlt
      *  1 - jährlich
      *  2 - halbjährlich
      *  4 - vierteljährlich
      *  6 - monatlich
               10  'XX'-ZAHLWEISE                  PIC  9(001).
      *
      * Abbuchungstermin
      *  0 - kein Sparplan läuft
      *  1 - 01. eines Monats
      *  2 - 08. eines Monats
      *  3 - 15. eines Monats
      *  4 - 22. eines Monats
               10  'XX'-TERMIN                     PIC  9(001).
      *
      * IBAN des Abbuchungskontos
               10  'XX'-IBAN                       PIC  X(034).
      *
      * BIC des Abbuchungskontos
               10  'XX'-BIC                        PIC  X(012).
      *
      * Vor- und Nachname des Kontoinhabers
               10  'XX'-ABWNAME                    PIC  X(040).
      *
      * Rate Beginn (Ein / Auszahlung) im Format JHJJMMTT               
               10  'XX'-RATEOEFFDAT                PIC  9(008). 
      *        
      * Endedatum der Aussetzung im Fomat JHJJMMTT
      *
               10  'XX'-ENDEAUSSETZUNG             PIC  9(008).
      *
      * Schließungsdatum des Sparplans im Format JHJJMMTT
               10  'XX'-RATESCHLDAT                PIC  9(008).
      *
      * Dynamik in %
               10  'XX'-DYNAMIK                    PIC  9(003).
      *
      * Sparrate-ID (eindeutiger 8-stelliger Schlüssel von Geiger
      * für einen Sparplan)
      *
               10  'XX'-SPARRATEID                 PIC  9(008).
      *
      * Reserve innerhalb des Blockes Sparplandaten
               10  FILLER                          PIC  X(011).
      *
      *===============================
      * bestandsdatenspezifische Daten
      *===============================
      *
      * Gramm Gold im Depot
               10  'XX'-BESTANDGRAMM               PIC  9(007)V9(004).
      *
      * Depotwert in EUR
               10  'XX'-BESTANDEURO                PIC  9(007)V9(002).
      *
      * Datum der letzten Buchung im Format JHJJMMTT
               10  'XX'-BESTANDDAT                 PIC  9(008).
      *
      * Rücknahmepreis für 1g Gold/Silber/etc.
               10  'XX'-1GRAMM                     PIC  9(004)V9(002).
      *
      * kum. Einzahlungen
               10  'XX'-SUMMEEINZ                  PIC  9(007)V9(002).
      *
      * kum. Auszahlungen
               10  'XX'-SUMMEAUSZ                  PIC  9(007)V9(002).
      *
      * Anzahl der Auslieferungen dieses Artikels gesamt
      *
      * WHEN PRODUKT = 'GEIGERG00050', dann sind 50 g Premiumgoldbarren 
      * gemeint
      * WHEN PRODUKT = 'GEIGERS05000', dann sind 5 kg 
      * Premiumsilberbarren Bundesrepublik Deutschland gemeint
      *
               10  'XX'-KUMAUSLIEF1                PIC  9(005).
      *
      * Anzahl der Auslieferungen dieses Artikels gesamt
      * WHEN PRODUKT = 'GEIGERG00050', dann sind 1 oz Gold Arche Noah 
      * gemeint                                                         
      * WHEN PRODUKT = 'GEIGERS05000', dann sind 5 kg 
      * Premiumsilberbarren Zollfreilager gemeint                       
      *
               10  'XX'-KUMAUSLIEF2                PIC  9(005).
      *
      * Anzahl der Auslieferungen dieses Artikels gesamt
      * WHEN PRODUKT = 'GEIGERG00050', dann ist dieses Datenfeld nicht 
      * gefüllt.
      * WHEN PRODUKT = 'GEIGERS05000', dann sind 1 oz Silber Arche Noah
      * gemeint.
      *
               10  'XX'-KUMAUSLIEF3                PIC  9(005).
      *
      * Reserve innerhalb des Blockes Bestandsdaten
               10  FILLER                          PIC  X(013).
      *
      *****************************************************************
      * Endesatz
      *****************************************************************
      *
           05  'XX'-ENDESATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'E'
               10  'XX'-N-SA                       PIC  X(001).
      *
      * Satzanzahl - gelieferte C-Sätze
               10  'XX'-N-ANZAHL                   PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(740).