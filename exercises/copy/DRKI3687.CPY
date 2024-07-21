      ******************************************************************
      *                                                                *
      *                                                                *
      * G79 - Geiger Edelmetalle                                       *
      *    Direktkaufdaten - DTA                                       *
      *                                                                *
      *    Satzlänge 441 Bytes                                         *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01692!24.06.19! Neu                              ! t13ex78   *
      *DBO10172!13.12.22!Anpassung, weg. Silberdirektkauf  ! u131774   *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
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
      * Welche Art Lieferung? immer 'D'
               10  'XX'-V-LIEFERUNG                PIC  X(001).
      *
      * Reserve
               10  FILLER                          PIC  X(419).
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
      * Gesellschaft - '79' für Golddirektkauf
      *                '80' für Silberdirektkauf
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *s
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * Buchungsdatum im Format JJJJMMTT
               10  'XX'-BUCHUNGSDATUM              PIC  X(008).
      *
      * VB-Nummer
               10  'XX'-VBNR                       PIC  9(007).
      *
      * Rabattierungskennzeichen - 'R' oder SPACE
               10  'XX'-RABATT                     PIC  X(001).
      *
      * 8-stellige Geiger Kunden-ID, numerisch
               10  'XX'-KUNDENIDEXT                PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(021).
      *
      *====================================================
      * Kundendaten - nur ein VN möglich
      *====================================================
      *
      * Nachname
               10  'XX'-NACHNAME                   PIC  X(030).
      *
      * Vorname
               10  'XX'-VORNAME                    PIC  X(030).
      *
      * Firmenname, wenn vorhanden
               10  'XX'-FIRMENNAME                 PIC  X(070).
      *
      * Geburtsdatum im Format JHJJMMTT
               10  'XX'-GEBDATUM                   PIC  9(008).
      *
      * Anredeschlüssel
      *  1 - Herr
      *  2 - Frau
      *  5 - Firma
               10  'XX'-ANREDE                     PIC  9(001).
      *
      * Anschrift -Straße und Hausnummer
               10  'XX'-STRASSE                    PIC  X(040).
      *
      * Postleitzahl
               10  'XX'-PLZ                        PIC  X(009).
      *
      * Wohnort des Kunden
               10  'XX'-ORT                        PIC  X(030).
      *
      * ISO-Ländercode, bspw. 'DE ' für Deutschland
               10  'XX'-LAND                       PIC  X(003).
      *
      * Emailadresse
               10  'XX'-EMAIL                      PIC  X(040).
      *
      * Reserve
               10  FILLER                          PIC  X(030).
 *
      *===============================
      * Daten zum gekauften Edelmetall
      *===============================
      *
      * Umsatz in EUR
               10  'XX'-UMSATZ                     PIC  9(007)V9(002).
      *
      * Anzahl/Stück
               10  'XX'-ANZAHL                     PIC  9(005).
      *
      * Produkt/Bezeichnung - Freitext
               10  'XX'-BEZEICHNUNG                PIC  X(030).
      *
      * Art des Metalls
      *  1 - Gold
      *  2 - Silber
      *  3 - Platin
      *  4 - Palladium
      *  5 - Kupfer
               10  'XX'-METALL                     PIC  9(001).
      *
      * Menge in Gramm
               10  'XX'-GRAMM                      PIC  9(010)V9(002).
      *
      * Reserve
               10  FILLER                          PIC  X(030).

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
               10  FILLER                          PIC  X(432).


