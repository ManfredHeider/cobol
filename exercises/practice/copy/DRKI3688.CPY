      *****************************************************************
      *                                                               *
      *                                                               *
      * G79 - Geiger Edelmetalle                                      *
      *    Direktkaufdaten - Ausgabe ermittelte Änderungen            *
      *                                                               *
      *    Satzlänge  500 Bytes                                       *
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01692!24.06.19!Neu                               ! t13ex78  *
      *DBO10172!13.12.22!Anpassung, weg. Silberdirektkauf  ! u131774  *
      *        !        !                                  !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01  'XX'-AEND-SATZ.
      *
      * Aktualisierungskennzeichen - gilt nur für die Kundendaten
      * Der eigentliche Direktkauf ist immer ein Insert
      * 'I' - Insert
      * 'U' - Update
      *
           05  'XX'-AEND-AKTU-KZ           PIC  X(001).
      *
      * Gesellschaft - '79' für Golddirektkauf
      *                '80' für Silberdirektkauf
           05  'XX'-AEND-GES               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           05  'XX'-AEND-VNR               PIC  X(015).
      *
      * VB-Nummer
           05  'XX'-AEND-VBNR              PIC  9(007).
      *
      *====================================================
      * Daten für TCKI0357 / VERTRAG
      *====================================================
      *
      * Vertragsbeginndatum im Format JHJJMMTT
      * Buchungsdatum im DTA (DRKI3687)
           05  'XX'-AEND-BEGINNDAT         PIC  9(008).
           05  'XX'-AEND-BEGINNDAT-KZN     PIC  X(001).
      *
      *====================================================
      * Daten für TCKI0022 / KUNDE
      *====================================================
      *
      * Nachname des Kunden
           05  'XX'-AEND-NACHNAME          PIC  X(030).
           05  'XX'-AEND-NACHNAME-KZN      PIC  X(001).
      *
      * Vorname des Kunden
           05  'XX'-AEND-VORNAME           PIC  X(030).
           05  'XX'-AEND-VORNAME-KZN       PIC  X(001).
      *
      * Nachname des Kunden
           05  'XX'-AEND-NACHNAME-ALT      PIC  X(030).
           05  'XX'-AEND-NACHNAME-ALT-KZN  PIC  X(001).
      *
      * Vorname des Kunden
           05  'XX'-AEND-VORNAME-ALT       PIC  X(030).
           05  'XX'-AEND-VORNAME-ALT-KZN   PIC  X(001).
      *
      * Firmenname - falls vorhanden
           05  'XX'-AEND-FIRMENNAME        PIC  X(070).
           05  'XX'-AEND-FIRMENNAME-KZN    PIC  X(001).
      *
      * Geburtsdatum im Format JHJJMMTT
           05  'XX'-AEND-GEBDAT            PIC  9(008).
           05  'XX'-AEND-GEBDAT-KZN        PIC  X(001).
      *
      * Anredeschlüssel
      *  1 - Herr
      *  2 - Frau
      *  5 - Firma
           05  'XX'-AEND-ANREDE            PIC  9(001).
           05  'XX'-AEND-ANREDE-KZN        PIC  X(001).
      *
      * Anschrift -Straße und Hausnummer
           05  'XX'-AEND-STRHNR            PIC  X(040).
           05  'XX'-AEND-STRHNR-KZN        PIC  X(001).
      *
      * Postleitzahl
           05  'XX'-AEND-PLZ               PIC  X(009).
           05  'XX'-AEND-PLZ-KZN           PIC  X(001).
      *
      * Wohnort des Kunden
           05  'XX'-AEND-ORT               PIC  X(030).
           05  'XX'-AEND-ORT-KZN           PIC  X(001).
      *
      * ISO-Ländercode, bspw. 'DE ' für Deutschland
           05  'XX'-AEND-LAND              PIC  X(003).
           05  'XX'-AEND-LAND-KZN          PIC  X(001).

      *
      *====================================================
      * Daten für TCKI0481 / DIREKTKAUF
      * immer Inserts, daher kein Aktu-Kennzeichen
      *====================================================
      *
      * Umsatz in EUR -> DIRUMSATZ
           05  'XX'-AEND-UMSATZ            PIC  9(007)V9(002).
      *
      * Stückzahl - DIRSTUECKZAHL
           05  'XX'-AEND-ANZAHL            PIC  9(005).
      *
      * Produktbezeichnung - DIRPRODBEZ
           05  'XX'-AEND-PRODBEZ           PIC  X(030).
      *
      * Art des Metalls - DIRMETALL
           05  'XX'-AEND-METALL            PIC  X(001).
      *
      * Menge in Gramm - DIRGRAMM
           05  'XX'-AEND-GRAMM             PIC  9(010)V9(002).
      *
      * Emailadresse - DIREMAIL
           05  'XX'-AEND-EMAIL             PIC  X(040).
      *
      * Rabattierungskennzeichen
           05  'XX'-AEND-RABATT            PIC  X(001).
      *
      *====================================================
      * Daten für TCKI0559/ GEIGERVERTRZUSATZ
      *====================================================
      *
      * --> GVZKUNDENIDEXT - TCKI0559
           05  'XX'-AEND-KUNDENIDEXT               PIC  9(008).         
           05  'XX'-AEND-KUNDENIDEXT-KZN           PIC  X(001). 
      *
      * Reserve
           05  FILLER                      PIC  X(067).


