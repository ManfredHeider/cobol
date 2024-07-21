      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3643.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3643.CPv  $
      *
      *   Rev 2.4   Jun 07 2019 08:41:46   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.3   Jun 05 2019 12:06:42   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.2   Jun 05 2019 10:43:06   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.1   Apr 15 2019 11:22:22   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.0   Mar 20 2019 09:58:18   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    G67 - DSL Bank                                             *
      *    Ausgabedatei mit den ermittelten Änderungen                *
      *                                                               *
      *    Satzlänge 700 Bytes                                        *
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01669!27.03.19!2.0 ! Neu                         !tfl       *
      *--------!-------------------------------------------!----------*
      *        !        !    !                             !          *
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
           05  'XX'-AEND-AKTU-KZ                   PIC  X(001).
      *
      * Gesellschaft - immer '67'
           05  'XX'-AEND-GES                       PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           05  'XX'-AEND-VNR                       PIC  X(015).
      *
      * VB-Nummer
           05  'XX'-AEND-VBNR                      PIC  9(007).
      *
      *====================================================
      * Daten für TCKI0357 / VERTRAG
      *====================================================
      * Vertragszustand
      *   'A' - aktiv
      *   'D' - abgelaufen/storniert
      *   'S' - Info- / Datenschutzsperre
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
      *====================================================
      * Daten für TCKI0465 / BANKPRODUKT
      *====================================================
      *
      * Eröffnungsdatum -> BKPDATEROFFNUNG
           05  'XX'-AEND-EROEFFNUNGSDAT            PIC  9(008).
           05  'XX'-AEND-EROEFFDAT-KZN             PIC  X(001).
      *
      * Schließungsdatum -> BKPDATSCHLIESS
           05  'XX'-AEND-SCHLIESSUNGDAT            PIC  9(008).
           05  'XX'-AEND-SCHLIESSDAT-KZN           PIC  X(001).
      *
      * Darlehensbetrag  -> BKPDARLBETRAG
           05  'XX'-AEND-DARLBETRAG                PIC  9(012)V99.
           05  'XX'-AEND-DARLBETR-KZN              PIC  X(001).
      *
      * Kreditzusagedatum -> BKPDATKREDITZUS
           05  'XX'-AEND-ZUSAGEDAT                 PIC  9(008).
           05  'XX'-AEND-ZUSAGEDAT-KZN             PIC  X(001).
      *
      * Kreditauszahlungsdatum -> BKPDATKREDITAUSZ
           05  'XX'-AEND-AUSZAHLDAT                PIC  X(008).
           05  'XX'-AEND-AUSZAHLDAT-KZN            PIC  X(001).
      *
      * Kreditart -> BKPKREDITART
           05  'XX'-AEND-KREDITART                 PIC  X(006).
           05  'XX'-AEND-KREDITART-KZN             PIC  X(001).
      *
      * Saldo -> BKPSALDOBETRAKT
           05  'XX'-AEND-SALDO                     PIC  9(012)V99.
           05  'XX'-AEND-SALDO-KZN                 PIC  X(001).
      *
      * Zinssatz -> BKPSOLLZINS
           05  'XX'-AEND-ZINSSATZ                  PIC  9(002)V9(004).
           05  'XX'-AEND-ZINSSATZ-KZN              PIC  X(001).
      *
      * Rate -> BKPRATENHOEHE
           05  'XX'-AEND-RATE                      PIC  9(012)V99.
           05  'XX'-AEND-RATE-KZN                  PIC  X(001).
      *
      * Datum der nächsten Fälligkeit -> BKPDATFAELLIG
           05  'XX'-AEND-FAELLIGKEIT               PIC  9(008).
           05  'XX'-AEND-FAELLIGKEIT-KZN           PIC  X(001).
      *
      * Sicherheitspaket -> BKPSICHPAKET
           05  'XX'-AEND-SICHERHEIT                PIC  X(001).
           05  'XX'-AEND-SICHERHEIT-KZN            PIC  X(001).
      *
      * Restlaufzeit in Monaten -> BKPRESTLAUFZEIT
           05  'XX'-AEND-RESTLAUFZEIT              PIC  9(003).
           05  'XX'-AEND-RESTLAUFZEIT-KZN          PIC  X(001).
 
      *
      * Reserve
           05  FILLER                              PIC  X(103).
 
 
 
 
 




