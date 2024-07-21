      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3286.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3286.CPv  $
      *
      *   Rev 2.1   Jun 11 2018 14:30:30   t13ex27
      *AKI01570: FW - Generali-Partnergesellschaften-E/A/A-Kunden
      *
      *   Rev 2.0   Feb 22 2018 13:31:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01570!21.02.18!2.1 ! neu                       ! IFR
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *   Generali - Kunden-DTA
      *
      *
      *
      *
      *
      *
      *
      *****************************************************************

           05  'XX'-SATZ.

      *  Gesellschaftsnummer DVAG-intern
               10  'XX'-GES                      PIC  9(002).

      *  Vertragsnummer
               10  'XX'-VNR                      PIC  X(015).

      *  Rolle
               10  'XX'-ROLLE                    PIC  X(002).
                  88 'XX'-ROLLE-VN                 VALUE 'VN'.
                  88 'XX'-ROLLE-VP                 VALUE 'VP'.

      *  Einzelproduktnummer
               10  'XX'-EZPNR                    PIC  X(004).

      *  Laufende Nummer für VP
               10  'XX'-LFDNR                    PIC  X(004).

      *  Nachname
               10  'XX'-NACHNAME                 PIC  X(040).

      *  Nachname Teil 2
               10  'XX'-NACHNAME2                PIC  X(040).

      *  Vorname
               10  'XX'-VORNAME                  PIC  X(030).

      *  Geburtsdatum
               10  'XX'-GEBDAT                   PIC  X(008).

      *  Anrede
               10  'XX'-ANREDE                   PIC  X(002).
                  88 'XX'-ANREDE-HERR              VALUE '01'.
                  88 'XX'-ANREDE-FRAU              VALUE '02'.
                  88 'XX'-ANREDE-FRL               VALUE '03'.
                  88 'XX'-ANREDE-EHELEUTE          VALUE '04'.
                  88 'XX'-ANREDE-FIRMA             VALUE '05'.
                  88 'XX'-ANREDE-HERREN            VALUE '06'.
                  88 'XX'-ANREDE-FRAUEN            VALUE '07'.
                  88 'XX'-ANREDE-REST              VALUE '08'.

      *  Geschlecht
               10  'XX'-GESCHLECHT               PIC  X(001).
                  88 'XX'-GESCHLECHT-W             VALUE '2'.
                  88 'XX'-GESCHLECHT-M             VALUE '1'.

      *  Straße/Hausnummer
               10  'XX'-STR                      PIC  X(045).

      *  Postleitzahl
               10  'XX'-PLZ                      PIC  X(015).

      *  Ort
               10  'XX'-ORT                      PIC  X(030).

      *  Länderkennzeichen
               10  'XX'-LKZ                      PIC  X(003).

      *-----------------------------------------------------------------
           05  'XX'-VORSATZ REDEFINES  'XX'-SATZ.

      *  'A' fuer Vorsatz (A-Satz)
               10  'XX'-V-SA                     PIC  X(001).
                  88 'XX'-SATZART-VORSATZ          VALUE 'A'.

      *  Text, welcher den Absender des DTA identifiziert
               10  'XX'-V-ABSENDER               PIC  X(003).

      *  Datum der Erstellung des DTA im Format JHJJMMTT
               10  'XX'-V-DATUM                  PIC  9(008).

      *  je nach Mandant
               10  'XX'-V-EMPFAENGER             PIC  X(004).

      *  Daten
               10  'XX'-V-TEXT                   PIC  X(016).

      *  'K' fuer Komplettbestand; 'D' für Deltalieferung
               10  'XX'-V-KENNZ                  PIC  X(004).

      *-----------------------------------------------------------------
           05  'XX'-NACHSATZ REDEFINES  'XX'-VORSATZ.

      *  'E' für Nachsatz (E-Satz)
               10  'XX'-N-SA                     PIC  X(001).
                  88 'XX'-SATZART-ENDESATZ         VALUE 'E'.

      *  Anzahl der gelieferten Daten-Sätze
               10  'XX'-N-ANZSATZ                PIC  9(009).





