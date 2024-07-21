      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3642.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3642.CPv  $
      *
      *   Rev 2.4   Jun 07 2019 08:41:08   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.3   Jun 05 2019 12:05:42   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.2   Jun 05 2019 10:42:04   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.1   Apr 15 2019 11:22:10   t13ex78
      *AKI01669: MDTA, Ermittlung und Aufbereitung
      *
      *   Rev 2.0   Mar 20 2019 09:58:04   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    G67 - DSL Bank                                             *
      *    Vertrags- und Kreditdaten DTA (Kundendaten werden über die *
      *    Tickets Typ 973 angeliefert und in der Ermittlung hinzu-   *
      *    gelesen                                                    *
      *                                                               *
      *    Satzlänge 200 Bytes                                        *
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01669!20.03.19!2.0 ! Neuerstellung               !tfl       *
      *--------!-------------------------------------------!----------*
      *        !        !    !                             !          *
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
      * Absender - 'DSL     '
               10  'XX'-V-ABSENDER                 PIC  X(008).
      *
      * Datum der Erstellung des DTA im Format JJJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *
      * Welche Art Lieferung?
      * Zu Beginn immer nur 'PK-K    ' für Privatkredit Komplett
               10  'XX'-V-LIEFERUNG                PIC  X(008).
      *
      * Reserve
               10  FILLER                          PIC  X(171).
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
      * Gesellschaft - immer '67'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * VB-Nummer
               10  'XX'-VBNR                       PIC  9(007).
      *
      * Eröffnungsdatum im Format JJJJMMTT
               10  'XX'-EROEFFNUNGSDAT             PIC  9(008).
      *
      * Schließungsdatum im Format JJJJMMTT
               10  'XX'-SCHLIESSUNGSDAT            PIC  9(008).
      *
      * Darlehensbetrag
               10  'XX'-DARLEHENSBETRAG            PIC  9(012)V99.
      *
      * Datum der Kreditzusage im Format JJJJMMTT
               10  'XX'-KREDITZUSAGEDAT            PIC  9(008).
      *
      * Datum der Kreditauszahlung im Format JJJJMMTT
               10  'XX'-KREDITAUSZAHLDAT           PIC  9(008).
      *
      * Kreditart
               10  'XX'-KREDITART                  PIC  X(006).
      *
      * aktueller Saldo
               10  'XX'-SALDO                      PIC  9(012)V99.
      *
      * Zinssatz p.a. in %
               10  'XX'-ZINSSATZ                   PIC  9(002)V9(04).
      *
      * Rate in EUR
               10  'XX'-RATE                       PIC  9(012)V99.
      *
      * Datum nächste Fälligkeit im Format JJJJMMTT
               10  'XX'-FAELLIGKEITDAT             PIC  9(008).
      *
      * Sicherheitspaket J/N
               10  'XX'-SICHERHEITSPAKET           PIC  X(001).
      *
      * Restlaufzeit in Monaten
               10  'XX'-RESTLAUFZEIT               PIC  9(003).
      *
      * Deletekennzeichen
      * 'A' - aktiver Kredit
      * 'D' - abgelaufen/storniert
      * 'S' - Info-/Datenschutzsperre
               10  'XX'-DELETEKENNZEICHEN          PIC  X(001).
      *
      * Reserve innerhalb des Blockes Bestandsdaten
               10  FILLER                          PIC  X(072).
 
 
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
               10  FILLER                          PIC  X(191).
 
 
 
 
 
 




