      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  01.09.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3846.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3846.CPv  $
      *
      *   Rev 2.3   Oct 06 2020 17:00:44   t13ex78
      *AKI01773: Ermittlung Kunden-DTA
      *
      *   Rev 2.2   Sep 10 2020 07:55:48   t13ex78
      *AKI01773: Ermittlung Kunden-DTA
      *
      *   Rev 2.1   Sep 08 2020 17:17:12   t13ex78
      *AKI01773: Ermittlung Kunden-DTA
      *
      *   Rev 2.0   Sep 01 2020 11:46:56   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      * G90 - Compeon                                                  *
      *    Kundendaten - DTA                                           *
      *                                                                *
      *    Satzlänge 1000 Bytes                                        *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01773!01.09.20!2.0 ! Neu                         ! t13ex78   *
      *        !        !    !                             !           *
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
      * Absender - '90'
               10  'XX'-V-ABSENDER                 PIC  X(002).
      *
      * Datum der Erstellung des DTA im Format  JHJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(010).
      *
      * Welche Inhalt hat die Lieferung?
      * 'KUNDENDATEN' - Komplettlieferung
               10  'XX'-V-DTA-INHALT               PIC  X(020).
      *
      * K - Komplett
      * D - Delta
               10  'XX'-V-K-ODER-D                 PIC  X(001).
      *
      * Reserve
               10  FILLER                          PIC  X(958).
      *
      *****************************************************************
      * Hauptsatz
      *****************************************************************
      *
           05  'XX'-HAUPTSATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'D'
               10  'XX'-SA                         PIC  X(001).
      *
      * Gesellschaft - immer '90'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * KI-Kundennummer - wird nicht verarbeitet
               10  FILLER                          PIC  X(015).
      *
      * Rechtsform
      *  0 - unbekannt
      *  1 - Einzelunternehmen
      *  2 - GbR
      *  3 - GmbH
      *  4 - GmbH & Co. KG
      *  5 - OHG
      *  6 - KG
      *  7 - KGaA
      *  8 - AG
      *  9 - PartGG
      * 10 - eG
      * 11 - Stiftung
      * 12 - e.V.
      * 13 - nicht eingetragener Verein
      * 14 - WEG
      * 15 - Sonstige
      * 16 - UG
               10  'XX'-RECHTSFORM                 PIC  9(002).
      *
      * Anrede
      * 0 - unbekannt
      * 1 - Herr
      * 2 - Frau
      * 4 - Eheeleute
      * 5 - Firma
               10  'XX'-ANREDE                     PIC  9(001).
      *
      * Firmenvorname
               10  'XX'-FIRMENVORNAME              PIC  X(030).
      *
      * Firmenname
               10  'XX'-FIRMENNAME                 PIC  X(030).
      *
      * Gründungsdatum des Unternehmens im Format JHJJMMTT
               10  'XX'-GRUENDUNGSDATUM            PIC  9(008).
      *
      * Firmeninhaber
               10  'XX'-FIRMENINHABER              PIC  X(100).
      *
      * Straße und Hausnummer
               10  'XX'-STRHNR                     PIC  X(040).
      *
      * Postleitzahl
               10  'XX'-PLZ                        PIC  9(009).
      *
      * Ort
               10  'XX'-ORT                        PIC  X(040).
      *
      * Postfach abw. Postfachanschrift
               10  'XX'-POSTFACHABWADR             PIC  9(009).
      *
      * Postleitzahl abw. Postfachanschrift
               10  'XX'-PLZABWADR                  PIC  9(009).
      *
      * Ort abw. Postfachanschrift
               10  'XX'-ORTABWADR                  PIC  X(040).
      *
      * Telefonnummer der Firma
               10  'XX'-TELEFONNUMMER              PIC  X(030).
      *
      * Mobilfunknummer der Firma
               10  'XX'-MOBILNUMMER                PIC  X(030).
      *
      * Faxnummer der Firma
               10  'XX'-FAXNUMMER                  PIC  X(030).
      *
      * Titel Ansprechpartner in der Firma
      *  0 - kein Titel
      *  1 - Dr.
      *  2 - Prof.
      *  3 - Prof. Dr.
      *  4 - Dr. h.c.
      *  5 - Mag.
      *  6 - Dipl-Kfm.
      *  7 - Ingenieur Mag.
      *  8 - Ingenieur
      *  9 - DI
      * 10 - Dipl.-Jur.
      * 11 - Dr. Ing.
      * 12 - Dipl. Ing.
      * 13 - Dr. med.
      * 14 - Dr. jur.
      * 15 - Bakk.
      * 16 - Dr. Dr.
      * 17 - Betriebswirt
               10  'XX'-TITELASP                   PIC  9(002).
      *
      * Anrede des Ansprechpartners
      *  0 - unbekannt
      *  1 - Herr
      *  2 - Frau
      *  4 - Eheleute
      *  5 - Firma
               10  'XX'-ANREDEASP                  PIC  9(001).
      *
      * Vorname Ansprechpartner in der Firma
               10  'XX'-VORNAMEASP                 PIC  X(030).
      *
      * Nachname Ansprechpartner in der Firma
               10  'XX'-NACHNAMEASP                PIC  X(030).
      *
      * Geburtsdatum Anprechpartner in der Firma
               10  'XX'-GEBDATUMASP                PIC  9(008).
      *
      * Kontakt / E-Mail
               10  'XX'-EMAILASP                   PIC  X(128).
      *
      * Funktion des Ansprechpartners im Unternehmen
               10  'XX'-FUNKTIONASP                PIC  X(050).
      * Reserve
               10  FILLER                          PIC  X(310).
 
      *****************************************************************
      * Endesatz
      *****************************************************************
      *
           05  'XX'-ENDESATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'E'
               10  'XX'-N-SA                       PIC  X(001).
      *
      * Satzanzahl - gelieferte D-Sätze
               10  'XX'-N-ANZAHL                   PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(991).




