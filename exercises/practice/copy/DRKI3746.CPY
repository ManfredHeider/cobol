      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3746.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3746.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 13:33:24   t13ex78
      *AKI01731: Programme und Copies
      *
      *   Rev 2.0   Dec 03 2019 10:59:32   t13ex78
      *Archiverstellung
      *
      ******************************************************************
      *                                                                *
      *                                                                *
      * G66 - Santander Consumerbank                                   *
      *    Kundendaten - DTA                                           *
      *                                                                *
      *    Satzlänge 300 Bytes                                         *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01731!03.12.19!2.0 ! Neu                         ! t13ex78   *
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
      * Absender - 'SCB BANK'
               10  'XX'-V-ABSENDER                 PIC  X(008).
      *
      * Datum der Erstellung des DTA im Format  JHJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *
      * Welche Art Lieferung?
      * 'KUNDE-K' - Komplettlieferung
      * 'KUNDE-D' - Deltalieferung
               10  'XX'-V-LIEFERUNG                PIC  X(008).
      *
      * Reserve
               10  FILLER                          PIC  X(271).
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
      * Gesellschaft - immer '66'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * laufende Nummer Kontoinhaber - max. 2
               10  'XX'-LFDNR                      PIC  9(001).
      *
      * Nachname
               10  'XX'-NACHNAME                   PIC  X(035).
      *
      * Vorname
               10  'XX'-VORNAME                    PIC  X(035).
      *
      * Geburtsdatum im Format JJJJMMTT
               10  'XX'-GEBDAT                     PIC  X(008).
      *
      * Anrede
      * 1 - Herr
      * 2 - Frau
               10  'XX'-ANREDE                     PIC  X(001).
      *
      * Straße und Hausnummer
               10  'XX'-STRHNR                     PIC  X(040).
      *
      * Postleitzahl
               10  'XX'-PLZ                        PIC  X(009).
      *
      * Ort
               10  'XX'-ORT                        PIC  X(030).
      *
      * ISO-Ländercode
               10  'XX'-ISOCODE                    PIC  X(003).
      *
      * Reserve
               10  FILLER                          PIC  X(120).
 
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
               10  FILLER                          PIC  X(291).



