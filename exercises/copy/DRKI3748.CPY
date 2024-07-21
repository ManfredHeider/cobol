      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3748.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3748.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 13:34:24   t13ex78
      *AKI01731: Programme und Copies
      *
      *   Rev 2.0   Dec 03 2019 05:59:58   t13ex78
      *Archiverstellung
      *
      ******************************************************************
      *                                                                *
      *                                                                *
      * G66 - Santander Consumerbank                                   *
      *    Kundendaten - Ausgabe Ermittlung                            *
      *                                                                *
      *    Satzlänge 400 Bytes                                         *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01731!14.01.20!2.0 ! Neu                         ! t13ex78   *
      *        !        !    !                             !           *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-AEND-SATZ.
      *
      * Änderungskennzeichen - I oder U'
           05  'XX'-AEND-AKTU-KZN              PIC  X(001).
      *
      * Gesellschaft - immer '66'
           05  'XX'-AEND-GESELLSCHAFT          PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           05  'XX'-AEND-VNR                   PIC  X(015).
      *
      * laufende Nummer Kontoinhaber - max. 2
           05  'XX'-AEND-LFDNR                 PIC  9(001).
      *
      * Nachname
           05  'XX'-AEND-NACHNAME              PIC  X(035).
           05  'XX'-AEND-NACHNAME-KZN          PIC  X(001).
 
      * Nachname - vor der Änderung
           05  'XX'-AEND-NACHNAME-ALT          PIC  X(035).
           05  'XX'-AEND-NACHNAME-ALT-KZN      PIC  X(001).
      *
      * Vorname
           05  'XX'-AEND-VORNAME               PIC  X(035).
           05  'XX'-AEND-VORNAME-KZN           PIC  X(001).
      *
      * Vorname - vor der Änderung
           05  'XX'-AEND-VORNAME-ALT           PIC  X(035).
           05  'XX'-AEND-VORNAME-ALT-KZN       PIC  X(001).
      *
      * Geburtsdatum im Format JJJJMMTT
           05  'XX'-AEND-GEBDAT                PIC  X(008).
           05  'XX'-AEND-GEBDAT-KZN            PIC  X(001).
      *
      * Anrede
      * 1 - Herr
      * 2 - Frau
           05  'XX'-AEND-ANREDE                PIC  X(001).
           05  'XX'-AEND-ANREDE-KZN            PIC  X(001).
      *
      * Straße und Hausnummer
           05  'XX'-AEND-STRHNR                PIC  X(040).
           05  'XX'-AEND-STRHNR-KZN            PIC  X(001).
      *
      * Postleitzahl
           05  'XX'-AEND-PLZ                   PIC  X(009).
           05  'XX'-AEND-PLZ-KZN               PIC  X(001).
      *
      * Ort
           05  'XX'-AEND-ORT                   PIC  X(030).
           05  'XX'-AEND-ORT-KZN               PIC  X(001).
      *
      * ISO-Ländercode
           05  'XX'-AEND-ISOCODE               PIC  X(003).
           05  'XX'-AEND-ISOCODE-KZN                PIC  X(001).
      *
      * Reserve
           05  FILLER                          PIC  X(140).
 
 



