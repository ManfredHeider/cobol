      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.05.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3789.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3789.CPv  $
      *
      *   Rev 2.1   Jul 07 2020 13:00:44   t13ex78
      *AKI01780: Programme und Copies
      *
      *   Rev 2.0   May 20 2020 08:59:52   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      * G90 - EOS                                                      *
      *    Kundendaten - Ausgabe Ermittlung                            *
      *                                                                *
      *    Satzlänge 1000 Bytes                                        *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01780!22.06.20!2.0 ! Neu                         ! t13ex78   *
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
      * Gesellschaft - immer '90'
           05  'XX'-AEND-GESELLSCHAFT          PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           05  'XX'-AEND-VNR                   PIC  X(015).
      *
      * Rechtsform
           05  'XX'-AEND-RECHTSFORM            PIC  X(001).
           05  'XX'-AEND-RECHTSFORM-KZN        PIC  X(001).
      *
      * Anrede
      * 1 - Herr
      * 2 - Frau
           05  'XX'-AEND-ANREDE                PIC  X(001).
           05  'XX'-AEND-ANREDE-KZN            PIC  X(001).
      *
      * Firmenvorname
           05  'XX'-AEND-FVORNAME              PIC  X(030).
           05  'XX'-AEND-FVORNAME-KZN          PIC  X(001).
 
      * Firmenvorname - vor der Änderung
           05  'XX'-AEND-FVORNAME-ALT          PIC  X(030).
           05  'XX'-AEND-FVORNAME-ALT-KZN      PIC  X(001).
      *
      * Firmennachname
           05  'XX'-AEND-FNACHNAME             PIC  X(030).
           05  'XX'-AEND-FNACHNAME-KZN         PIC  X(001).
      *
      * Firmennachname - vor der Änderung
           05  'XX'-AEND-FNACHNAME-ALT         PIC  X(030).
           05  'XX'-AEND-FNACHNAME-ALT-KZN     PIC  X(001).
      *
      * Firmeninhaber
           05  'XX'-AEND-FINHABER              PIC  X(100).
           05  'XX'-AEND-FINHABER-KZN          PIC  X(001).
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
           05  'XX'-AEND-ORT                   PIC  X(040).
           05  'XX'-AEND-ORT-KZN               PIC  X(001).
      *
      * Postfach abw. Postfachanschrift
           05  'XX'-AEND-POSTFACHABWADR        PIC  9(009).
           05  'XX'-AEND-POSTFACHABWADR-KZN    PIC  X(001).
      *
      * Postleitzahl abw. Postfachanschrift
           05  'XX'-AEND-PLZABWADR             PIC  9(009).
           05  'XX'-AEND-PLZABWADR-KZN         PIC  X(001).
      *
      * Ort abw. Postfachanschrift
           05  'XX'-AEND-ORTABWADR             PIC  X(040).
           05  'XX'-AEND-ORTABWADR-KZN         PIC  X(001).
      *
      * Telefonnummer der Firma
           05  'XX'-AEND-TELEFONNUMMER         PIC  X(030).
           05  'XX'-AEND-TELEFONNUMMER-KZN     PIC  X(001).
      *
      * Mobilfunknummer der Firma
           05  'XX'-AEND-MOBILNUMMER           PIC  X(030).
           05  'XX'-AEND-MOBILNUMMER-KZN       PIC  X(001).
      *
      * Faxnummer der Firma
           05  'XX'-AEND-FAXNUMMER             PIC  X(030).
           05  'XX'-AEND-FAXNUMMER-KZN         PIC  X(001).
      *
      * Titel Ansprechpartner
           05  'XX'-AEND-TITELASP              PIC  9(002).
           05  'XX'-AEND-TITELASP-KZN          PIC  X(001).
      *
      * Anrede Ansprechpartner
           05  'XX'-AEND-ANREDEASP             PIC  9(001).
           05  'XX'-AEND-ANREDEASP-KZN         PIC  X(001).
      *
      * Vorname des Ansprechpartners
           05  'XX'-AEND-VORNAMEASP            PIC  X(030).
           05  'XX'-AEND-VORNAMEASP-KZN        PIC  X(001).
      *
      * Nachname des Ansprechpartners
           05  'XX'-AEND-NACHNAMEASP           PIC  X(030).
           05  'XX'-AEND-NACHNAMEASP-KZN       PIC  X(001).
      *
      * Geburtsdatum des Ansprechpartners
           05  'XX'-AEND-GEBDATUMASP           PIC  9(008).
           05  'XX'-AEND-GEBDATUMASP-KZN       PIC  X(001).
      *
      * Kontakt / E-Mail
           05  'XX'-AEND-EMAILASP              PIC  X(128).
           05  'XX'-AEND-EMAILASP-KZN          PIC  X(001).
      *
      * Funktion des Ansprechpartners im Unternehmen
           05  'XX'-AEND-FUNKTIONASP           PIC  X(050).
           05  'XX'-AEND-FUNKTIONASP-KZN       PIC  X(001).
      *
      * Reserve
           05  FILLER                          PIC  X(251).
 
 



