      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1517.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1517.CPv  $
      *
      *   Rev 2.6   Apr 17 2020 13:42:24   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.5   Jan 25 2018 07:20:52   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.4   Sep 02 2013 10:10:12   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.3   Sep 02 2013 10:01:24   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.2   Nov 05 2008 10:52:14   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.1   Jul 13 2003 23:24:08   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:49:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Ausgabedatei Privatkredit Daten der     *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 100                                               *
130613*    SATZLÄNGE 315                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
221008*AKI00609!22.10.08!2.2 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
221008*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
130613*AKI01204!13.06.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
130613*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
      *----------------------------------------------------------------*
      * AKI01767!04.03.20! NEUES ATTRIBUTE                ! T13EX39
      *         !        ! GESAMTLAUFZEIT                 !
      *         !        ! VERSICHERUNGSUMFANG1           !
      *         !        ! VERSICHERUNGSUMFANG2           !
      *         !        ! RESTLAUFZEIT                   !
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1517.
      *
           05  BNK-SA                         PIC X(002).
      *            SATZART        = '15'
           05  BNK-GES                        PIC 9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER        PIC 9(003).
                   15 BNK-KUNDENNUMMER        PIC 9(007).
               10 BNK-UNTERKONTONUMMER        PIC 9(002).
               10 BNK-WAEHRUNG                PIC 9(003).
      *
           05  BNK-VERARB-KZ                  PIC X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
      *                D = DELETE
      *
           05  BNK-DATUM-EROEFFNUNG.
               10 BNK-JAHR-EROEFFNUNG         PIC 9(004).
               10 BNK-MONAT-EROEFFNUNG        PIC 9(002).
               10 BNK-TAG-EROEFFNUNG          PIC 9(002).
      *
           05  BNK-AEND-DAT-E-KZ              PIC X(001).
      *            J = Änderung Datum Eroeffnung
      *
           05  BNK-DATUM-SCHLIESSUNG.
               10 BNK-JAHR-SCHLIESSUNG        PIC 9(004).
               10 BNK-MONAT-SCHLIESSUNG       PIC 9(002).
               10 BNK-TAG-SCHLIESSUNG         PIC 9(002).
      *
           05  BNK-AEND-DAT-S-KZ              PIC X(001).
      *            J = Änderung Datum Schliessung
      *
           05  BNK-DARLEHEN                   PIC S9(012)V99.
      *        URSPRUNGSKREDITBETRAG
      *
           05  BNK-AEND-DARLEHEN-KZ           PIC  X(001).
      *            J = Änderung Datum Eroeffnung
      *
221008     05  BNK-DATUM-KREDITZUSAGE.
>>>>>>         10 BNK-JAHR-KREDITZUSAGE       PIC 9(004).
               10 BNK-MONAT-KREDITZUSAGE      PIC 9(002).
               10 BNK-TAG-KREDITZUSAGE        PIC 9(002).
      *            DATUM DER KREDITZUSAGE
      *
           05  BNK-AEND-DAT-KREDZUS-KZ        PIC  X(01).
      *        J WENN DATUM GEÄNDERT
      *
           05  BNK-DATUM-KREDITAUSZAHLUNG.
               10 BNK-JAHR-KREDITAUSZAHLUNG   PIC  9(004).
               10 BNK-MONAT-KREDITAUSZAHLUNG  PIC  9(002).
               10 BNK-TAG-KREDITAUSZAHLUNG    PIC  9(002).
      *            DATUM DER KREDITAUSZAHLUNG
      *
           05  BNK-AEND-DAT-KREDAUS-KZ        PIC  X(001).
      *        J WENN DATUM GEÄNDERT
      *
           05  BNK-KREDITART                  PIC  X(006).
      *            KREDITART  PK ODER TK
      *
           05  BNK-AEND-KREDITART-KZ          PIC  X(001).
      *        J = ÄNDERUNG DER KREDITART
      *
           05  BNK-AKTUELLER-SALDO            PIC S9(12)V99.
      *        SALDO
      *
           05  BNK-AEND-SALDO-KZ              PIC  X(001).
      *        J WENN SALDO GEAENDERT
      *
           05  BNK-SOLLZINSSATZ               PIC S9(02)V9999.
      *        SOLLZINSSATZ
      *
           05  BNK-AEND-SOLLZINS-KZ           PIC  X(001).
      *        J = ÄNDERUNG SOLLZINSSATZ
      *
           05  BNK-ANNUITAET                  PIC S9(012)V99.
      *            AKTUELLE RATE IN EURO
      *
           05  BNK-AEND-ANNUITAET-KZ          PIC  X(001).
      *            J = ÄNDERUNG DER RATE
      *
           05  BNK-DATUM-FAELLIGKEIT.
               10 BNK-JAHR-FAELLIGKEIT        PIC  9(004).
               10 BNK-MONAT-FAELLIGKEIT       PIC  9(002).
               10 BNK-TAG-FAELLIGKEIT         PIC  9(002).
      *            DATUM DER KREDITFALLIGKEIT
      *
           05  BNK-AEND-DAT-KREDFAEL-KZ       PIC  X(001).
      *        J WENN DATUM GEÄNDERT
      *
130613     05  BNK-IBAN                       PIC X(034).
>>    *        IBAN zur Lastschrift für den Rateneinzug

           05  BNK-AEND-IBAN-KZ               PIC X(001).
      *        J WENN IBAN GEÄNDERT

           05  BNK-BIC                        PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug

           05  BNK-AEND-BIC-KZ                PIC X(001).
      *        J WENN BIC GEÄNDERT

           05  BNK-MANDREF                    PIC X(035).
      *        Mandatsreferenz

           05  BNK-AEND-MANDREF-KZ            PIC X(001).
      *        J WENN MANDATSREFERENZ GEÄNDERT

           05  BNK-NACHNAME-BZ                PIC X(028).
      *        Kontoinhaber Fremdbank zur Lastschrift
      *        für den Rateneinzug

           05  BNK-AEND-NACHNAME-BZ-KZ        PIC X(001).
      *        J WENN KONTOINHABER GEÄNDERT

           05  BNK-IBAN-PK                    PIC X(034).
      *        IBAN des PK-Kontos

           05  BNK-AEND-IBAN-PK-KZ            PIC X(001).
      *        J WENN IBAN PK-Konto GEÄNDERT
101217     05  BNK-PIN                       PIC  9(009).
101217     05  BNK-AEND-PIN-KZ               PIC  X(001).

           05  BNK-GES-LAUFZEIT              PIC 9(003).
      *        GESAMTLAUFZEIT IN MONATEN (AKTUELL)

           05  BNK-AEND-GES-LAUFZEIT-KZ      PIC X(001).
 
           05  BNK-REST-LAUFZEIT             PIC 9(003).
      *        RESTLAUFZEIT IN MONATEN (AKTUELL

           05  BNK-AEND-REST-LAUFZEIT-KZ     PIC X(001).

      *        VERSICHERUNGSUMFANG 1. KREDITNEHMER
           05  BNK-VERSUMFANG1               PIC 9(001).

           05  BNK-AEND-VERSUMFANG1-KZ       PIC X(001).

      *        VERSICHERUNGSUMFANG 2. KREDITNEHMER
           05  BNK-VERSUMFANG2               PIC 9(001).

           05  BNK-AEND-VERSUMFANG2-KZ       PIC X(001).
 
           05  BNK-RESERVE                   PIC X(022).
      *            RESERVE



