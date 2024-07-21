      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1111.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI1111.CPv  $
      *
      *   Rev 2.12   Jan 28 2020 15:14:12   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen 
      *
      *   Rev 2.11   Mar 18 2019 17:38:10   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.10   Jan 25 2018 07:19:02   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.9   Jul 21 2016 11:50:46   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.8   Jul 20 2016 10:24:20   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.7   Jul 20 2016 10:13:40   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.6   Jul 20 2016 10:00:28   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.5   Sep 03 2013 11:09:18   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.4   Aug 22 2013 08:25:44   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.3   Jan 31 2012 10:01:18   t13ex39
      *AKI01085: Zusätzliche Anzeige in der OKI bei "Junges Konto"
      *
      *   Rev 2.2   Jul 25 2006 17:30:22   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en Kontokorrent
      *
      *   Rev 2.1   Jul 13 2003 23:36:32   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   07 Jun 2000 17:08:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Ausgabedatei Änderungsbestand           *
      *                Kontokorrentdaten von der Deutschen Bank        *
      *                                                                *
      *    SATZLÄNGE 450                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!15.01.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
      *AKI00587!16.06.06!2.2 ! Neue Felder                 !IFR        *
      *        !        !    !                             !           *
      *----------------------------------------------------------------*
      *AKI01085!29.11.11!3.0 !'Ende der Begünstigung'      !WHO        *
      *----------------------------------------------------------------*
      *AKI01204!17.07.13!3.1 !Neue Felder fuer SEPA        !UZI        *
      *----------------------------------------------------------------*
240616*AKI01449!24.06.16! neues Feld. Bchungsdatum Saldo   ! WHO       *
      *----------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO       *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI01658!24.01.19!*AUFNAHME ZUSÄTZLICHE FELDER      !SCHUMACHER *
      *        !        !  - KFW DARLEHEN ERSTRATE         !           *
      *        !        !  - KFW DARLEHEN FOLGERATE        !           *
      *        !        !  - KFW DARLEHEN KREDITBETRAG     !           *
      *        !        !  - KFW DARLEHEN SOLLZINSSATZ     !           *
      *        !        !  - KFW DARLEHEN ENDE             !           *
      *        !        !        ZINSFESTSCHREIBUNG        !           *
      *        !        !*ERWEITERUNG SATZLÄNGE AUF 450    !           *
      *---------!--------!--------------------------------!----------*
      * AKI01754!04.01.20! NEUES ATTRIBUT "ULTIMOSPAREN"  ! T13EX39
      *         !        !                                !
      *         !        !                                !
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1111.
      *
           05  BNK-SA                        PIC  X(002).
      *            SATZART        = '10'
           05  BNK-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER       PIC  9(003).
                   15 BNK-KUNDENNUMMER       PIC  9(007).
               10 BNK-UNTERKONTONUMMER       PIC  9(002).
               10 BNK-WAEHRUNG               PIC  9(003).
      *
           05  BNK-VERARB-KZ                 PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
      *
           05  BNK-DATUM-EROEFFNUNG.
               10 BNK-JAHR-EROEFFNUNG        PIC  9(004).
               10 BNK-MONAT-EROEFFNUNG       PIC  9(002).
               10 BNK-TAG-EROEFFNUNG         PIC  9(002).
           05  BNK-AEND-DAT-E-KZ             PIC  X(001).
      *        J = Änderung Datum Eroeffnung
      *
           05  BNK-DATUM-SCHLIESSUNG.
               10 BNK-JAHR-SCHLIESSUNG       PIC  9(004).
               10 BNK-MONAT-SCHLIESSUNG      PIC  9(002).
               10 BNK-TAG-SCHLIESSUNG        PIC  9(002).
           05  BNK-AEND-DAT-S-KZ             PIC  X(001).
      *        J = Änderung Datum Schliessung
      *
           05  BNK-BILANZSCHLUESSEL          PIC  9(004).
           05  BNK-AEND-BILANZSCHLUESSEL-KZ  PIC  X(001).
      *        J = Änderung Bilanzschlüssel
      *
           05  BNK-EXTERNES-LIMIT            PIC  9(012)V99.
           05  BNK-AEND-EXTERNES-LIMIT-KZ    PIC  X(001).
      *        J = Änderung Externes Limit
      *
           05  BNK-SALDO                     PIC  S9(012)V99.
           05  BNK-AEND-SALDO-KZ             PIC  X(001).
      *        J = Änderung Saldo
      *
           05  BNK-KONDITIONSSCHLUESSEL      PIC  X(002).
           05  BNK-AEND-KOND-SCHLUESSEL-KZ   PIC  X(001).
      *        J = Änderung Konditionsschlüssel
160606*
>>         05  BNK-KUMHABENAM                PIC  S9(012)V99.
           05  BNK-AEND-KUMHABENAM-KZ        PIC  X(001).
      *        J = Änderung Habenumsätze aktueller Monat
           05  BNK-KUMSOLLAM                 PIC  S9(012)V99.
           05  BNK-AEND-KUMSOLLAM-KZ         PIC  X(001).
      *        J = Änderung Sollumsätze aktueller Monat
           05  BNK-KUMHABENVM                PIC  S9(012)V99.
           05  BNK-AEND-KUMHABENVM-KZ        PIC  X(001).
      *        J = Änderung Habenumsätze Vormonat
           05  BNK-KUMSOLLVM                 PIC  S9(012)V99.
           05  BNK-AEND-KUMSOLLVM-KZ         PIC  X(001).
      *        J = Änderung Sollumsätze Vormonat
           05  BNK-ZRAM                      PIC  X(005).
           05  BNK-AEND-ZRAM-KZ              PIC  X(001).
      *        J = Änderung Monat für aktuelle Umsätze
           05  BNK-ZRVM                      PIC  X(005).
           05  BNK-AEND-ZRVM-KZ              PIC  X(001).
      *        J = Änderung Monat für Vormonats-Umsätze
           05  BNK-SOLLZINS                  PIC  S9(02)V9999.
           05  BNK-AEND-SOLLZINS-KZ          PIC  X(001).
      *        J = Änderung Sollzinssatz
           05  BNK-UEBERZZINS                PIC  S9(02)V9999.
           05  BNK-AEND-UEBERZZINS-KZ        PIC  X(001).
      *        J = Änderung Überuiehungszinssatz
           05  BNK-HABENZINS                 PIC  S9(02)V9999.
           05  BNK-AEND-HABENZINS-KZ         PIC  X(001).
<<    *        J = Änderung Habenzinssatz
291111     05  BNK-ENDE-BEGUENSTIGUNG.
291111         10 BNK-JAHR-ENDE-BEGUENSTIGUNG       PIC  9(004).
291111         10 BNK-MONAT-ENDE-BEGUENSTIGUNG      PIC  9(002).
291111         10 BNK-TAG-ENDE-BEGUENSTIGUNG        PIC  9(002).
291111     05  BNK-AEND-DAT-END-BEG-KZ              PIC  X(001).
291111*        J = Änderung Datum Ende Begünstigungszeitraum
170713     05  BNK-IBAN                      PIC X(034).
>>    *        IBAN zur Lastschrift für den Rateneinzug
           05  BNK-AEND-IBAN-KZ              PIC  X(001).
      *        J = Änderung IBAN
           05  BNK-BIC                       PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug
           05  BNK-AEND-BIC-KZ               PIC  X(001).
      *        J = Änderung BIC
           05  BNK-MANDREF                   PIC X(035).
      *        Mandatsreferenz
           05  BNK-AEND-MANDREF-KZ           PIC  X(001).
      *        J = Änderung MANDREF
           05  BNK-KONTOINH                  PIC X(028).
      *        Kontoinhaber Belastungskontot
      *        für den Rateneinzug
           05  BNK-AEND-KONTOINH-KZ          PIC  X(001).
      *        J = Änderung KONTOINH
    <<     05  BNK-IBAN-KK                   PIC X(034).
170713*            IBAN des PK-Kontos
           05  BNK-AEND-IBAN-KK-KZ           PIC  X(001).
      *        J = Änderung IBAN-KK
      *
           05  BNK-KREDIT-KENNZ-SCHL         PIC 9(002).
      *        KREDIT KENNZEICHENSCHLÜSSEL
           05  BNK-AEND-KREDIT-KENNZ-SCHL-KZ PIC X(001).
      *        J = ÄNDERUNG KREDIT-KENNZ-SCHL
 
240615     05  BNK-DATUM-SALDO.
>>             10 BNK-JAHR-DATUM-SALDO       PIC  9(004).
               10 BNK-MONAT-DATUM-SALDO      PIC  9(002).
>>             10 BNK-TAG-DATUM-SALDO        PIC  9(002).
240615     05  BNK-AEND-DATUM-SALDO-KZ       PIC  X(001).

           05  BNK-KOSTEN-SCHL               PIC  X(001).
      *        KOSTENSCHLÜSSEL ZINSKONTO PLUS
           05  BNK-AEND-KOSTEN-SCHL          PIC  X(001).
      *        J = ÄNDERUNG BNK-KOSTEN-SCHL
 
101217     05  BNK-PIN                       PIC  9(009).
101217     05  BNK-AEND-PIN-KZ               PIC  X(001).
 
           05  BNK-KFW-DARL-ERSTRATE         PIC 9(012)V99.
      *        KFW DARLEHEN ERSTRATE
           05  BNK-AEND-KFW-DARL-ERSTRATE    PIC X(001).
      *        J = ÄNDERUNG KFW-DARL-ERSTRATE
 
           05  BNK-KFW-DARL-FOLGERATE        PIC 9(012)V99.
      *        KFW DARLEHEN FOLGERATE
           05  BNK-AEND-KFW-DARL-FOLGERATE   PIC X(001).
      *        J = ÄNDERUNG KFW-DARL-FOLGERATE
 
           05  BNK-KFW-DARL-KREDITBETRAG     PIC 9(012)V99.
      *        KFW DARLEHEN KREDITBETRAG
           05  BNK-AEND-KFW-DARL-KREDITBETRAG
                                             PIC X(001).
      *        J = ÄNDERUNG KFW-DARL-KREDITBETRAG
 
           05  BNK-KFW-DARL-SOLLZINS         PIC 9(002)V9999.
      *        KFW DARLEHEN SOLLZINSSATZ
           05  BNK-AEND-KFW-DARL-SOLLZINS    PIC X(001).
      *        J = ÄNDERUNG KFW-DARL-SOLLZINS
 
           05  BNK-KFW-DARL-ENDE-ZFST.
               10 BNK-JAHR-ENDE-ZFST         PIC 9(004).
               10 BNK-MONAT-ENDE-ZFST        PIC 9(002).
               10 BNK-TAG-ENDE-ZFST          PIC 9(002).
      *        KFW DARLEHEN DATUM ENDE ZINSFESTSCHREIBUNG JJJJMMTT
           05  BNK-AEND-KFW-DARL-ENDE-ZFST   PIC X(001).
      *        J = ÄNDERUNG KFW-DARL-ENDE-ZFST
 
           05  BNK-KFW-DARL-TILGUNG          PIC 9(002)V9999.
      *        KFW DARLEHEN TILGUNG IN PROZENT
           05  BNK-AEND-KFW-DARL-TILGUNG     PIC X(001).
      *        J = ÄNDERUNG KFW-DARL-TILGUNG
           05  BNK-ULTIMOSTAR-KZ              PIC  X(001).
      *       UILTIMOSPAREN J/' '
           05  BNK-AEND-ULTIMOSTAR-KZ         PIC  X(001).
      *        J = ÄNDERUNG UILTIMOSPAREN J/' '
           05  BNK-RESERVE                   PIC X(031).
      *        Reserve







