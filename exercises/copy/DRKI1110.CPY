      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1110.CPY  $
      *    $Revision:   2.19  $
      *
      *    $Log:   L:/mf/CPY/DRKI1110.CPv  $
      *
      *   Rev 2.19   Jan 28 2020 15:13:42   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen 
      *
      *   Rev 2.18   Mar 14 2019 14:45:14   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.17   Mar 12 2019 12:58:38   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.16   Sep 19 2017 10:03:30   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.15   Jun 26 2017 10:34:14   t13ex39
      *AKI01506: Deuba KK aus GHP und DBP nach BANKPRODUKT überführ
      *en
      *
      *   Rev 2.14   May 23 2017 15:40:14   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.13   May 23 2017 15:34:44   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.12   May 23 2017 15:07:22   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.11   Jul 21 2016 11:50:38   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.10   Jul 20 2016 10:24:12   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.9   Jul 20 2016 10:13:32   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.8   Jul 20 2016 10:00:18   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.7   Mar 09 2016 14:43:56   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.6   Jul 30 2013 14:05:02   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.5   Jan 31 2012 10:01:10   t13ex39
      *AKI01085: Zusätzliche Anzeige in der OKI bei "Junges Konto"
      *
      *   Rev 2.4   Jul 25 2006 17:30:08   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en Kontokorrent
      *
      *   Rev 2.3   Jul 26 2004 15:13:20   T13EX39
      *AKI00249: DeuBA Depot u. Banking-Produkte
      *
      *   Rev 2.2   Jul 26 2004 12:54:18   T13EX39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank KONTOKORRENT
      *
      *   Rev 2.1   Jul 13 2003 23:35:58   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   07 Jun 2000 17:08:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand           *
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
      *AKI00249!14.07.04!      Löschkennzeichen 'D' ver-   !WHO        *
      *        !        !      arbeiten                    !           *
      *AKI00587!16.06.06!2.2 ! Neue Felder                 !IFR        *
      *----------------------------------------------------------------*
      *AKI01085!29.11.11!3.0 !'Ende der Begünstigung'      !WHO        *
      *----------------------------------------------------------------*
      *AKI01204!17.07.13!3.1 !Neue Felder fuer SEPA        !UZI        *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
240616*AKI01449!24.06.16! neues Feld. Bchungsdatum Saldo   ! WHO       *
      *----------------------------------------------------------------*
240417*AKI01512!24.04.17! neu: 'Kostenschlüssel'           !WHO        *
      *----------------------------------------------------------------*
010917*AKI01542!01.09.17! Aufhname PIN                     !WHO        *
      *----------------------------------------------------------------*
      *AKI01658!22.01.19!*AUFNAHME ZUSÄTZLICHE FELDER      !SCHUMACHER *
      *        !        !  - KFW DARLEHEN ERSTRATE         !           *
      *        !        !  - KFW DARLEHEN FOLGERATE        !           *
      *        !        !  - KFW DARLEHEN KREDITBETRAG     !           *
      *        !        !  - KFW DARLEHEN SOLLZINSSATZ     !           *
      *        !        !  - KFW DARLEHEN ENDE             !           *
      *        !        !        ZINSFESTSCHREIBUNG        !           *
      *        !        !*ERWEITERUNG SATZLÄNGE AUF 450    !           *
      *        !        !*DATUM ENDE BEGUENSTIGUNG         !           *
      *        !        ! UND DATUM SALDO ALS NUMERISCH    !           *
      *        !        ! UND ANPASSUNG STRUKTUR           !           *
      *---------!--------!--------------------------------!----------*
      * AKI01754!04.01.20! NEUES ATTRIBUT "ULTIMOSPAREN"  ! T13EX39
      *         !        !                                !
      *         !        !                                !

      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1110.
      *
      ******************************************************************
      *                                                                *
      *                 VORSATZ                                        *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-VORSATZ.
               15  'XX'-V-SA                      PIC X(002).
      *            SATZART = '01'
               15  'XX'-V-ABSENDER                PIC X(008).
      *            ABSENDER = 'DB AG'
               15  'XX'-V-DATUM                   PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER              PIC X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT                    PIC X(008).
      *            INHALTSANGABE:
      *            Tägliche Lieferung 'KONTOT'
      *            Komplettbestand    'KONTOK'
               15  'XX'-V-WKZ                     PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
160606*        15  FILLER                         PIC X(067).
160606*        15  FILLER                         PIC X(267).
160713*        15  FILLER                         PIC X(317).
               15  FILLER                         PIC X(417).
      *            RESERVE
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ   REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                        PIC  X(002).
      *            SATZART        = '10'
               15  'XX'-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-KEY.
                   20  'XX'-VNR.
                       25 'XX'-FILIALNUMMER       PIC  9(003).
                       25 'XX'-KUNDENNUMMER       PIC  9(007).
                   20 'XX'-UNTERKONTONUMMER       PIC  9(002).
                   20 'XX'-WAEHRUNG               PIC  9(003).
      *
               15  'XX'-DATUM-EROEFFNUNG.
                   20 'XX'-JAHR-EROEFFNUNG        PIC  9(004).
                   20 'XX'-MONAT-EROEFFNUNG       PIC  9(002).
                   20 'XX'-TAG-EROEFFNUNG         PIC  9(002).

               15  'XX'-DATUM-SCHLIESSUNG.
                   20 'XX'-JAHR-SCHLIESSUNG       PIC  9(004).
                   20 'XX'-MONAT-SCHLIESSUNG      PIC  9(002).
                   20 'XX'-TAG-SCHLIESSUNG        PIC  9(002).
               15  'XX'-BILANZSCHLUESSEL          PIC  9(004).
               15  'XX'-EXTERNES-LIMIT            PIC  9(012)V99.
               15  'XX'-SALDO                     PIC  S9(012)V99.
      *
      *        Der Konditionsschlüssel wird übertragen, wenn der
      *        Bilanzschlüssel kleiner als 30 ist, und der Konditions-
      *        schlüssel 02, 04 oder 90 enthält
      *        00 = ZinsKontoPlus
      *        02 = dbServiceKonto
      *        04 = dbOnlineKonto
      *        90 = das junge Konto
               15  'XX'-KONDITIONSSCHLUESSEL      PIC  X(002).
140704         15  'XX'-DEL-KZ                    PIC  X(001).
160606         15  'XX'-KUMHABENAM                PIC  S9(012)V99.
160606         15  'XX'-KUMSOLLAM                 PIC  S9(012)V99.
160606         15  'XX'-KUMHABENVM                PIC  S9(012)V99.
160606         15  'XX'-KUMSOLLVM                 PIC  S9(012)V99.
160606         15  'XX'-ZRAM                      PIC  X(005).
160606         15  'XX'-ZRVM                      PIC  X(005).
160606         15  'XX'-SOLLZINS                  PIC  S9(02)V9999.
160606         15  'XX'-UEBERZZINS                PIC  S9(02)V9999.
160606         15  'XX'-HABENZINS                 PIC  S9(02)V9999.
291111
               15  'XX'-DATUM-ENDEBEGUENST.
                   20 'XX'-JAHR-ENDEBEGUENST      PIC  9(004).
                   20 'XX'-MONAT-ENDEBEGUENST     PIC  9(002).
                   20 'XX'-TAG-ENDEBEGUENST       PIC  9(002).

170713         15  'XX'-IBAN                      PIC X(034).
>>    *            IBAN zur Lastschrift für den Rateneinzug
               15  'XX'-BIC                       PIC X(011).
      *            BIC zur Lastschrift für den Rateneinzug
               15  'XX'-MANDREF                   PIC X(035).
      *            Mandatsreferenz
               15  'XX'-KONTOINH                  PIC X(028).
      *            Kontoinhaber Belastungskontot
      *            für den Rateneinzug
    <<         15  'XX'-IBAN-KK                   PIC X(034).
170713*            IBAN des PK-Kontos

240416         15  'XX'-KREDIT-KENNZ-SCHL         PIC 9(002).

               15  'XX'-DATUM-SALDO.
                   20 'XX'-JAHR-SALDO             PIC  9(004).
                   20 'XX'-MONAT-SALDO            PIC  9(002).
                   20 'XX'-TAG-SALDO              PIC  9(002).

240417         15  'XX'-KOSTEN-SCHL               PIC X(001).

010917         15  'XX'-PIN                       PIC 9(009).

               15  'XX'-KFW-DARL-ERSTRATE         PIC 9(012)V99.
      *            KFW DARLEHEN ERSTRATE
               15  'XX'-KFW-DARL-FOLGERATE        PIC 9(012)V99.
      *            KFW DARLEHEN FOLGERATE
               15  'XX'-KFW-DARL-KREDITBETRAG     PIC 9(012)V99.
      *            KFW DARLEHEN KREDITBETRAG
               15  'XX'-KFW-DARL-SOLLZINS         PIC 9(002)V9999.
      *            KFW DARLEHEN SOLLZINSSATZ
               15  'XX'-KFW-DARL-ENDE-ZFST.
                   20 'XX'-JAHR-ENDE-ZFST         PIC 9(004).
                   20 'XX'-MONAT-ENDE-ZFST        PIC 9(002).
                   20 'XX'-TAG-ENDE-ZFST          PIC 9(002).
      *            KFW DARLEHEN DATUM ENDE ZINSFESTSCHREIBUNG JJJJMMTT

               15  'XX'-KFW-DARL-TILGUNG          PIC 9(002)V9999.
      *            KFW DARLEHEN TILGUNG IN PROZENT
               15  'XX'-ULTIMOSTAR-KZ              PIC  X(001).
      *            UILTIMOSPAREN J/' '
               15  'XX'-AEND-ULTIMOSTAR-KZ         PIC  X(001).
      *            J = ÄNDERUNG UILTIMOSPAREN J/' '
      *
               15  'XX'-RESERVE                   PIC X(062).
      *            RESERVE
      *
      ******************************************************************
      *                                                                *
      *          NACHSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-NACHSATZ  REDEFINES     'XX'-VORSATZ.
               15  'XX'-N-SA                      PIC  X(002).
      *            SATZART '99'
               15  'XX'-N-ANZ                     PIC  9(007).
      *            ANZAHL SAETZE INCL. VORSATZ
160606*        15  FILLER                         PIC  X(091).
160606*        15  FILLER                         PIC  X(291).
170713*        15  FILLER                         PIC  X(341).
               15  FILLER                         PIC  X(441).













