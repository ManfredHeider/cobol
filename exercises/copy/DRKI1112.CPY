      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1112.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI1112.CPv  $
      *
      *   Rev 2.13   Feb 28 2018 09:33:30   t13ex39
      *FKI01564: Fehler in der Ermitlung Kontokorrent
      *
      *   Rev 2.12   Jan 25 2018 07:19:18   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.11   Feb 09 2017 13:29:56   t13ex39
      *AKI01459: DEUBA  Migration PK-Kredit Bankprodukt GHP DBProdukt
      *
      *   Rev 2.10   Jul 21 2016 11:50:52   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.9   Jul 20 2016 10:24:26   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.8   Jul 20 2016 10:13:46   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.7   Jul 20 2016 10:00:34   t13ex39
      *AKI01449: Deutsche Bank Konto: Datum des Saldos
      *
      *   Rev 2.6   Sep 03 2013 11:09:24   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.5   Aug 22 2013 08:25:48   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.4   Jan 31 2012 10:01:26   t13ex39
      *AKI01085: Zusätzliche Anzeige in der OKI bei "Junges Konto"
      *
      *   Rev 2.3   Jul 25 2006 17:30:34   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en Kontokorrent
      *
      *   Rev 2.2   Mar 16 2005 11:45:32   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 13 2003 23:37:04   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   07 Jun 2000 17:08:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Kontokorrentdaten der        *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 450                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!14.01.03! Neuerstellung                    !Golücke    *
      *--------!-------------------------------------------!-----------*
      *FKI00992!10.03.05!02  ! wegen Tagesdatum der        !Hornung    *
      *        !        !    ! Löschung in hist. KSDS      !           *
      *        !        !    ! DEL-KZ mit aufgenommen      !           *
      *        !        !    ! wegen MOVE CORR             !           *
      *AKI00587!16.06.06!2.3 ! Neue Felder                 !IFR        *
      *----------------------------------------------------------------*
      *AKI01085!29.11.11!3.0 !'Ende der Begünstigung'      !WHO        *
      *----------------------------------------------------------------*
      *AKI01204!17.07.13!3.1 ! Anpassung für SEPA          !UZI        *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
240616*AKI01449!24.06.16! neues Feld. Bchungsdatum Saldo   ! WHO       *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
080217*AKI01459!08.02.17! KREDIT-KENNZ-SCHL hat gefehlt    ! WHO       *
      *        !        ! Stellengerecht eingefügt         !           *
      *--------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *--------------------------------------------------------------*
280218* FKI01564!28.02.18! Fehler in COPY DRKI1112        ! WHO      *
      *         !        ! Attribut KOSTEN-SCHL fehlt     !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1112.
      *
           05  KSDS-SA                        PIC  X(002).
      *            SATZART        = '10'
           05  KSDS-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 KSDS-FILIALNUMMER       PIC  9(003).
                   15 KSDS-KUNDENNUMMER       PIC  9(007).
               10 KSDS-UNTERKONTONUMMER       PIC  9(002).
               10 KSDS-WAEHRUNG               PIC  9(003).
      *
           05  KSDS-DATUM-EROEFFNUNG.
               10 KSDS-JAHR-EROEFFNUNG        PIC  9(004).
               10 KSDS-MONAT-EROEFFNUNG       PIC  9(002).
               10 KSDS-TAG-EROEFFNUNG         PIC  9(002).
      *
      *-----------------------------------------------------------------
      *    Wenn der Vertrag in der KSDS nicht mehr in dem gelieferten
      *    Bestand vorhanden ist, wird das Schliessungsdatum auf das
      *    Tagesdatum gesetzt.
      *-----------------------------------------------------------------
           05  KSDS-DATUM-SCHLIESSUNG.
               10 KSDS-JAHR-SCHLIESSUNG       PIC  9(004).
               10 KSDS-MONAT-SCHLIESSUNG      PIC  9(002).
               10 KSDS-TAG-SCHLIESSUNG        PIC  9(002).
      *
           05  KSDS-BILANZSCHLUESSEL          PIC  9(004).
           05  KSDS-EXTERNES-LIMIT            PIC  9(012)V99.
           05  KSDS-SALDO                     PIC  S9(012)V99.
      *
           05  KSDS-KONDITIONSSCHLUESSEL      PIC  X(002).
100305     05  KSDS-DEL-KZ                    PIC  X(001).
160606     05  KSDS-KUMHABENAM                PIC  S9(012)V99.
160606     05  KSDS-KUMSOLLAM                 PIC  S9(012)V99.
160606     05  KSDS-KUMHABENVM                PIC  S9(012)V99.
160606     05  KSDS-KUMSOLLVM                 PIC  S9(012)V99.
160606     05  KSDS-ZRAM                      PIC  X(005).
160606     05  KSDS-ZRVM                      PIC  X(005).
160606     05  KSDS-SOLLZINS                  PIC  S9(02)V9999.
160606     05  KSDS-UEBERZZINS                PIC  S9(02)V9999.
160606     05  KSDS-HABENZINS                 PIC  S9(02)V9999.
291111     05  KSDS-ENDE-BEGUENSTIGUNG        PIC  X(008).
170713     05  KSDS-IBAN                      PIC X(034).
>>    *        IBAN zur Lastschrift für den Rateneinzug
           05  KSDS-BIC                       PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug
           05  KSDS-MANDREF                   PIC X(035).
      *        Mandatsreferenz
           05  KSDS-KONTOINH                  PIC X(028).
      *        Kontoinhaber Belastungskontot
      *        für den Rateneinzug
    <<     05  KSDS-IBAN-KK                   PIC X(034).
170713*            IBAN des PK-Kontos
080217     05  KSDS-KREDIT-KENNZ-SCHL         PIC X(002).
240616     05  KSDS-DATUM-SALDO               PIC X(008).
280218     05  KSDS-KOSTEN-SCHL               PIC X(001).
101217     05  KSDS-PIN                       PIC 9(009).
280218*    05  KSDS-RESERVE                   PIC X(127).
280218     05  KSDS-RESERVE                   PIC X(126).
      *            RESERVE







