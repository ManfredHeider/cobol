      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1515.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI1515.CPv  $
      *
      *   Rev 2.8   Apr 17 2020 13:42:06   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.7   Sep 19 2017 10:04:18   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.6   Mar 09 2016 14:44:16   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.5   Sep 02 2013 10:10:06   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.4   Sep 02 2013 10:01:18   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.3   Nov 05 2008 10:52:06   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.2   Aug 18 2004 10:50:48   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank
      *
      *   Rev 2.1   Jul 13 2003 23:25:48   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:49:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand der       *
      *                Privatkredit Daten von der Deutschen Bank       *
      *                                                                *
      *    SATZLÄNGE 100                                               *
190613*    SATZLÄNGE 315
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
      *AKI00249!14.07.04!      Löschkennzeichen 'D' ver-   !WHO        *
      *        !        !      arbeiten                    !           *
      *----------------------------------------------------------------*
221008*AKI00609!22.10.08!2.3 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
221008*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
190613*AKI01204!19.06.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
190613*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
010917*AKI01542!01.09.17! Aufhname PIN                     !WHO        *
      *        !        !                                  !           *
      *---------!--------!--------------------------------!----------*
      * AKI01767!04.03.20! NEUES ATTRIBUTE                ! T13EX39
      *         !        ! GESAMTLAUFZEIT                 !
      *         !        ! VERSICHERUNGSUMFANG1           !
      *         !        ! VERSICHERUNGSUMFANG2           !
      *         !        ! RESTLAUFZEIT                   !
      *         !        !                                !
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1515.
      *
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ.
      *
               15  'XX'-SA                        PIC X(002).
      *            SATZART        = '15'
               15  'XX'-GES                       PIC 9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-KEY.
                   20  'XX'-VNR.
                       25 'XX'-FILIALNUMMER       PIC 9(003).
                       25 'XX'-KUNDENNUMMER       PIC 9(007).
                   20 'XX'-UNTERKONTONUMMER       PIC 9(002).
                   20 'XX'-WAEHRUNG               PIC 9(003).
      *
               15  'XX'-DATUM-EROEFFNUNG.
                   20 'XX'-JAHR-EROEFFNUNG        PIC 9(004).
                   20 'XX'-MONAT-EROEFFNUNG       PIC 9(002).
                   20 'XX'-TAG-EROEFFNUNG         PIC 9(002).
      *
               15  'XX'-DATUM-SCHLIESSUNG.
                   20 'XX'-JAHR-SCHLIESSUNG       PIC 9(004).
                   20 'XX'-MONAT-SCHLIESSUNG      PIC 9(002).
                   20 'XX'-TAG-SCHLIESSUNG        PIC 9(002).
      *
               15  'XX'-DARLEHEN                  PIC S9(012)V99.
      *            URSPRUNGSKREDITBETRAG
      *
221008*
>>>>>>         15  'XX'-DATUM-KREDITZUSAGE.
                   20 'XX'-JAHR-KREDITZUSAGE      PIC 9(004).
                   20 'XX'-MONAT-KREDITZUSAGE     PIC 9(002).
                   20 'XX'-TAG-KREDITZUSAGE       PIC 9(002).
      *
               15  'XX'-DATUM-KREDITAUSZAHLUNG.
                   20 'XX'-JAHR-KREDITAUSZAHLUNG  PIC 9(004).
                   20 'XX'-MONAT-KREDITAUSZAHLUNG PIC 9(002).
                   20 'XX'-TAG-KREDITAUSZAHLUNG   PIC 9(002).
      *
               15  'XX'-KREDITART                 PIC X(006).
      *
               15  'XX'-AKTUELLER-SALDO           PIC S9(012)V99.
      *                AKTUELLER SALDO
      *
               15  'XX'-SOLLZINSSATZ              PIC S9(02)V9999.
      *                SOLLZINSSATZ IN PROZENT
      *
               15  'XX'-ANNUITAET                 PIC S9(012)V99.
      *                RATE IN EURO
      *
               15  'XX'-DATUM-FAELLIGKEIT.
                   20 'XX'-JAHR-FAELLIGKEIT       PIC 9(004).
<<<<<<             20 'XX'-MONAT-FAELLIGKEIT      PIC 9(002).
221008             20 'XX'-TAG-FAELLIGKEIT        PIC 9(002).
      *
140704         15  'XX'-DEL-KZ                    PIC X(001).

190613         15  'XX'-IBAN                      PIC X(034).
>>    *            IBAN zur Lastschrift für den Rateneinzug

               15  'XX'-BIC                       PIC X(011).
      *            BIC zur Lastschrift für den Rateneinzug

               15  'XX'-MANDREF                   PIC X(035).
      *            Mandatsreferenz

               15  'XX'-NACHNAME-BZ               PIC X(028).
      *            Kontoinhaber Fremdbank zur Lastschrift
      *            für den Rateneinzug

               15  'XX'-IBAN-PK                   PIC X(034).
      *            IBAN des PK-Kontos

010917         15  'XX'-PIN                       PIC 9(009).

               15  'XX'-GES-LAUFZEIT              PIC 9(003).
      *            GESAMTLAUFZEIT IN MONATEN (AKTUELL)
               15  'XX'-REST-LAUFZEIT             PIC 9(003).
      *            RESTLAUFZEIT IN MONATEN (AKTUELL
               15  'XX'-VERSUMFANG1               PIC X(001).
      *            VERSICHERUNGSUMFANG 1. KREDITNEHMER
               15  'XX'-VERSUMFANG2               PIC X(001).
      *            VERSICHERUNGSUMFANG 2. KREDITNEHMER

 
               15  'XX'-RESERVE                   PIC X(027).
      *
      ******************************************************************
      *                                                                *
      *          VORSATZ                                               *
      *                                                                *
      ******************************************************************
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
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
      *            Komplettlieferung 'PKK'
               15  'XX'-V-WKZ                     PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
130613         15  FILLER                         PIC X(267).
      *
      ******************************************************************
      *                                                                *
      *          NACHSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
               15  'XX'-N-SA                      PIC  X(002).
      *            SATZART '99'
               15  'XX'-N-ANZ                     PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
190613         15  FILLER                         PIC  X(291).



