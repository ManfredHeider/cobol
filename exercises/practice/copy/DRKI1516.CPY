      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1516.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1516.CPv  $
      *
      *   Rev 2.6   Apr 17 2020 13:42:14   t13ex39
      *AKI01767: Deutsche Bank - PK Smart - Erweiterung der Datenli
      *eferung um Laufzeit und Restkreditversicherungen
      *
      *   Rev 2.5   Jan 25 2018 07:20:38   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.4   Sep 02 2013 10:09:58   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.3   Sep 02 2013 10:01:12   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.2   Nov 05 2008 10:52:10   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.1   Jul 13 2003 23:25:18   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:49:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Privatkredit Daten           *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 150                                               *
130613*    SATZLÄNGE 300                                               *
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
      *--------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
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
       01  DRKI1516.
      *
           05  KSDS-SA                        PIC  X(002).
      *            SATZART        = '15'
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
      *    Vertrag aus KSDS ist in dem geleiferten Bestand nicht mehr
      *    vorhanden. Das Ablaufdatum wird auf das Tagesdatum gesetzt.
      *-----------------------------------------------------------------
           05  KSDS-DATUM-SCHLIESSUNG.
               10 KSDS-JAHR-SCHLIESSUNG       PIC  9(004).
               10 KSDS-MONAT-SCHLIESSUNG      PIC  9(002).
               10 KSDS-TAG-SCHLIESSUNG        PIC  9(002).
      *
           05  KSDS-DARLEHEN                  PIC S9(012)V99.
      *        Ursprungskreditbetrag
      *
221008*
>>>>>>     05  KSDS-DATUM-KREDITZUSAGE.
               10 KSDS-JAHR-KREDITZUSAGE      PIC  9(004).
               10 KSDS-MONAT-KREDITZUSAGE     PIC  9(002).
               10 KSDS-TAG-KREDITZUSAGE       PIC  9(002).
      *
           05  KSDS-DATUM-KREDITAUSZAHLUNG.
               10 KSDS-JAHR-KREDITAUSZAHLUNG  PIC  9(004).
               10 KSDS-MONAT-KREDITAUSZAHLUNG PIC  9(002).
               10 KSDS-TAG-KREDITAUSZAHLUNG   PIC  9(002).
      *
           05  KSDS-KREDITART                 PIC  X(006).
      *
           05  KSDS-AKTUELLER-SALDO           PIC S9(012)V99.
      *                AKTUELLER SALDO
      *
           05  KSDS-SOLLZINSSATZ              PIC S9(002)V9999.
      *                SOLLZINSSATZ IN PROZENT
      *
           05  KSDS-ANNUITAET                 PIC S9(012)V99.
      *                RATE IN EURO
      *
           05  KSDS-DATUM-FAELLIGKEIT.
               10 KSDS-JAHR-FAELLIGKEIT       PIC  9(004).
<<<<<<         10 KSDS-MONAT-FAELLIGKEIT      PIC  9(002).
221008         10 KSDS-TAG-FAELLIGKEIT        PIC  9(002).
      *
130613     05  KSDS-DEL-KZ                    PIC X(001).
>>
           05  KSDS-IBAN                      PIC X(034).
      *        IBAN zur Lastschrift für den Rateneinzug

           05  KSDS-BIC                       PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug

           05  KSDS-MANDREF                   PIC X(035).
      *        Mandatsreferenz

           05  KSDS-NACHNAME-BZ               PIC X(028).
      *        Kontoinhaber Fremdbank zur Lastschrift
      *        für den Rateneinzug

           05  KSDS-IBAN-PK                   PIC X(034).
      *        IBAN des PK-Kontos
101217     05  KSDS-PIN                       PIC 9(009).

           05  KSDS-GES-LAUFZEIT              PIC 9(003).
      *        GESAMTLAUFZEIT IN MONATEN (AKTUELL)
           05  KSDS-REST-LAUFZEIT             PIC 9(003).
      *        RESTLAUFZEIT IN MONATEN (AKTUELL
           05  KSDS-VERSUMFANG1               PIC 9(001).
      *        VERSICHERUNGSUMFANG 1. KREDITNEHMER
           05  KSDS-VERSUMFANG2               PIC 9(001).
      *        VERSICHERUNGSUMFANG 2. KREDITNEHMER
           05  KSDS-RESERVE                   PIC X(027).



