      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1510.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1510.CPv  $
      *
      *   Rev 2.4   Jan 25 2018 07:19:32   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.3   Sep 02 2013 10:46:30   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Spardaten
      *
      *   Rev 2.2   Nov 05 2008 10:51:44   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.1   Jul 13 2003 23:28:36   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Spardaten von der            *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 100                                               *
080713*    SATZLÄNGE 250                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !VERS! Text                        !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
290808*AKI00609!29.08.08!2.2 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
290808*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
080713*AKI01204!08.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
080713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *--------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1510.
      *
           05  KSDS-SA                        PIC X(002).
      *            SATZART        = '13'
           05  KSDS-GES                       PIC 9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 KSDS-FILIALNUMMER       PIC 9(003).
                   15 KSDS-KUNDENNUMMER       PIC 9(007).
               10 KSDS-UNTERKONTONUMMER       PIC 9(002).
      *
           05  KSDS-DATUM-EROEFFNUNG.
               10 KSDS-JAHR-EROEFFNUNG        PIC 9(004).
               10 KSDS-MONAT-EROEFFNUNG       PIC 9(002).
               10 KSDS-TAG-EROEFFNUNG         PIC 9(002).
      *
           05  KSDS-DATUM-SCHLIESSUNG.
               10 KSDS-JAHR-SCHLIESSUNG       PIC 9(004).
               10 KSDS-MONAT-SCHLIESSUNG      PIC 9(002).
               10 KSDS-TAG-SCHLIESSUNG        PIC 9(002).
      *
           05  KSDS-KONTOART                  PIC X(003).
      *        SPL - Bonussparen (Bilanzschlüssel 3100)
      *        FZS - Festzinssparen (Bilanzschlüssel 3070)
      *
           05  KSDS-SALDO                     PIC S9(012)V99.
      *
           05  KSDS-VERTRAGSENDE.
               10 KSDS-JAHR-VERTRAGSENDE      PIC 9(004).
               10 KSDS-MONAT-VERTRAGSENDE     PIC 9(002).
               10 KSDS-TAG-VERTRAGSENDE       PIC 9(002).
      *        Dieses Datum kann beim Festzinssparen revolvierend
      *        sein.
      *
290808     05  KSDS-ZAHLUNGSWEISE             PIC 9(001).
290808*        ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
290808*        2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
290808*
290808     05  KSDS-RATENHOEHE                PIC S9(012)V99.
290808*        SPARRATE
290808*
290808     05  KSDS-HABENZINS                 PIC S9(02)V9999.
290808*        HABENZINSSATZ
290808*

080713     05  KSDS-DEL-KZ                    PIC X(001).
>>
           05  KSDS-IBAN                      PIC X(034).
      *        IBAN zur Lastschrift für den Rateneinzug

           05  KSDS-BIC                       PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug

           05  KSDS-MANDREF                   PIC X(035).
      *        Mandatsreferenz

           05  KSDS-NACHNAME-BZ               PIC X(028).
      *        Kontoinhaber des Belastungskontos zur Lastschrift
      *        für den Rateneinzug

           05  KSDS-IBAN-SPK                  PIC X(034).
      *        IBAN des Sparkontos
101217     05  KSDS-PIN                       PIC 9(009).

101217     05  KSDS-RESERVE                   PIC X(020).
101217*    05  KSDS-RESERVE                   PIC X(029).
      *        RESERVE



