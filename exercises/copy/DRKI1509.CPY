      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1509.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1509.CPv  $
      *
      *   Rev 2.6   Sep 19 2017 10:04:08   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.5   Mar 09 2016 14:44:58   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.4   Sep 02 2013 10:46:24   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Spardaten
      *
      *   Rev 2.3   Nov 05 2008 10:51:38   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.2   Aug 18 2004 10:50:30   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank
      *
      *   Rev 2.1   Jul 13 2003 23:31:20   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand der       *
      *                Spardaten von der Deutschen Bank                *
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
      *AKI00249!14.07.04!      LÖSCHKENNZEICHEN 'D' VER-   !WHO        *
      *        !        !      ARBEITEN                    !           *
      *----------------------------------------------------------------*
290808*AKI00609!29.08.08!2.3 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
290808*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
080713*AKI01204!08.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
080713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *        !        !    !                             !           *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
010917*AKI01542!01.09.17! Aufhname PIN                     !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1509.
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ.
      *
               15  'XX'-SA                        PIC X(002).
      *            SATZART        = '13'
               15  'XX'-GES                       PIC 9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-KEY.
                   20  'XX'-VNR.
                       25 'XX'-FILIALNUMMER       PIC 9(003).
                       25 'XX'-KUNDENNUMMER       PIC 9(007).
                   20 'XX'-UNTERKONTONUMMER       PIC 9(002).
      *
               15  'XX'-DATUM-EROEFFNUNG.
                   20 'XX'-JAHR-EROEFFNUNG        PIC 9(004).
                   20 'XX'-NMONAT-EROEFFNUNG      PIC 9(002).
                   20 'XX'-TAG-EROEFFNUNG         PIC 9(002).
      *
               15  'XX'-DATUM-SCHLIESSUNG.
                   20 'XX'-JAHR-SCHLIESSUNG       PIC 9(004).
                   20 'XX'-MONAT-SCHLIESSUNG      PIC 9(002).
                   20 'XX'-TAG-SCHLIESSUNG        PIC 9(002).
      *
               15  'XX'-KONTOART                  PIC X(003).
      *            SPL - Bonussparen (Bilanzschlüssel 3100)
      *            FZS - Festzinssparen (Bilanzschlüssel 3070)
      *
               15  'XX'-SALDO                     PIC S9(012)V99.
      *
               15  'XX'-VERTRAGSENDE.
                   20 'XX'-JAHR-VERTRAGSENDE      PIC 9(004).
                   20 'XX'-MONAT-VERTRAGSENDE     PIC 9(002).
                   20 'XX'-TAG-VERTRAGSENDE       PIC 9(002).
      *            Dieses Datum kann beim Festzinssparen revolvierend
      *            sein.
      *
290808         15  'XX'-ZAHLUNGSWEISE             PIC 9(001).
290808*            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
290808*            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
290808*
290808         15  'XX'-RATENHOEHE                PIC S9(012)V99.
290808*            SPARRATE
290808*
290808         15  'XX'-HABENZINS                 PIC S9(02)V9999.
290808*            HABENZINSSATZ
290808*

140704         15  'XX'-DEL-KZ                    PIC X(001).
140704*            Delete-Kennzeichen

080713         15  'XX'-IBAN                      PIC X(034).
>>    *            IBAN zur Lastschrift für den Rateneinzug

               15  'XX'-BIC                       PIC X(011).
      *            BIC zur Lastschrift für den Rateneinzug

               15  'XX'-MANDREF                   PIC X(035).
      *            Mandatsreferenz

               15  'XX'-NACHNAME-BZ               PIC X(028).
      *            Kontoinhaber des Belastungskontos zur Lastschrift
      *            für den Rateneinzug

               15  'XX'-IBAN-SPK                  PIC X(034).
      *            IBAN des Sparkontos
010917         15  'XX'-PIN                       PIC 9(009).

    <<         15  'XX'-RESERVE                   PIC X(020).
      *
      ******************************************************************
      *                                                                *
      *                 VORSATZ                                        *
      *                                                                *
      ******************************************************************
      *
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
      *            Komplettlieferung 'SPARK'
               15  'XX'-V-WKZ                     PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
080713         15  FILLER                         PIC X(217).
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
080713         15  FILLER                         PIC  X(241).


