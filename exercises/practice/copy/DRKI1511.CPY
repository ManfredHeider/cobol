      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1511.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1511.CPv  $
      *
      *   Rev 2.4   Jan 25 2018 07:19:44   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.3   Sep 02 2013 10:46:36   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Spardaten
      *
      *   Rev 2.2   Nov 05 2008 10:51:48   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.1   Jul 13 2003 23:28:04   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Ausgabedatei Änderungsbestand der       *
      *                Spardaten von der Deutschen Bank                *
      *                                                                *
      *    SATZLÄNGE 100                                               *
080713*    SATZLÄNGE 280                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
290808*AKI00609!29.08.08!2.2 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
290808*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
130613*AKI01204!13.06.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
130613*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO      *
      *        !        !                                  !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1511.
      *
           05  BNK-SA                         PIC X(002).
      *            SATZART        = '13'
           05  BNK-GES                        PIC 9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER        PIC 9(003).
                   15 BNK-KUNDENNUMMER        PIC 9(007).
               10  BNK-UNTERKONTONUMMER       PIC 9(002).
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
           05  BNK-KONTOART                   PIC X(003).
      *        SPL - Bonussparen (Bilanzschlüssel 3100)
      *        FZS - Festzinssparen (Bilanzschlüssel 3070)
      *
           05  BNK-AEND-KONTOART-KZ           PIC X(001).
      *            J = Änderung Kontoart
      *
           05  BNK-SALDO                      PIC S9(012)V99.
      *
           05  BNK-AEND-SALDO-KZ              PIC X(001).
      *            J = Änderung Saldo
      *
           05  BNK-VERTRAGSENDE.
               10 BNK-JAHR-VERTRAGSENDE       PIC 9(004).
               10 BNK-MONAT-VERTRAGSENDE      PIC 9(002).
               10 BNK-TAG-VERTRAGSENDE        PIC 9(002).
      *        Dieses Datum kann beim Festzinssparen revolvierend
      *        sein.
      *
           05  BNK-AEND-VERTRAGSENDE-KZ       PIC X(001).
      *            J = ÄNDERUNG VERTRAGSENDE
      *
290808     05  BNK-ZAHLUNGSWEISE              PIC 9(001).
290808*            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
290808*            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
290808*
290808     05  BNK-AEND-ZAHLUNGSWEISE-KZ      PIC X(001).
290808*            J = ÄNDERUNG ZAHLUNGSWEISE
290808*
290808     05  BNK-RATENHOEHE                 PIC S9(012)V99.
290808*            SPARRATE
290808*
290808     05  BNK-AEND-RATENHOEHE-KZ         PIC X(001).
290808*            J = ÄNDERUNG RATENHOEHE
290808*
290808     05  BNK-HABENZINS                  PIC S9(02)V9999.
290808*            HABENZINSSATZ
290808*
290808     05  BNK-AEND-HABENZINS-KZ          PIC X(001).
290808*            J = ÄNDERUNG HABENZINSSATZ
290808*
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
      *        Kontoinhaber des Belastungskontos zur Lastschrift
      *        für den Rateneinzug

           05  BNK-AEND-NACHNAME-BZ-KZ        PIC X(001).
      *        J WENN KONTOINHABER GEÄNDERT

           05  BNK-IBAN-SPK                   PIC X(034).
      *        IBAN des Sparkontos

           05  BNK-AEND-IBAN-SPK-KZ           PIC X(001).
      *        J WENN IBAN SPARKONTO GEÄNDERT
    <<
101217     05  BNK-PIN                       PIC  9(009).
101217     05  BNK-AEND-PIN-KZ               PIC  X(001).

101217     05  BNK-RESERVE                    PIC X(036).
101217*    05  BNK-RESERVE                    PIC X(046).
      *        RESERVE

