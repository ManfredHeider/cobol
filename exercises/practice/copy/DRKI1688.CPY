      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1688.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1688.CPv  $
      *
      *   Rev 2.7   Mar 06 2018 14:29:12   t13ex39
      *FKI01564: Fehlende Attr. im DTA von Kontokorrent und Baufi
      *
      *   Rev 2.6   Mar 06 2018 08:35:56   t13ex39
      *FKI01564: Fehlende Attr. im DTA von Kontokorrent und Baufi
      *
      *   Rev 2.5   Jan 25 2018 07:21:04   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.4   Sep 03 2013 11:09:50   t13ex20
      *AKI01204: SEPA - DB-Online DeuBa KK
      *
      *   Rev 2.3   Jul 25 2006 17:30:44   t13ex27
      *AKI00587: Deutsche Bank. Maskenanpassung zu Kontokorrentkont
      *en Kontokorrent
      *
      *   Rev 2.2   Mar 16 2005 11:46:26   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:34:36   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:38:44   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Kontokorrentdaten der        *
      *                Deutschen Bank HISTORIE                         *
      *                                                                *
      *    SATZLÄNGE 312                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00082!11.06.04! Neuerstellung                    !WHO        *
      *--------!--------!----------------------------------!-----------*
      *FKI00992!10.03.05! Tagesdatum der Löschung          !WHO        *
      *--------!--------!----!-----------------------------!-----------*
      *AKI00587!16.06.06!2.3 ! Neue Felder                 !IFR        *
      *--------------------------------------------------------------*
280218* FKI01564!28.02.18! Fehler in COPY DRKI1112        ! WHO      *
      *         !        ! Attribut KOSTEN-SCHL fehlt     !          *
      *         !        !                                !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1688.
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
               10 KSDS-LFDNR                  PIC  9(002).
      *           LFD. NR. FÜR PRIMARY-KEY
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
100305     05  KSDS-AEND-DAT                  PIC  X(010).
100305*         ÄNDERUNGSDATUM
020813     05  KSDS-IBAN                      PIC X(034).
>>    *        IBAN zur Lastschrift für den Rateneinzug
           05  KSDS-BIC                       PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug
           05  KSDS-MANDREF                   PIC X(035).
      *        Mandatsreferenz
           05  KSDS-KONTOINH                  PIC X(028).
      *        Kontoinhaber Belastungskontot
      *        für den Rateneinzug
    <<     05  KSDS-IBAN-KK                   PIC X(034).
020813*            IBAN des PK-Kontos

280218     05  KSDS-KOSTEN-SCHL               PIC X(001).

101217*    05  KSDS-PIN                       PIC 9(009).

280218     05  KSDS-RESERVE                   PIC X(003).
      *            RESERVE



