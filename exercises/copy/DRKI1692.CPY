      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1692.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1692.CPv  $
      *
      *   Rev 2.7   Jan 25 2018 07:21:32   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.6   Sep 02 2013 10:10:28   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.5   Sep 02 2013 10:01:36   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Privatkredit
      *
      *   Rev 2.4   Nov 17 2008 10:47:54   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.3   Nov 05 2008 10:52:46   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.2   Mar 16 2005 11:46:56   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:35:08   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:39:46   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Privatkredit Daten           *
      *                Deutschen Bank HISTORIE                         *
      *                                                                *
      *    SATZLÄNGE 152                                               *
130613*    SATZLÄNGE 302                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00082!11.06.04! Neuerstellung                    !WHO        *
      *--------!--------!----------------------------------!-----------*
      *FKI00992!10.03.05! Tagesdatum der Löschung          !WHO        *
      *----------------------------------------------------------------*
221008*AKI00609!22.10.08!2.3 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
221008*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
      *AKI00609!14.11.08!2.4 !FELDREIHENFOLGE VERÄNDERT,   !SCHLEICHER *
      *        !        !    !  SONST MUSS IN DER KSDS GE- !           *
      *        !        !    !  SCHOBEN WERDEN             !           *
      *----------------------------------------------------------------*
130613*AKI01204!13.06.13!2.5 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
130613*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *--------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1692.
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
               10  KSDS-LFDNR                 PIC  9(002).
      *           LFD. NR. FÜR PRIMARY-KEY
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
100305     05  KSDS-AEND-DAT                  PIC  X(010).
100305*         ÄNDERUNGSDATUM
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

           05  KSDS-NACHNAME-BZ               PIC X(028).
      *        Kontoinhaber Fremdbank zur Lastschrift
      *        für den Rateneinzug

           05  KSDS-MANDREF                   PIC X(035).
      *        Mandatsreferenz

           05  KSDS-IBAN-PK                   PIC X(034).
      *        IBAN des PK-Kontos
101217     05  KSDS-PIN                       PIC 9(009).
101217*    05  KSDS-RESERVE                   PIC X(034).
101217     05  KSDS-RESERVE                   PIC X(025).
      *            RESERVE


