      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1691.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI1691.CPv  $
      *
      *   Rev 2.9   Nov 29 2018 09:40:36   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.8   Mar 06 2018 08:35:46   t13ex39
      *FKI01564: Fehlende Attr. im DTA von Kontokorrent und Baufi
      *
      *   Rev 2.7   Jan 25 2018 07:21:20   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.6   Feb 04 2014 12:47:02   t13ex39
      *AKI01204: SEPA - DB-Online
      *
      *   Rev 2.5   Sep 02 2013 11:49:46   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Baufinanzierung
      *
      *   Rev 2.4   Nov 11 2008 13:57:04   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.3   Nov 05 2008 10:52:40   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.2   Mar 16 2005 11:46:50   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:35:00   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:39:30   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Baufi-Daten der              *
      *                Deutschen Bank HISTORIE                         *
      *                                                                *
      *    SATZLÄNGE 202                                               *
110713*    SATZLÄNGE 352                                               *
280218*    SATZLÄNGE 406                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00082!11.06.04! Neuerstellung                    !WHO        *
      *-----------------!----------------------------------!-----------*
      *FKI00992!10.03.05! Tagesdatum der Löschung          !WHO        *
      *----------------------------------------------------------------*
090908*AKI00609!09.09.08!2.3 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
090908*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
      *AKI00609!11.11.08!2.4 !FELDREIHENFOLGE VERÄNDERT,   !SCHLEICHER *
      *        !        !    !  SONST MUSS IN DER KSDS GE- !           *
      *        !        !    !  SCHOBEN WERDEN             !           *
      *----------------------------------------------------------------*
110713*AKI01204!13.06.13!2.5 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
110713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *--------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO      *
      *        !        !                                  !          *
      *--------------------------------------------------------------*
280218* FKI01564!28.02.18! Fehler in COPY DRKI1513        ! WHO      *
      *         !        ! Attribut Zinsart und Forward-  !          *
      *         !        ! Kennzeichen fehlten            !          *
      *         !        ! ---> recompile                 !          *
      *         !        ! neues Datumsfeld TEIL-AUSZ-DAT !          *
      *         !        ! Datei vergrößert               !          *
      *---------!--------!--------------------------------!------------*
      *AKI01645 !14.11.18! UMBENENNUNG KSDS-TEIL-AUSZ-DAT !SCHUMACHER  *
      *         !        ! NACH KSDS-DATUM-VOLLAUSZ       !            *
      *---------!--------!--------------------------------!------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1691.
      *
           05  KSDS-SA                        PIC  X(002).
      *            SATZART        = '14'
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
100305     05  KSDS-AEND-DAT                  PIC  X(10).
100305*         ÄNDERUNGSDATUM
      *
090908*
>>>>>>     05  KSDS-DATUM-KREDITZUSAGE.
               10 KSDS-JAHR-KREDITZUSAGE      PIC 9(004).
               10 KSDS-MONAT-KREDITZUSAGE     PIC 9(002).
               10 KSDS-TAG-KREDITZUSAGE       PIC 9(002).
      *
           05  KSDS-DATUM-KREDITAUSZAHLUNG.
               10 KSDS-JAHR-KREDITAUSZAHLUNG  PIC 9(004).
               10 KSDS-MONAT-KREDITAUSZAHLUNG PIC 9(002).
               10 KSDS-TAG-KREDITAUSZAHLUNG   PIC 9(002).
      *
           05  KSDS-DATUM-ENDEZINSFS.
               10 KSDS-JAHR-ENDEZINSFS        PIC 9(004).
               10 KSDS-MONAT-ENDEZINSFS       PIC 9(002).
               10 KSDS-TAG-ENDEZINSFS         PIC 9(002).
      *                DATUM ENDE DER ZINSFESTSCHREIBUNG
      *
           05  KSDS-AKTUELLER-SALDO           PIC S9(012)V99.
      *                AKTUELLER SALDO
      *
           05  KSDS-DATUM-ENDEZRFREI.
               10 KSDS-JAHR-ENDEZRFREI        PIC 9(004).
               10 KSDS-MONAT-ENDEZRFREI       PIC 9(002).
               10 KSDS-TAG-ENDEZRFRREI        PIC 9(002).
      *                ENDE ZEITRAUM FREIJAHRE

           05  KSDS-TILGUNGSART               PIC  X(001).
      *                TILGUNGSART A=ANNITÄTISCH E=ENDFÄLLIG
      *
           05  KSDS-TILGUNGSPROZENT           PIC S9(02)V9999.
      *                TILGUNG IN PROZENT
      *
           05  KSDS-TILGUNGSPRODUKT           PIC  X(010).
      *                LV, BSV ...
      *
           05  KSDS-BEITRAG-TILGSATZ          PIC S9(012)V99.
      *                BEITRAG FÜR TILGUNGSERSATZ IN EURO
      *
           05  KSDS-SOLLZINSSATZ              PIC S9(02)V9999.
      *                SOLLZINSSATZ IN PROZENT
      *
           05  KSDS-UEBERZZINSS               PIC S9(02)V9999.
      *                ÜBERZIEHUNGSZINSSATZ IN PROZENT
      *
           05  KSDS-ZAHLUNGSWEISE             PIC  9(001).
      *            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
      *            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
      *
           05  KSDS-ANNUITAET                 PIC S9(012)V99.
      *                RATE IN EURO
      *
           05  KSDS-ANTEIL-RATE               PIC S9(012)V99.
      *                TILGUNGSANTEIL PRO RATE IN EURO
      *
<<<<<<     05  KSDS-ANTEIL-ZINS               PIC S9(012)V99.
090908*                ZINSANTEIL PRO RATE IN EURO
      *
110713     05  KSDS-DEL-KZ                    PIC X(001).
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

<<         05  KSDS-IBAN-BF                   PIC X(034).
      *        IBAN des Baufinanzierungs-Kontos

280218     05  KSDS-ZINSART                     PIC X(002).

280218     05  KSDS-FORWARD-KZ                  PIC X(001).

101217     05  KSDS-PIN                         PIC 9(009).

           05  KSDS-DATUM-VOLLAUSZ.
               10 KSDS-JAHR-VOLLAUSZ            PIC 9(004).
               10 KSDS-MONAT-VOLLAUSZ           PIC 9(002).
               10 KSDS-TAG-VOLLAUSZ             PIC 9(002).
      *        DATUM DER VOLLAUSZAHLUNG

280218     05  KSDS-RESERVE                     PIC X(050).
      *            RESERVE



