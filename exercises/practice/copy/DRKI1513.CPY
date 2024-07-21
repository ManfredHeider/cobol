      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1513.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1513.CPv  $
      *
      *   Rev 2.7   Nov 29 2018 09:37:02   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.6   Mar 06 2018 08:35:36   t13ex39
      *FKI01564: Fehlende Attr. im DTA von Kontokorrent und Baufi
      *
      *   Rev 2.5   Jan 25 2018 07:20:12   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.4   Feb 04 2014 12:46:46   t13ex39
      *AKI01204: SEPA - DB-Online
      *
      *   Rev 2.3   Sep 02 2013 11:49:36   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Baufinanzierung
      *
      *   Rev 2.2   Nov 05 2008 10:51:56   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.1   Jul 13 2003 23:26:58   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Baufi-Daten der              *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 200                                               *
110713*    SATZLÄNGE 350                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
090908*AKI00609!09.09.08!2.2 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
090908*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
110713*AKI01204!11.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
110713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *--------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO      *
      *--------------------------------------------------------------*
280218* FKI01564!28.02.18! Fehler in COPY DRKI1513        ! WHO        *
      *         !        ! Attribut Zinsart und Forward-  !            *
      *         !        ! Kennzeichen fehlten            !            *
      *         !        ! neues Datumsfeld TEIL-AUSZ-DAT !            *
      *         !        !                                !            *
      *---------!--------!--------------------------------!------------*
      *AKI01645 !14.11.18! UMBENENNUNG KSDS-TEIL-AUSZ-DAT !SCHUMACHER  *
      *         !        ! NACH KSDS-DATUM-VOLLAUSZ       !            *
      *         !        ! KORREKTUR FILLER               !            *
      *---------!--------!--------------------------------!------------*
      * ENDE-LOGBUCH                                                 *
      ******************************************************************
      *
       01  DRKI1513.
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
      *        URSPRUNGSKREDITBETRAG
090908*
>>>>>>     05  KSDS-DATUM-KREDITZUSAGE.
               10 KSDS-JAHR-KREDITZUSAGE        PIC 9(004).
               10 KSDS-MONAT-KREDITZUSAGE       PIC 9(002).
               10 KSDS-TAG-KREDITZUSAGE         PIC 9(002).
      *
           05  KSDS-DATUM-KREDITAUSZAHLUNG.
               10 KSDS-JAHR-KREDITAUSZAHLUNG    PIC 9(004).
               10 KSDS-MONAT-KREDITAUSZAHLUNG   PIC 9(002).
               10 KSDS-TAG-KREDITAUSZAHLUNG     PIC 9(002).
      *
           05  KSDS-DATUM-ENDEZINSFS.
               10 KSDS-JAHR-ENDEZINSFS          PIC 9(004).
               10 KSDS-MONAT-ENDEZINSFS         PIC 9(002).
               10 KSDS-TAG-ENDEZINSFS           PIC 9(002).
      *                DATUM ENDE DER ZINSFESTSCHREIBUNG
      *
           05  KSDS-AKTUELLER-SALDO             PIC S9(012)V99.
      *                AKTUELLER SALDO
      *
           05  KSDS-DATUM-ENDEZRFREI.
               10 KSDS-JAHR-ENDEZRFREI          PIC 9(004).
               10 KSDS-MONAT-ENDEZRFREI         PIC 9(002).
               10 KSDS-TAG-ENDEZRFRREI          PIC 9(002).
      *                ENDE ZEITRAUM FREIJAHRE

           05  KSDS-TILGUNGSART                 PIC  X(001).
      *                TILGUNGSART A=ANNITÄTISCH E=ENDFÄLLIG
      *
           05  KSDS-TILGUNGSPROZENT             PIC S9(02)V9999.
      *                TILGUNG IN PROZENT
      *
           05  KSDS-TILGUNGSPRODUKT             PIC  X(010).
      *                LV, BSV ...
      *
           05  KSDS-BEITRAG-TILGSATZ            PIC S9(012)V99.
      *                BEITRAG FÜR TILGUNGSERSATZ IN EURO
      *
           05  KSDS-SOLLZINSSATZ                PIC S9(02)V9999.
      *                SOLLZINSSATZ IN PROZENT
      *
           05  KSDS-UEBERZZINSS                 PIC S9(02)V9999.
      *                ÜBERZIEHUNGSZINSSATZ IN PROZENT
      *
           05  KSDS-ZAHLUNGSWEISE               PIC  9(001).
      *            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
      *            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
      *
           05  KSDS-ANNUITAET                   PIC S9(012)V99.
      *                RATE IN EURO
      *
           05  KSDS-ANTEIL-RATE                 PIC S9(012)V99.
      *                TILGUNGSANTEIL PRO RATE IN EURO
      *
<<<<<<     05  KSDS-ANTEIL-ZINS                 PIC S9(012)V99.
090908*                ZINSANTEIL PRO RATE IN EURO
      *
110713     05  KSDS-DEL-KZ                      PIC X(001).
>>
           05  KSDS-IBAN                        PIC X(034).
      *        IBAN zur Lastschrift für den Rateneinzug

           05  KSDS-BIC                         PIC X(011).
      *        BIC zur Lastschrift für den Rateneinzug

           05  KSDS-MANDREF                     PIC X(035).
      *        Mandatsreferenz

           05  KSDS-NACHNAME-BZ                 PIC X(028).
      *        Kontoinhaber des Belastungskontos zur Lastschrift
      *        für den Rateneinzug

           05  KSDS-IBAN-BF                     PIC X(034).
      *        IBAN des Baufinanzierungs-Kontos

280218     05  KSDS-ZINSART                     PIC X(002).

280218     05  KSDS-FORWARD-KZ                  PIC X(001).

101217     05  KSDS-PIN                         PIC 9(009).

           05  KSDS-DATUM-VOLLAUSZ.
               10 KSDS-JAHR-VOLLAUSZ            PIC 9(004).
               10 KSDS-MONAT-VOLLAUSZ           PIC 9(002).
               10 KSDS-TAG-VOLLAUSZ             PIC 9(002).
      *        DATUM DER VOLLAUSZAHLUNG

280218*    05  KSDS-RESERVE                     PIC X(017).
           05  KSDS-RESERVE                     PIC X(006).
      *            RESERVE



