      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1512.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI1512.CPv  $
      *
      *   Rev 2.8   Nov 29 2018 09:35:18   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.7   Mar 06 2018 08:35:26   t13ex39
      *FKI01564: Fehlende Attr. im DTA von Kontokorrent und Baufi
      *
      *   Rev 2.6   Jan 25 2018 07:19:58   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.5   Mar 09 2016 14:44:12   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.4   Sep 02 2013 11:49:30   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Baufinanzierung
      *
      *   Rev 2.3   Nov 05 2008 10:51:52   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.2   Aug 18 2004 10:50:40   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank
      *
      *   Rev 2.1   Jul 13 2003 23:27:30   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand der       *
      *                Baufi-Daten von der Deutschen Bank              *
      *                                                                *
      *    SATZLÄNGE 200                                               *
110713*    SATZLÄNGE 350  
      *    SATZLÄNGE 400  
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *--------!--------!----------------------------------!-----------*
      *AKI00249!14.07.04!      Löschkennzeichen 'D' ver-   !WHO        *
      *        !        !      arbeiten                    !           *
      *----------------------------------------------------------------*
090908*AKI00609!09.09.08!2.3 !NEUE FELDER EINGEFÜGT        !SCHLEICHER *
090908*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
110713*AKI01204!11.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
110713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO       *
      *----------------------------------------------------------------*
280218* FKI01564!28.02.18! Fehler in COPY                 ! WHO        *
      *         !        ! Attribut Zinsart und Forward-  !            *
      *         !        ! Kennzeichen fehlen             !            *
      *         !        ! neues Datumsfeld TEIL-AUSZ-DAT !            *
      *---------!--------!--------------------------------!------------*
      *AKI01645 !14.11.18! UMBENENNUNG TEIL-AUSZ-DAT NACH !SCHUMACHER  *
      *         !        ! DATUM-VOLLAUSZ                 !            *
      *---------!--------!--------------------------------!------------*
      *DBO-9707 !04.02.22! Erweiterung um neues Feld      ! u130685    *
      *         !        ! -DATUM-FAELLIGKEIT             !            *
      *         !        ! Erweiterung Satzlänge auf 400  !            *
      *         !        !                                !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1512.
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ.
      *
               15  'XX'-SA                        PIC X(002).
      *            SATZART        = '14'
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
      *             Ursprungskreditbetrag
090908*
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
               15  'XX'-DATUM-ENDEZINSFS.
                   20 'XX'-JAHR-ENDEZINSFS        PIC 9(004).
                   20 'XX'-MONAT-ENDEZINSFS       PIC 9(002).
                   20 'XX'-TAG-ENDEZINSFS         PIC 9(002).
      *                DATUM ENDE DER ZINSFESTSCHREIBUNG
      *
               15  'XX'-AKTUELLER-SALDO           PIC S9(012)V99.
      *                AKTUELLER SALDO
      *
               15  'XX'-DATUM-ENDEZRFREI.
                   20 'XX'-JAHR-ENDEZRFREI        PIC 9(004).
                   20 'XX'-MONAT-ENDEZRFREI       PIC 9(002).
                   20 'XX'-TAG-ENDEZRFRREI        PIC 9(002).
      *                ENDE ZEITRAUM FREIJAHRE
 
               15  'XX'-TILGUNGSART               PIC  X(001).
      *                TILGUNGSART A=ANNITÄTISCH E=ENDFÄLLIG
      *
               15  'XX'-TILGUNGSPROZENT           PIC S9(02)V9999.
      *                TILGUNG IN PROZENT
      *
               15  'XX'-TILGUNGSPRODUKT           PIC  X(010).
      *                LV, BSV ...
      *
               15  'XX'-BEITRAG-TILGSATZ          PIC S9(012)V99.
      *                BEITRAG FÜR TILGUNGSERSATZ IN EURO
      *
               15  'XX'-SOLLZINSSATZ              PIC S9(02)V9999.
      *                SOLLZINSSATZ IN PROZENT
      *
               15  'XX'-UEBERZZINSS               PIC S9(02)V9999.
      *                ÜBERZIEHUNGSZINSSATZ IN PROZENT
      *
               15  'XX'-ZAHLUNGSWEISE             PIC  9(001).
      *            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
      *            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
      *
               15  'XX'-ANNUITAET                 PIC S9(012)V99.
      *                RATE IN EURO
      *
               15  'XX'-ANTEIL-RATE               PIC S9(012)V99.
      *                TILGUNGSANTEIL PRO RATE IN EURO
      *
<<<<<<         15  'XX'-ANTEIL-ZINS               PIC S9(012)V99.
090908*                ZINSANTEIL PRO RATE IN EURO
      *
140704         15  'XX'-DEL-KZ                    PIC X(001).
      *
110713         15  'XX'-IBAN                      PIC X(034).
>>    *            IBAN zur Lastschrift für den Rateneinzug
 
               15  'XX'-BIC                       PIC X(011).
      *            BIC zur Lastschrift für den Rateneinzug
 
               15  'XX'-MANDREF                   PIC X(035).
      *            Mandatsreferenz
 
               15  'XX'-NACHNAME-BZ               PIC X(028).
      *            Kontoinhaber Belastungskontos zur Lastschrift
      *            für den Rateneinzug
 
<<             15  'XX'-IBAN-BF                   PIC X(034).
      *            IBAN des Baufinanzierungs-Kontos
280218         15  'XX'-ZINSART                   PIC X(002).

280218         15  'XX'-FORWARD-KZ                PIC X(001).

101217         15  'XX'-PIN                       PIC 9(009).

               15  'XX'-DATUM-VOLLAUSZ.
                   20 'XX'-JAHR-VOLLAUSZ          PIC 9(004).
                   20 'XX'-MONAT-VOLLAUSZ         PIC 9(002).
                   20 'XX'-TAG-VOLLAUSZ           PIC 9(002).
      *            DATUM DER VOLLAUSZAHLUNG

               15  'XX'-DATUM-FAELLIGKEIT.
                   20 'XX'-JAHR-FAELLIGKEIT       PIC 9(004).
                   20 'XX'-MONAT-FAELLIGKEIT      PIC 9(002).
                   20 'XX'-TAG-FAELLIGKEIT        PIC 9(002).
      *            DATUM DER NÄCHSTEN RATENFÄLLIGKEIT

280218*        15  'XX'-RESERVE                   PIC X(017).
      *        15  'XX'-RESERVE                   PIC X(006).
               15  'XX'-RESERVE                   PIC X(048).
      *
      ******************************************************************
      *                                                                *
      *        VORSATZ                                                 *
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
      *            Komplettlieferung 'BAUFIK'
               15  'XX'-V-WKZ                     PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
               15  FILLER                         PIC X(367).
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
               15  FILLER                         PIC  X(391).
 




