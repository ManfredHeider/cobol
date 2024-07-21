      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1514.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1514.CPv  $
      *
      *   Rev 2.5   Nov 29 2018 09:38:52   t13ae52
      *AKI01645: G48  -  Sourcen/COPYs
      *
      *   Rev 2.4   Jan 25 2018 07:20:26   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs BANKPRODUKT
      *
      *   Rev 2.3   Sep 02 2013 11:49:40   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Baufinanzierung
      *
      *   Rev 2.2   Nov 05 2008 10:52:02   t13ex50
      *AKI00609: Deutsche Bank. Maskenanpassung und neue Produkte
      *
      *   Rev 2.1   Jul 13 2003 23:26:22   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Ausgabedatei Baufi-Daten der            *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 220                                               *
110713*    SATZLÄNGE 370                                               *
      *    SATZLÄNGE 400                                               *
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
      *----------------------------------------------------------------*
101217*AKI01556!10.12.17! PIN verarbeiten                  ! WHO       *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *--------!--------!----------------------------------!-----------*
      *AKI01645!14.11.18! NEUES FELD BNK-DATUM-VOLLAUSZ UND!SCHUMACHER *
      *        !        ! DEN FILLER ANGEPASST             !           *
      *--------!--------!----------------------------------!-----------*
      *DBO9399 !16.11.21! Aufnahme Forward-Kennzeichen     !t13ex78    *
      *        !        !                                  !           *
      *--------!--------!----------------------------------!-----------*
      *DBO-9707!04.02.22! Erweiterung um neues Feld        ! u130685   *
      *        !        ! -DATUM-FAELLIGKEIT               !           *
      *        !        ! Erweiterung Satzlänge auf 400    !           *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1514.
      *
           05  BNK-SA                         PIC X(002).
      *            SATZART        = '14'
           05  BNK-GES                        PIC 9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER        PIC 9(003).
                   15 BNK-KUNDENNUMMER        PIC 9(007).
               10 BNK-UNTERKONTONUMMER        PIC 9(002).
               10 BNK-WAEHRUNG                PIC 9(003).
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
      *            J = ÄNDERUNG DATUM SCHLIESSUNG
      *
           05  BNK-DARLEHEN                   PIC S9(012)V99.
      *        URSPRUNGSKREDITBETRAG
           05  BNK-AEND-DARLEHEN-KZ           PIC X(001).
      *            J = ÄNDERUNG URSPRÜNGLICHER DARLEHENSBETRAG
      *

090908*
>>>>>>     05  BNK-DATUM-KREDITZUSAGE.
               10 BNK-JAHR-KREDITZUSAGE       PIC 9(004).
               10 BNK-MONAT-KREDITZUSAGE      PIC 9(002).
               10 BNK-TAG-KREDITZUSAGE        PIC 9(002).
      *
           05  BNK-AEND-DAT-KREDZUS-KZ        PIC X(001).
      *            J = ÄNDERUNG DATUM KREDITZUSAGE
      *
           05  BNK-DATUM-KREDITAUSZAHLUNG.
               10 BNK-JAHR-KREDITAUSZAHLUNG   PIC 9(004).
               10 BNK-MONAT-KREDITAUSZAHLUNG  PIC 9(002).
               10 BNK-TAG-KREDITAUSZAHLUNG    PIC 9(002).
      *
           05  BNK-AEND-DAT-KREDAUS-KZ        PIC X(001).
      *            J = ÄNDERUNG DATUM KREDITAUSZAHLUNG
      *
           05  BNK-DATUM-ENDEZINSFS.
               10 BNK-JAHR-ENDEZINSFS         PIC 9(004).
               10 BNK-MONAT-ENDEZINSFS        PIC 9(002).
               10 BNK-TAG-ENDEZINSFS          PIC 9(002).
      *                DATUM ENDE DER ZINSFESTSCHREIBUNG
      *
           05  BNK-AEND-DAT-ENDEZFS-KZ        PIC X(001).
      *            J = ÄNDERUNG DATUM ENDE ZINSFESTSCHREIBUNG
      *
           05  BNK-AKTUELLER-SALDO            PIC S9(012)V99.
      *                AKTUELLER SALDO
      *
           05  BNK-AEND-AKT-SALDO-KZ          PIC X(001).
      *            J = ÄNDERUNG DES AKTUELLEN SALDOS
      *
           05  BNK-DATUM-ENDEZRFREI.
               10 BNK-JAHR-ENDEZRFREI         PIC 9(004).
               10 BNK-MONAT-ENDEZRFREI        PIC 9(002).
               10 BNK-TAG-ENDEZRFREI          PIC 9(002).
      *                ENDE ZEITRAUM FREIJAHRE
      *
           05  BNK-AEND-DAT-ZRFREI-KZ         PIC X(001).
      *            J = ÄNDERUNG DATUM ZEITRAUM FREIJAHRE
      *
           05  BNK-TILGUNGSART                PIC  X(001).
      *                TILGUNGSART A=ANNITÄTISCH E=ENDFÄLLIG
      *
           05  BNK-AEND-TILGUNGSART-KZ        PIC X(001).
      *            J = ÄNDERUNG DER TILGUNGSART
      *
           05  BNK-TILGUNGSPROZENT            PIC S9(02)V9999.
      *                TILGUNG IN PROZENT
      *
           05  BNK-AEND-TILG-PROZ-KZ          PIC X(001).
      *            J = ÄNDERUNG DES TILGUNGSPROZENTSATZES
      *
           05  BNK-TILGUNGSPRODUKT            PIC  X(010).
      *                LV, BSV ...
      *
           05  BNK-AEND-TILG-PROD-KZ          PIC X(001).
      *            J = ÄNDERUNG DES TILGUNGSPRODUKTES
      *
           05  BNK-BEITRAG-TILGSATZ           PIC S9(012)V99.
      *                BEITRAG FÜR TILGUNGSERSATZ IN EURO
      *
           05  BNK-AEND-TILG-SATZ-KZ          PIC X(001).
      *            J = ÄNDERUNG DES TILGUNGSSATZES IN EURO
      *
           05  BNK-SOLLZINSSATZ               PIC S9(02)V9999.
      *                SOLLZINSSATZ IN PROZENT
      *
           05  BNK-AEND-SOLLZINS-KZ           PIC X(001).
      *            J = ÄNDERUNG DES SOLLZINSSATZES
      *
           05  BNK-UEBERZZINSS                PIC S9(02)V9999.
      *                ÜBERZIEHUNGSZINSSATZ IN PROZENT
      *
           05  BNK-AEND-UEBERZZINS-KZ         PIC X(001).
      *            J = ÄNDERUNG DES ÜBERZIEHUNGSZINSES
      *
           05  BNK-ZAHLUNGSWEISE              PIC  9(001).
      *            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
      *            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
      *
           05  BNK-AEND-ZAHLWEISE-KZ          PIC X(001).
      *            J = ÄNDERUNG DER ZAHLUNGSWEISE
      *
           05  BNK-ANNUITAET                  PIC S9(012)V99.
      *                RATE IN EURO
      *
           05  BNK-AEND-ANNUITAET-KZ          PIC X(001).
      *            J = ÄNDERUNG DER AKTUELLEN RATE
      *
           05  BNK-ANTEIL-RATE                PIC S9(012)V99.
      *                TILGUNGSANTEIL PRO RATE IN EURO
      *
           05  BNK-AEND-ANT-RATE-KZ           PIC X(001).
      *            J = ÄNDERUNG DES TILGUNGSANTEILS EINER RATE
      *
           05  BNK-ANTEIL-ZINS                PIC S9(012)V99.
      *                ZINSANTEIL PRO RATE IN EURO
      *
<<<<<<     05  BNK-AEND-ANT-ZINS-KZ           PIC X(001).
090908*            J = ÄNDERUNG DES ZINSANTEILS EINER RATE
      *
110713     05  BNK-IBAN                       PIC X(034).
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

           05  BNK-IBAN-BF                    PIC X(034).
      *        IBAN des Baufinanzierungs-Kontos

           05  BNK-AEND-IBAN-BF-KZ            PIC X(001).
<<    *        J WENN IBAN BF-Konto GEÄNDERT

101217     05  BNK-PIN                        PIC 9(009).
101217     05  BNK-AEND-PIN-KZ                PIC X(001).

           05  BNK-DATUM-VOLLAUSZ.
               10 BNK-JAHR-VOLLAUSZ           PIC 9(004).
               10 BNK-MONAT-VOLLAUSZ          PIC 9(002).
               10 BNK-TAG-VOLLAUSZ            PIC 9(002).
      *        DATUM DER VOLLAUSZAHLUNG
 
           05  BNK-AEND-DATUM-VOLLAUSZ-KZ     PIC X(001).
      *        J WENN DATUM VOLLAUSZAHLUNG GEÄNDERT
      *
           05  BNK-FORWARD-KZ                 PIC X(001).
           05  BNK-AEND-FORWARD-KZ            PIC X(001).
      *
           05  BNK-DATUM-FAELLIGKEIT.
               10 BNK-JAHR-FAELLIGKEIT        PIC 9(004).
               10 BNK-MONAT-FAELLIGKEIT       PIC 9(002).
               10 BNK-TAG-FAELLIGKEIT         PIC 9(002).
      *        DATUM DER NÄCHSTEN RATENFÄLLIGKEIT
           05  BNK-AEND-FAELLIGDAT-KZ         PIC X(001).
      *        J WENN DATUM DER NÄCHSTEN RATENFÄLLIGKEIT GEÄNDERT
      *    
           05  BNK-RESERVE                    PIC X(023).
      *    


