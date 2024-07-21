      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0173.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/MF/CPY/DRKI0173.CPv  $
      *
      *   Rev 2.8   Nov 26 2002 12:38:38   t13ex39
      *AKI00315 STAND GUTHABEN
      *
      *   Rev 2.7   Nov 26 2002 12:14:52   t13ex39
      *AKI00315  STAND GUTHABEN
      *
      *   Rev 2.6   Mar 07 2002 17:43:42   t13ex12
      *AKI00162 Neue Felder von DIT
      *
      *   Rev 2.5   Dec 01 1999 16:30:06   T13EX39
      *AKI00051 WKZ
      *
      *   Rev 2.4   Nov 25 1999 15:58:06   T13EX39
      *AKI00051    WKZ
      *
      *   Rev 2.3   Nov 25 1999 14:12:30   T13EX39
      *AKI00051    WKZ
      *
      *   Rev 2.2   Nov 23 1999 15:00:24   T13EX50
      *Erweitert um das Feld BA-BUCH-ANTEILE
      *
      *   Rev 2.1   Apr 22 1999 13:33:04   T13EX39
      *FKI00018 Feld für  Kontoart
      *
      *   Rev 1.1   09 Feb 1999 11:06:18   T13EX12
      *aki00014 - Aufbereitete Bank-Anteilsdaten
      *
      *   Rev 1.0   29 Sep 1997 11:54:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 1 7 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE   von 131 in 140 geändert (19.4.99)               *
      *    SATZLÄNGE   von 140 in 169 geändert (24.11.99)               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *28.05.98!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *23.09.98!Buchungsschl. mit Vorzeichen           !V.D.GOLTZ! 01  *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *14.04.99!Feld für Kontoart (DE, VL) definieren. !V.D.GOLTZ! 02  *
      *FKI00018!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *23.11.99!ERWEITERT UM DAS FELD BA-BUCH-ANTEILE  !H.HERWEG ! 03  *
      *--------!---------------------------------------!---------!-----*
      *22.11.01!Neue DIT-Felder, Änderungs-KZ für      !v.d.Goltz! 04  *
      *AKI00162!Anteilsdaten.                          !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.11.02!NEUES FELD STAND GUTHABEN              ! WHO     ! 02  *
      *AKI00315!                                       !         !     *
      *        !                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0173.
           05  BA-GES                        PIC  9(02).
           05  BA-VNR                        PIC  9(15).
           05  BA-ANTEIL-LFDNR               PIC  9(02).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER

           05  BA-WP-KENN-NR                 PIC  X(06).
      *        FUER DIT: WERTPAPIERKENNNUMMER
      *        FUER III UND RESTLICHE BANKEN: DVAG-PRODUKTSCHLUSSEL

           05  BA-GUTHABEN                   PIC  9(11)V99.
           05  BA-ANTEILE                    PIC  9(06)V999.
           05  BA-EINZ-KUM                   PIC  9(13).
      *        KUMULIERTE EINZAHLUNGEN

           05  BA-AUSZ-KUM                   PIC  9(13).
      *        KUMULIERTE AUSZAHLUNGEN

           05  BA-WERT-GES                   PIC  9(03)V99.
      *        WERTENTWICKLUNG GESAMT

           05  BA-WERT-JAHR                  PIC  9(03)V99.
      *        WERTENTWICKLUNG FUER DAS LFD JAHR

           05  BA-BUCH-BETRAG                PIC  9(07)V99.
           05  BA-BUCH-DATUM                 PIC  X(10).
230998*    05  BA-BUCH-SCHLUESSEL            PIC  9(02).
230998     05  BA-BUCH-SCHLUESSEL            PIC S9(02).
           05  BA-BUCH-STEUERBETRAG          PIC  9(07)V99.
231199     05  BA-BUCH-ANTEILE               PIC  9(06)V999.

           05  BA-FREI-VERBRAUCHT            PIC  9(05)V99.
      *        VERBRAUCHTER FREISTELLUNGSBETRAG
           05  BA-AEND-FREI-VERBRAUCHT-KZ    PIC  X(01).
      *        J WENN VERBRAUCHTER FREISTELLUNGAUFTRAG GEAENDERT

           05  BA-ZINS-AB-ST                 PIC  9(06)V99.
      *        ZINSABSCHLAGSTEUER
           05  BA-AEND-ZINS-AB-ST-KZ         PIC  X(01).
      *        J WENN ZINABSCHLAGSTEUER GEAENDERT
190499     05  BA-VL-KZ                      PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)

221101     05  BA-ZW-GEWINN                  PIC S9(005)V99.
...   *        Zwischengewinn Guthaben
...        05  BA-AEND-ZW-GEWINN-KZ          PIC  X(01).
...   *        J WENN Zwischengewinn GEAENDERT
...        05  BA-UMSATZ-NETTO               PIC  9(007)V99.
...   *        Nettobetrag = Rücknahmepreis x Anteile
...
...        05  BA-AEND-ANTEIL-KZ             PIC  X(01).
221101*        J wenn eins der Felder in GuthabenAnteil geaendert


221101*    05  FILLER                        PIC  X(28).
051102*    05  FILLER                        PIC  X(010).

051102     05  BA-GUTHABEN-DATUM-VON         PIC  X(10).
           05  BA-AKTUAL-KZ                  PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

