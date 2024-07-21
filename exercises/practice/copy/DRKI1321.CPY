      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1321.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1321.CPv  $
      *
      *   Rev 2.5   Mar 27 2017 09:01:48   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.4   Jan 27 2003 10:56:54   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.3   Nov 26 2002 12:38:24   t13ex39
      *AKI00315 STAND GUTHABEN
      *
      *   Rev 2.2   Nov 26 2002 12:15:32   t13ex39
      *AKI00315  STAND GUTHABEN
      *
      *   Rev 2.1   Feb 27 2002 12:00:18   t13ex39
      *AKI00267 Vertrags-ID für PMKI0116
      *
      *   Rev 2.0   Dec 14 2001 09:19:20   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 3 2 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE 182                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *10.02.02!NEUERSTELLUNG                          ! WHO     ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *05.11.02!NEUES FELD STAND GUTHABEN              ! WHO     ! 02  *
      *AKI00315!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *22.03.17! III entfernt                          ! HHU     ! 03  *
      *FKI01544!                                       !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1321.
           05  BA-VERT-ID                        PIC  X(13).
           05  BA-GESAMT.
               15  BA-GES                        PIC  9(02).
               15  BA-VNR                        PIC  9(15).
               15  BA-ANTEIL-LFDNR               PIC  9(02).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
 
               15  BA-WP-KENN-NR                 PIC  X(06).
      *        FUER DIT: WERTPAPIERKENNNUMMER
      *        FUER RESTLICHE BANKEN: DVAG-PRODUKTSCHLUSSEL
 
               15  BA-GUTHABEN                   PIC  9(11)V99.
               15  BA-ANTEILE                    PIC  9(06)V999.
               15  BA-EINZ-KUM                   PIC  9(13).
      *        KUMULIERTE EINZAHLUNGEN
 
               15  BA-AUSZ-KUM                   PIC  9(13).
      *        KUMULIERTE AUSZAHLUNGEN
 
               15  BA-WERT-GES                   PIC  9(03)V99.
      *        WERTENTWICKLUNG GESAMT
 
               15  BA-WERT-JAHR                  PIC  9(03)V99.
      *        WERTENTWICKLUNG FUER DAS LFD JAHR
 
               15  BA-BUCH-BETRAG                PIC  9(07)V99.
               15  BA-BUCH-DATUM                 PIC  X(10).
      *        15  BA-BUCH-SCHLUESSEL            PIC  9(02).
               15  BA-BUCH-SCHLUESSEL            PIC S9(02).
               15  BA-BUCH-STEUERBETRAG          PIC  9(07)V99.
               15  BA-BUCH-ANTEILE               PIC  9(06)V999.
 
               15  BA-FREI-VERBRAUCHT            PIC  9(05)V99.
      *        VERBRAUCHTER FREISTELLUNGSBETRAG
               15  BA-AEND-FREI-VERBRAUCHT-KZ    PIC  X(01).
      *        J WENN VERBRAUCHTER FREISTELLUNGAUFTRAG GEAENDERT
 
               15  BA-ZINS-AB-ST                 PIC  9(06)V99.
      *        ZINSABSCHLAGSTEUER
               15  BA-AEND-ZINS-AB-ST-KZ         PIC  X(01).
      *        J WENN ZINABSCHLAGSTEUER GEAENDERT
               15  BA-VL-KZ                      PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)
 
               15  BA-ZW-GEWINN                  PIC S9(005)V99.
...   *        Zwischengewinn Guthaben
...            15  BA-AEND-ZW-GEWINN-KZ          PIC  X(01).
...   *        J WENN Zwischengewinn GEAENDERT
...            15  BA-UMSATZ-NETTO               PIC  9(007)V99.
...   *        Nettobetrag = Rücknahmepreis x Anteile
...
...            15  BA-AEND-ANTEIL-KZ             PIC  X(01).
      *        J wenn eins der Felder in GuthabenAnteil geaendert
 
 
      *        15  FILLER                        PIC  X(28).
      *        15  FILLER                        PIC  X(010).
 
               15  BA-GUTHABEN-DATUM-VON         PIC  X(10).
               15  BA-AKTUAL-KZ                  PIC  X(01).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
 
 
 
 

