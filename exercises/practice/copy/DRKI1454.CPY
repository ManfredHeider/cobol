      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1454.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI1454.CPv  $
      *
      *   Rev 2.9   Nov 15 2017 14:40:18   t13ae31
      *FKI01558: fehlerhafte Felder DRKI1454 entfernen
      *
      *   Rev 2.8   Mar 27 2017 09:02:02   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.7   Oct 05 2010 13:49:14   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.6   06 May 2008 10:14:08   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.5   22 Apr 2008 10:29:52   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.4   Oct 19 2004 08:12:48   T13EX39
      *FKI00883: PMKI0723 und DRKI1454 3 NAchkommastellen
      *
      *   Rev 2.2   Jan 27 2003 10:59:00   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   Sep 12 2002 08:30:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################
      *     ########    D R K I 1 4 5 4    ########
      *              #####################
      *
      *  COPY-MEMBER FUER Aktualisierung der Anteile mit Vertrags-ID
      *
      *  PMKI0595 --> PMKI0596
      *
      *    SATZLÄNGE 366
      *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *11.10.02!NEUERSTELLUNG                          ! PPO     ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00162! Neues Feld STAND Guthaben             ! WHO     ! 02  *
      *14.11.02!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *FKI00883! Neues Feld BA-ANTEILE-4-NK            ! WHO     ! 03  *
      *18.10.04!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00708! Neues Feld BA-KUM-AUT-WIEDER-ANLAGE   ! WHO     ! 04  *
      *14.04.08!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00892! zus. Attribute Depotrendite           ! WHO     ! 05  *
      *09.03.10!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *FKI01544! III entfernt                          ! HHU     ! 06  *
      *--------!---------------------------------------!---------!-----*
      *FKI01558! überflüssige Felder entfernt          !HHU      !     *
      *        !  BA-INV-KAPITAL-AKT         PIC 9(11)V99.
      *        !     iverstiertes Kapital aktuell
      *        !  BA-INV-KAPITAL-VOR-JAHR    PIC 9(11)V99.
      *        !   iverstiertes Kapital Vorjahr
      *        !  BA-KUM-UEBERTRAEGE         PIC S9(11)V99.
      *        !   Kumulierte Übertraäge
      *        !  BA-KUM-AUTOM-WIEDER-VORJ   PIC S9(11)V99.
      *        !   Kumulierte automatische Wiederanlage 31.12. des
      *        !   Vorjahres
      *        !  BA-DURCH-EINSTANDSKURS     PIC 9(05)V99.
      *        !   durchschnittlicher Einstandskurs
      *        !  BA-QUALITETSFLAG           PIC 9(01).
      *        !   Qualitätsflag zum Einstandskurs
      *        !  BA-REALISIERTE-GEW-VERL    PIC S9(11)V99.
      *        !   Realisierte Geweinne/Verlust
      *----------------------------------------------------------------*
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI1454.
           05  BA-VERT-ID                        PIC  X(13).
           05  BA-GESAMT.
               10  BA-GES                        PIC  9(002).
               10  BA-VNR                        PIC  9(015).
               10  BA-ANTEIL-LFDNR               PIC  9(002).
      *            LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
 
               10  BA-ISIN-NR                    PIC  X(012).
      *            ISIN-Nummer
 
               10  BA-GUTHABEN                   PIC  9(011)V99.
               10  BA-ANTEILE                    PIC  9(006)V999.
               10  BA-EINZ-KUM                   PIC  9(013).
      *            KUMULIERTE EINZAHLUNGEN
 
               10  BA-AUSZ-KUM                   PIC  9(013).
      *            KUMULIERTE AUSZAHLUNGEN
 
               10  BA-WERT-GES                   PIC  9(003)V99.
      *            WERTENTWICKLUNG GESAMT
 
               10  BA-WERT-JAHR                  PIC  9(003)V99.
      *            WERTENTWICKLUNG FUER DAS LFD JAHR
 
               10  BA-BUCH-BETRAG                PIC  9(007)V99.
               10  BA-BUCH-DATUM                 PIC  X(010).
               10  BA-BUCH-SCHLUESSEL            PIC S9(002).
               10  BA-BUCH-STEUERBETRAG          PIC  9(007)V99.
               10  BA-BUCH-ANTEILE               PIC  9(006)V999.
 
               10  BA-FREI-VERBRAUCHT            PIC  9(005)V99.
      *            VERBRAUCHTER FREISTELLUNGSBETRAG
               10  BA-AEND-FREI-VERBRAUCHT-KZ    PIC  X(001).
      *            J WENN VERBRAUCHTER FREISTELLUNGAUFTRAG GEAENDERT
 
               10  BA-ZINS-AB-ST                 PIC  9(006)V99.
      *            ZINSABSCHLAGSTEUER
               10  BA-AEND-ZINS-AB-ST-KZ         PIC  X(001).
      *            J WENN ZINABSCHLAGSTEUER GEAENDERT
               10  BA-VL-KZ                      PIC  X(001).
      *            KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J o. ' ')
 
               10  BA-ZW-GEWINN                  PIC S9(005)V99.
      *            Zwischengewinn Guthaben
               10  BA-AEND-ZW-GEWINN-KZ          PIC  X(001).
      *            J WENN Zwischengewinn GEAENDERT
               10  BA-UMSATZ-NETTO               PIC  9(007)V99.
      *            Nettobetrag = Rücknahmepreis x Anteile
 
               10  BA-AEND-ANTEIL-KZ             PIC  X(001).
      *            J wenn eins der Felder in GuthabenAnteil geaendert
 
               10  BA-ANTEILE-AKT                PIC  9(006)V999.
      *            Anteile der aktuellen Buchung
 
               10  BA-WP-KENN-NR                 PIC  X(006).
      *            FUER DIT: WERTPAPIERKENNNUMMER
      *            FUER RESTLICHE BANKEN: DVAG-PRODUKTSCHLUSSEL
 
               10  BA-GUTHABEN-DATUM-VON         PIC  X(10).
      *        10  FILLER                        PIC  X(100).
               10  BA-ANTEILE-4-NK               PIC  9(006)V9999.
      *            DEUTSCHE BANK Anteile mit 4 Nachkommastellen
               10  BA-KUM-AUT-WIEDER-ANLAGE      PIC  9(11)V99.
      *            Kumulierte automatische Wiederanlage
 
               10  FILLER                     PIC  X(067).
               10  BA-AKTUAL-KZ               PIC  X(001).
 
      *            I = INSERT
      *            U = UPDATE
      *            D = DELETE
 
 

