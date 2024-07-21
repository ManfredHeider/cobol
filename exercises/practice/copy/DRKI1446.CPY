      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1446.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRKI1446.CPv  $
      *
      *   Rev 2.11   Nov 15 2017 13:37:32   t13ae31
      *FKI01558: fehlerhafte Felder DRKI1446 entfernen
      *
      *   Rev 2.10   Mar 27 2017 09:01:56   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.9   Oct 05 2010 13:49:02   t13ex39
      *AKI00892: Bruttoperformance; Mapping
      *
      *   Rev 2.8   06 May 2008 10:13:56   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.7   21 Apr 2008 10:31:24   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.6   21 Apr 2008 10:27:22   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten DEPOT
      *
      *   Rev 2.5   Jan 26 2005 16:41:22   t13ex12
      *AKI00455: Fondsverschmelzung des iii-Fonds Nr. 1 und Nr. 2
      *
      *   Rev 2.4   Oct 19 2004 08:17:14   T13EX39
      *FKI00883: PMKI0723 und DRKI1454 3 NAchkommastellen
      *
      *   Rev 2.2   Jan 27 2003 10:58:44   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.0   Sep 12 2002 08:29:18   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 4 4 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE   353 BYTE - 23. 09. 2002                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *23.09.02!NEU aus DRKI0173                       !P. Pohl  ! 01  *
      *        !AKI00294                               !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00162! Neues Feld STAND Guthaben             ! WHO     ! 02  *
      *14.11.02!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *FKI00883! Neues Feld BA-ANTEILE-4-NK            ! WHO     ! 03  *
      *18.10.04!                                       !         !     *
      *        !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00455! BA-ANTEILE-AKT entfernen weil Feld    !v.d.Goltz!     *
      *        ! doppelt vorh. und nur für III benutzt.!         !     *
      *        !                                       !         !     *
      *----------------------------------------------------------------*
      *AKI00708! zus. Attribute                        !WHO      !     *
      *        !                                       !         !     *
      *----------------------------------------------------------------*
      *AKI00892! zus. Attribute                        !WHO      !     *
      *----------------------------------------------------------------*
      *FKI01544! III entfernt                          !HHU      !     *
      *        !                                       !         !     *
      *----------------------------------------------------------------*
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
       01  DRKI1446.
           05  BA-GES                        PIC  9(002).
           05  BA-VNR                        PIC  9(015).
           05  BA-ANTEIL-LFDNR               PIC  9(002).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
 
           05  BA-ISIN-NR                    PIC  X(012).
      *        ISIN-Nummer
 
           05  BA-GUTHABEN                   PIC  9(011)V99.
           05  BA-ANTEILE                    PIC  9(006)V999.
           05  BA-EINZ-KUM                   PIC  9(013).
      *        KUMULIERTE EINZAHLUNGEN
 
           05  BA-AUSZ-KUM                   PIC  9(013).
      *        KUMULIERTE AUSZAHLUNGEN
 
           05  BA-WERT-GES                   PIC  9(003)V99.
      *        WERTENTWICKLUNG GESAMT
 
           05  BA-WERT-JAHR                  PIC  9(003)V99.
      *        WERTENTWICKLUNG FUER DAS LFD JAHR
 
           05  BA-BUCH-BETRAG                PIC  9(007)V99.
           05  BA-BUCH-DATUM                 PIC  X(010).
      *    05  BA-BUCH-SCHLUESSEL            PIC  9(02).
           05  BA-BUCH-SCHLUESSEL            PIC S9(002).
           05  BA-BUCH-STEUERBETRAG          PIC  9(007)V99.
           05  BA-BUCH-ANTEILE               PIC  9(006)V999.
      *        Anteile der aktuellen Buchung
 
           05  BA-FREI-VERBRAUCHT            PIC  9(005)V99.
      *        VERBRAUCHTER FREISTELLUNGSBETRAG
           05  BA-AEND-FREI-VERBRAUCHT-KZ    PIC  X(001).
      *        J WENN VERBRAUCHTER FREISTELLUNGAUFTRAG GEAENDERT
 
           05  BA-ZINS-AB-ST                 PIC  9(006)V99.
      *        ZINSABSCHLAGSTEUER
           05  BA-AEND-ZINS-AB-ST-KZ         PIC  X(001).
      *        J WENN ZINABSCHLAGSTEUER GEAENDERT
           05  BA-VL-KZ                      PIC  X(001).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)
 
           05  BA-ZW-GEWINN                  PIC S9(005)V99.
      *        Zwischengewinn Guthaben
           05  BA-AEND-ZW-GEWINN-KZ          PIC  X(001).
      *        J WENN Zwischengewinn GEAENDERT
           05  BA-UMSATZ-NETTO               PIC  9(007)V99.
      *        Nettobetrag = Rücknahmepreis x Anteile
 
           05  BA-AEND-ANTEIL-KZ             PIC  X(001).
      *        J wenn eins der Felder in GuthabenAnteil geaendert
 
      *    05  BA-ANTEILE-AKT                PIC  9(006)V999.
      *        Anteile der aktuellen Buchung
           05  FILLER                        PIC  X(009).
 
           05  BA-WP-KENN-NR                 PIC  X(006).
      *        FUER DIT: WERTPAPIERKENNNUMMER
      *        FUER RESTLICHE BANKEN: DVAG-PRODUKTSCHLUSSEL
 
      *    05  FILLER                        PIC  X(28).
      *    05  FILLER                        PIC  X(10).
 
           05  BA-GUTHABEN-DATUM-VON         PIC  X(010).
      *    05  FILLER                        PIC  X(100).
           05  BA-ANTEILE-4-NK               PIC  9(006)V9999.
      *        DEUTSCHE BANK Anteile mit 4 Nachkommastellen
           05  BA-KUM-AUT-WIEDER-ANLAGE     PIC  9(11)V99.
      *        Kumulierte automatische Wiederanlage
           05  FILLER                        PIC  X(067).
 
           05  BA-AKTUAL-KZ                  PIC  X(001).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
 
 

