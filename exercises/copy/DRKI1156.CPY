      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1156.CPY  $                                 0001000
 
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRKI1156.CPv  $
      *
      *   Rev 2.11   Dec 14 2020 06:35:06   t13ex39
      *AKI01733: FoDB Verarbeitung "urspr. Sparrate des Sparplan"
      *
      *   Rev 2.10   Nov 18 2019 14:02:20   t13ex39
      *AKI01733: FoDB Verarbeitung "urspr. Sparrate des Sparplan"
      *
      *   Rev 2.9   Mar 27 2017 09:01:42   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.8   Dec 19 2007 14:44:10   t13ex50
      *AKI00611: dit-AufbauPlan Plus/VL: Datenspiegel und Maskenanp
      *assungen
      *
      *   Rev 2.6   Sep 06 2006 15:57:30   t13ex14
      *AKI00611: dit-AufbauPlan Plus/VL: Datenspiegel und Maskenanp
      *assungen
      *
      *   Rev 2.5   Dec 01 2005 17:02:30   t13ex39
      *AKI00512: VermögensSparplan
      *
      *   Rev 2.4   Jan 19 2005 14:29:52   t13ex39
      *AKI00249: SPARPLÄNE
      *
      *   Rev 2.3   Jan 27 2003 16:46:02   t13ex12
      *AKI00162 Compile wg DRKI1156
      *
      *   Rev 2.2   Jan 27 2003 09:12:24   t13ex12
      *AKI00162 Aktualisierung neue DIT- und DB-Felder
      *
      *   Rev 2.1   Mar 07 2002 17:45:00   t13ex12
      *AKI00162 Neue Felder von DIT
      *
      *   Rev 2.0   07 Dec 2000 09:36:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 1 5 6    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER GES. DIT,         *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: ALLE EIN- UND AUSZAHLUNGEN                   *
      *                                                                *
      *    SATZLÄNGE   100                                             *
251102*    Neue SATZLÄNGE 250                                          *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *04.12.00!NEUERSTELLUNG                          !V.D.GOLTZ! 01  *
      *AKI0162 !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *26.11.02!Neue Felder von DeuBa. KZ für VL.      !V.D.GOLTZ! 02  *
      *AKI0162 !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *27.01.03! Länge korrigiert.                     !V.D.GOLTZ! 03  *
      *AKI0162 !                                       !         !     *
      *        !                                       !         !     *
      *--------!-------------------------------------------!-----------*
      *AKI00249!18.10.04! Neue Felder                      !Hornung    *
      *        !        ! Begin/Ende der Ratenaussetzung   !           *
      *--------!-------------------------------------------!-----------*
280706*AKI00611!28.07.06! NEUE FELDER                      !MISGELD    *
      *        !        ! DEL-KZ, VERTRAGSTYP,DYNAMISIERUNG!           *
      *--------!-------------------------------------------!-----------*
031207*AKI00611!03.12.07! DEL-KZ WIEDER RAUS + LFDNR VON   !SCHLEICHER *
      *        !        !   2 AUF 4 STELLEN ERWEITERT      ! V2.07     *
      *--------!-------------------------------------------!-----------*
      *FKI01544!22.03.17! III entfernt                     !HHU        *
      *--------------------------------------------------------------*
      * AKI01733!05.11.19! uprsprüngliche Sparrate jetzt  ! WHO      *
      *         !        ! 9(07)V99                       !          *
      *         !        !                                !          *
      ******************************************************************
       01  DRKI1156.
           05  GZ-GES                PIC  9(002).
           05  GZ-VNR                PIC  9(015).
      *    05  GZ-ANTEIL-LFDNR       PIC  9(002).
 
           05  GZ-ZAHLUNGS-KZ        PIC  X(001).
      *        'E' = Einzahlung, 'A' = Auszahlung
 
251102     05  GZ-DEPOT-UNR          PIC  9(002).
251102*        Depotunternr. wird Teil des Keys in Guthabenprodukt
 
           05  GZ-ZAHLUNG-LFDNR      PIC  9(004).
      *        LFDNR von DIT /Sparplannr. von DB
 
251102     05  GZ-ISIN               PIC  X(012).
251102*        Internationale WKN
 
           05  GZ-WP-KENN-NR         PIC  X(006).
      *        WERTPAPIERKENNNUMMER
 
           05  GZ-BETRAG             PIC  9(007)V99.
           05  GZ-BETRAG-AEND        PIC  X(001).
      *        Sparleistung oder Auszahlung
           05  GZ-DYNAM              PIC  9(002)V99.
           05  GZ-DYNAM-AEND         PIC  X(001).
 
           05  GZ-ZAHLWEISE          PIC S9(002).
           05  GZ-ZAHLWEISE-AEND     PIC  X(001).
 
           05  GZ-TAG                PIC  9(002).
           05  GZ-TAG-AEND           PIC  X(001).
      *        Tag der Ein- oder Auszahlung
 
           05  GZ-BUCHSCHL           PIC S9(002).
           05  GZ-BUCHSCHL-AEND      PIC  X(001).
 
251102     05  GZ-DAT-EROEFFNUNG     PIC  X(010).
251102*        Datum Sparplan-/Auszahlplaneröffnung
 
251102     05  GZ-DAT-SCHLIESSUNG    PIC  X(010).
251102*        Datum Sparplan-/ Auszahlplanschliessung
 
251102     05  GZ-DAT-ENDE-EINZ      PIC  X(010).
251102*        Datum Ende der Einzahlungsfrist bei VL-Verträgen
 
251102     05  GZ-KONTOINHABER       PIC  X(027).
251102*        Kontoinhaber von DB
 
251102     05  GZ-BANKNAME           PIC  X(020).
251102*        Bankverbindung von DB, Namen des Instituts
 
251102     05  GZ-BLZ                PIC  9(011).
251102*        Bankverbindung von DB, BLZ des Instituts
 
251102     05  GZ-KONTONR            PIC  9(018).
251102*        Bankverbindung von DB, Kontonr.
 
           05  GZ-AKTUAL-KZ          PIC  X(001).
      *        I = INSERT
      *        D = DELETE
      *        U = UPDATE
 
           05  GZ-EIN-LOESCHEN-KZ    PIC  X(001).
      *       'J'  = alle Einzahlungen in DB löschen
           05  GZ-AUS-LOESCHEN-KZ    PIC  X(001).
      *       'J'  = alle Auszahlungen in DB löschen
 
251102     05  GZ-VL-KZ              PIC X(001).
251102*       'J'  = VL-Vertrag
251102*       'N'  = nicht-VL-Vertrag
 
181004     05  GZ-BEGINN-RATENAUSSETZUNG PIC  X(010).
181004*            Beginndatum der Ratenaussetzung
181004     05  GZ-ENDE-RATENAUSSETZUNG   PIC  X(010).
181004*            Endedatum der Ratenaussetzung
270705     05  GZ-KOSTEN-PROZ                       PIC 9(01)V99.
>>    *               Kosten in %
           05  GZ-URSPR-GES-SPAR-LEIST              PIC 9(06)V99.
      *               Ursprüngliche Gesamtsparleistung in EUR
           05  GZ-URSPR-SPAR-RATE                   PIC 9(07)V99.
      *               Ursprüngliche Sparrate
           05  GZ-AUSG-AUFSCHL-URSPR                PIC 9(02)V99.
      *               Ausgabeaufschlag des ursprünglichen Fonds
           05  GZ-AUSG-AUFSCHL-AKTUELL              PIC 9(02)V99.
      *               Ausgabeaufschlag des aktuellen Fonds
           05  GZ-VERTR-LAUFZEIT                    PIC 9(02).
      *               Vertragslaufzeit
           05  GZ-BEG-DYNAM                         PIC X(10).
      *               Beginn Dynamisierung
           05  GZ-ZAEHLER-LASTSCHR-RUECK            PIC 9(02).
      *               Zähler Lastschriftrückgaben
           05  GZ-KND-MERKMAL                       PIC X(01).
      *               Kündigungsmerkmal = 'K' bzw. ' '
           05  GZ-DAT-VERTR-AUFLOESUNG              PIC X(10).
      *               Datum Vertragsauflösung  bzw. vorzeitige
<<    *               Vertragskündigung
270705*
280706*    05  GZ-DEL-KZ                            PIC X(001).
280706*       'J'  = Sparplan DIT in DB löschen
280706*
280706     05  GZ-VERTRAGSTYP                       PIC X(002).
 
181004*    05  FILLER                    PIC  X(076).
270705*    05  FILLER                    PIC  X(056).
280706*    05  FILLER                    PIC  X(006).
031207*    05  FILLER                    PIC  X(003).
031207     05  FILLER                    PIC  X(050).



