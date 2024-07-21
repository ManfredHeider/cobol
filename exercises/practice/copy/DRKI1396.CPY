      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1396.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI1396.CPv  $
      *
      *   Rev 2.12   Sep 02 2013 10:29:54   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Sparpläne
      *
      *   Rev 2.11   Jul 02 2012 10:34:54   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.10   Jun 10 2008 07:37:36   t13ex39
      *AKI00708: OnlineKI - Deutsche Bank Depot - angezeigte Daten
      *
      *   Rev 2.9   22 Apr 2008 16:58:12   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten ZAHLUNG
      *
      *   Rev 2.8   Jan 16 2006 17:56:52   t13ex39
      *AKI00494: Änderung der Lieferung DeuBa-Sparpläne
      *
      *   Rev 2.4   Mar 16 2005 11:45:46   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.3   Jan 19 2005 14:30:16   t13ex39
      *AKI00249: SPARPLÄNE
      *
      *   Rev 2.2   Oct 20 2002 17:43:44   T13EX39
      *FKI00669 KSDS-KEY redefiniert
      *
      *   Rev 2.1   Jul 30 2002 16:48:42   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Apr 09 2002 08:33:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS DB24-Investmentsparpläne             *
      *                                                                *
      *    SATZLÄNGE   250                                             *
      *    SATZLÄNGE   400                                           *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!16.04.02! Neuerstellung                    !v.d.Goltz  *
      *----------------------------------------------------------------*
      * FKI00669!19.10.02!02  ! VL-FONDS WURDEN NICHT     ! HORNUNG    *
      *         !        !    ! RICHTIG ERKANNT. ABFRAGE  !            *
      *         !        !    ! DES KSDS-KEYS AUCH AUF    !            *
      *         !        !    ! UNTERDEPOTNUMMER          !            *
      *         !        !    !                           !            *
      *         !        !    ! KSDS-KEY MIT NEUER LÄNGE  !            *
      *         !        !    ! REDEFINIERT               !            *
      *         !        !    !                           !            *
      *---------!-----------------------------------------!------------*
      *AKI00249 !18.10.04!03  ! Begin/Ende der Ratenauset-!Hornung     *
      *         !        !    ! zung                      !            *
      *         !        !    !                           !            *
      *---------!-----------------------------------------!------------*
      *FKI00992 !10.03.05!04  ! wegen Tagesdatum der      !Hornung     *
      *         !        !    ! Löschung in hist. KSDS    !            *
      *         !        !    ! DEL-KZ mit aufgenommen    !            *
      *         !        !    ! wegen MOVE CORR           !            *
      *         !        !    !                           !            *
      *---------!--------!--------------------------------!------------*
      * AKI00512!27.07.05! zus. Attribute für Vermögens-  !Hornung     *
      *         !        ! sparplan                       !            *
      *         !        !                                !            *
      *---------!--------!--------------------------------!------------*
      * AKI00494!11.11.05! neuer Aufbaudes Primary-Keys   !Hornung     *
      *         !        !                                !            *
      *         !        !                                !            *
      *---------!--------!--------------------------------!------------*
      * AKI00708!14.04.08! zus. Attribute                 !Hornung     *
      *         !        !                                !            *
      *---------!--------!--------------------------------!------------*
      *AKI01149 !11.06.12! PIN verarbeiten                !WHO         *
      *----------------------------------------------------------------*
      *AKI01204 !19.06.13! NEUE FELDER EINGEFÜGT f. SEPA  !WMI         *
      *         !        !   UND DEN FILLER ANGEPASST     !            *
      *         !        !                                !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI1396.

      *
           05  KSDS-SA                        PIC  X(001).
      *            SATZART        = '7'
           05  KSDS-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '48'
>>    *    05  KSDS-KEY.
      *        10  KSDS-VNR.
      *            15 FILIALNUMMER            PIC  9(003).
      *            15 KUNDENNUMMER            PIC  9(007).
      *             VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
      *        10  UNTERDEPOTNUMMER           PIC  9(002).
      *        10  WAEHRUNG                   PIC  9(003).
      *             Immer 0
      *        10  PROD-ID.
      *            15  PLAN-NR                PIC  9(002).
      *            15  PROD-ID-REST           PIC  X(018).

      *    05  FILLER REDEFINES KSDS-KEY.
      *        10 KSDS-KEY-1                  PIC  9(12).
      *        10 FILLER                      PIC  X(23).
      *
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 FILIALNUMMER            PIC  9(003).
                   15 KUNDENNUMMER            PIC  9(007).
      *             VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               10 FILLER REDEFINES KSDS-VNR.
                   15 KSDS-KEY-1              PIC  9(10).
               10  PLAN-NR                    PIC  9(002).
               10  WAEHRUNG                   PIC  9(003).
      *             Immer 0

           05  KSDS-UNTERDEPOTNUMMER          PIC  9(002).
<<         05  PROD-ID.
               15  KSDS-PROD-ID-REST          PIC  X(018).


           05  KSDS-DATUM-EROEFFNUNG.
               10 JAHR-EROEFFNUNG             PIC  9(004).
               10 MONAT-EROEFFNUNG            PIC  9(002).
               10 TAG-EROEFFNUNG              PIC  9(002).
           05  KSDS-DATUM-SCHLIESSUNG.
               10 JAHR-SCHLIESSUNG            PIC  9(004).
               10 MONAT-SCHLIESSUNG           PIC  9(002).
               10 TAG-SCHLIESSUNG             PIC  9(002).
           05  KSDS-BANKVERBINDUNG            PIC  X(020).
           05  KSDS-BLZ                       PIC  9(011).
           05  KSDS-KONTONUMMER               PIC  9(018).
           05  KSDS-ABW-KONTOINHABER          PIC  X(027).
           05  KSDS-WKN                       PIC  X(006).
      *               WERTPAPIERKENN-NUMMER
           05  KSDS-ISIN                      PIC  X(012).
           05  KSDS-SPARRATE                  PIC  9(007)V99.
      *               SPARLEISTUNG FUER SPARPLAENE
           05  KSDS-DYNAM                     PIC  9(002)V99.
      *               DYNAMISIERUNG FUER SPARPLAENE
           05  KSDS-ZAHLWEISE                 PIC  X(001).
      *               ZAHLWEISE......***0 : ZAHLWEISE FEHLT
      *               ZAHLWEISE........*1 : JAEHRLICH
      *               ZAHLWEISE........*2 : HALBJAEHRLICH
      *               ZAHLWEISE........*4 : VIERTELJAEHRLICH
      *               ZAHLWEISE........*5 : 2-MONATLICH
      *               ZAHLWEISE........*6 : MONATLICH
      *               ZAHLWEISE.........8 : EINMALPRAEMIE
           05  KSDS-TAG                       PIC  9(002).
      *               Tag im Monat für die Einzahlung
           05  KSDS-DATUM-VERTRAGSENDE.
               10  JAHR-VERTRAGSENDE           PIC  9(004).
               10  MONAT-VERTRAGSENDE          PIC  9(002).
               10  TAG-VERTRAGSENDE            PIC  9(002).
      *               Ende der Einzahlungsfrist bei VL-Verträgen
           05  KSDS-VERTRAGSTYP                PIC  X(002).
      *               Vertragstyp......0 : wenn Einmalanlage
      *               Vertragstyp......1 : wenn VL
      *               Vertragstyp......2 : wenn Sparplan
      *               Vertragstyp......3 : Entnahmeplan
      *               Vertragstyp......4 : Lebensphasenmodell
           05  KSDS-BEGINN-RATENAUSSETZUNG     PIC  9(008).
      *               Beginndatum der Ratenaussetzung
           05  KSDS-ENDE-RATENAUSSETZUNG       PIC  9(008).
      *               Endedatum der Ratenaussetzung
           05  KSDS-DEL-KZ                     PIC  X(001).

           05  KSDS-KOSTEN-PROZ                PIC 9(01)V99.
>>    *               Kosten in %
           05  KSDS-URSPR-GES-SPAR-LEIST       PIC 9(06)V99.
      *               Ursprüngliche Gesamtsparleistung in EUR
           05  KSDS-URSPR-SPAR-RATE            PIC 9(04)V99.
      *               Ursprüngliche Sparrate
           05  KSDS-AUSG-AUFSCHL-URSPR         PIC 9(02)V99.
      *               Ausgabeaufschlag des ursprünglichen Fonds
           05  KSDS-AUSG-AUFSCHL-AKTUELL       PIC 9(02)V99.
      *               Ausgabeaufschlag des aktuellen Fonds
           05  KSDS-VERTR-LAUFZEIT             PIC 9(02).
      *               Vertragslaufzeit
           05  KSDS-BEG-DYNAM                  PIC 9(08).
      *               Beginn Dynamisierung
           05  KSDS-ZAEHLER-LASTSCHR-RUECK     PIC 9(02).
      *               Zähler Lastschriftrückgaben
           05  KSDS-KND-MERKMAL                PIC X(01).
      *               Kündigungsmerkmal = 'K' bzw. ' '
           05  KSDS-DAT-VERTR-AUFLOESUNG       PIC 9(08).
      *               Datum Vertragsauflösung  bzw. vorzeitige
<<    *               Vertragskündigung
           05  KSDS-BONIFIKATION               PIC  9(003)V9(04).
           05  KSDS-URSPR-VERT-NR              PIC  X(012).
           05  KSDS-PIN                        pIC  9(09).
           05  KSDS-IBAN                       PIC  X(034).
>>    *        IBAN zur Lastschrift für den Rateneinzug
      *
           05  KSDS-BIC                        PIC  X(011).
      *        BIC zur Lastschrift für den Rateneinzug
      *
           05  KSDS-MANDREF                    PIC  X(035).
      *        Mandatsreferenz
      *
      *    05  KSDS-RESERVE                    PIC  X(044).
    <<*    05  KSDS-RESERVE                    PIC  X(035).
           05  KSDS-RESERVE                    PIC  X(055).

      *            RESERVE
