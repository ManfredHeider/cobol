      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0478.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0478.CPv  $
      *
      *   Rev 2.6   Jan 10 2014 12:38:26   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.5   Nov 02 2006 14:50:28   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   Mar 19 2003 09:22:00   t13ex21
      *APA00223 - Vergrößerung Prov-Multi
      *
      *   Rev 2.3   Nov 21 2002 17:49:00   t13ex22
      *APA00219: PROV-MULTI hinzugefügt
      *
      *   Rev 2.2   31 Aug 2001 09:48:00   t13ex21
      *APA00133 - Erweiterung um Laufzeit
      *
      *   Rev 2.1   Jun 22 1999 11:44:20   T13EX22
      *I162 als Meldungsfehler implementiert
      *
      *   Rev 2.0   May 26 1999 15:51:50   T13EX25
      *APA00042, Erstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:41:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0478 FUNKTION GELD AUF STRUKTUR VERTEILEN*
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    21.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !21.07.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      * APA00012!22.06.99! 02 ! PMBE.ID NEU               ! MUND     *
      * APA00133!22.08.01!2.02! PI-BEREICH UM LAUFZEIT ERW! STORCH   *
      * APA00212!13.11.02!2.03! PROV-MULTI ERGÄNZT        ! MUND     *
      * APA00223!12.03.03!2.04! PROV-MULTI VERGRÖSSERT    ! STORCH   *
      * APA00452!01.11.06!2.05! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL *
      * APA01539!04.10.13!2.06! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA01526!24.10.13!2.36! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0478                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0478.
          05 PI-DRPA0478-DATEN.
             10 PI-DRPA0478-GSMT-PROV         PIC S9(009)V99
                                                      PACKED-DECIMAL.
             10 PI-DRPA0478-BEITRAG           PIC S9(009)V99
                                                      PACKED-DECIMAL.
             10 PI-DRPA0478-SUMME             PIC S9(009)V99
                                                      PACKED-DECIMAL.
             10 PI-DRPA0478-PGEB-GES          PIC S9(004) BINARY.
             10 PI-DRPA0478-PROD-PROD-SCHL    PIC S9(004) BINARY.
             10 PI-DRPA0478-PRODART           PIC S9(004) BINARY.
             10 PI-DRPA0478-STICHTAG          PIC  X(010).
             10 PI-DRPA0478-PMBE-ID           PIC S9(015) BINARY.
             10 PI-DRPA0478-LAUFZEIT          PIC S9(004) BINARY.
             10 PI-DRPA0478-PROV-MULTI        PIC  9(003)V999999.
041013       10 PI-DRPA0478-TARIFSCHL-GRP     PIC S9(009) BINARY.
041013       10 PI-DRPA0478-TARIFSCHL         PIC S9(009) BINARY.
041013       10 PI-DRPA0478-KNR-TARIF-KZ      PIC S9(004) BINARY.
241013       10 PI-DRPA0478-VERM-GES          PIC S9(004) BINARY.
241013       10 PI-DRPA0478-KNR-MANDANT       PIC S9(004) BINARY.
241013       10 PI-DRPA0478-SAM-NR            PIC S9(009) BINARY.
241013       10 PI-DRPA0478-KNR-RAB-ST        PIC S9(004) BINARY.
241013       10 PI-DRPA0478-ZAHLW             PIC S9(004) BINARY.
241013       10 PI-DRPA0478-URSPR-POL-DAT     PIC X(010).
241013       10 PI-DRPA0478-FAELLIG-VON       PIC X(010).
      *
       01 PO-DRPA0478.
          05 PO-DRPA0478-RC                   PIC  X(003).
             88 PO-DRPA0478-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0478-RC-VSTAT-GEFUELLT     VALUE 'VSG'.
          05 PO-DRPA0478-DATEN.
             10 PO-DRPA0478-KONDITION-GRP     PIC S9(004) BINARY.
             10 PO-DRPA0478-GRUND-PROV        PIC S9(009)V99
                                                      PACKED-DECIMAL.
241013       10 PO-DRPA0478-BEGRSATZ-AP       PIC S9(009)V9(2) COMP-3.
241013       10 PO-DRPA0478-BEGRSATZ-PRPFLSU  PIC S9(009)V9(2) COMP-3.
241013       10 PO-DRPA0478-EH-SATZ-ALLF      PIC S9(009)V9(2) COMP-3.
241013       10 PO-DRPA0478-KUERZ-SATZ        PIC S9(009)V9(6) COMP-3.
             10 PO-DRPA0478-KUERZ-ART          PIC X(01).
                88 PO-DRPA0478-KUERZ-ART-ALT   VALUE 'A'.
                88 PO-DRPA0478-KUERZ-ART-NEU   VALUE 'N'.
                88 PO-DRPA0478-KUERZ-ART-KEINE VALUE 'K'.
      *
      ******** Ende Copy-Member **************************************
