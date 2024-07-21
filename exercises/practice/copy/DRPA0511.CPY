      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0511.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA0511.CPv  $
      *
      *   Rev 2.6   Jun 05 2018 14:41:22   t13ex44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.5   Jan 10 2014 12:45:00   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.4   Nov 02 2006 14:51:12   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Mar 20 2006 08:37:30   t13ex44
      *APA00414: Erweiterung der Felder für Haftungszeitangaben bei
      * Central
      *
      *   Rev 2.2   Feb 03 2003 09:25:04   t13ex25
      *FPA00521; Printe und PGV-KZ neu
      *
      *   Rev 2.1   09 May 2001 09:07:46   T13EX25
      *APA00132; Neu: Haftungs- und Belastungsdauer
      *
      *
      *   Rev 2.0   May 27 1999 08:19:38   T13EX25
      *APA00042, Erfassung V2.0
      *
      *   Rev 1.0   12 Sep 1997 16:59:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0511  FUNKTION Einheiten verteilen       *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    16.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * DAISY 1 !16.09.97!01.01! NEUERSTELLUNG             ! JÄGER   *
      *         !        !     !                           !         *
      * APA00012!27.01.98!01.02! EINH-NEU IN EINH-PMPT UMB.! JÄGER   *
      * APA00012!25.09.98!01.03! EINH-AKT ENTFERNT         ! MUND    *
      * APA00132!07.05.01!01.04! NEUE ATTRIBUTE EINFÜGEN   ! JÄGER   *
      * FPA00521!31.01.03!01.05! NEUE ATRRIBUTE EINFÜGEN   ! JÄGER   *
      * APA00414!12.01.06! 2.3 ! HAFTZEITDAUER UND BELAS-  ! SCHEIDEL*
      *         !        !     ! TUNGSDAUER VIERSTELLIG    !         *
      * APA00452!01.11.06! 2.4 ! DREIST. PRODUKTSCHLUESSEL ! SCHEIDEL*
      * APA01526!24.10.13! 2.5 ! BEGR.SÄTZE KOLLEKTIV      ! BÖTTCHER*
      * APA02579!24.04.18! 2.6 ! PGV IN PI-BEREICH HINZU   ! SCHEIDEL*
      * DSY04126!15.12.21!     ! NEUST-KZ HINZU            ! WANG    *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0511                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0511.
          05 PI-DRPA0511-DATEN.
             10 PI-DRPA0511-GSMT-PROV  PIC S9(009)V9(02) PACKED-DECIMAL.
             10 PI-DRPA0511-GRUND-PROV PIC S9(009)V9(02) PACKED-DECIMAL.
             10 PI-DRPA0511-EINH-AKT   PIC S9(006)V9(02) PACKED-DECIMAL.
             10 PI-DRPA0511-STICHTAG   PIC  X(010).
120106       10 PI-DRPA0511-HAFTZEITDAUER
                                       PIC  X(004).
120106       10 PI-DRPA0511-BELASTUNGSDAUER
                                       PIC  X(004).
             10 PI-DRPA0511-PGEB-GES   PIC S9(004)       BINARY.
011106       10 PI-DRPA0511-PROD-PROD-SCHL
011106                                 PIC S9(004)       BINARY.
             10 PI-DRPA0511-PRODART    PIC S9(004)       BINARY.
310103       10 PI-DRPA0511-PRINTE     PIC S9(015)       PACKED-DECIMAL.
310103       10 PI-DRPA0511-PGV-KZ     PIC  X(001).
241013       10 PI-DRPA0511-TARIFSCHL-GRP
241013                                 PIC S9(009)       BINARY.
241013       10 PI-DRPA0511-TARIFSCHL  PIC S9(009)       BINARY.
241013       10 PI-DRPA0511-KNR-TARIF-KZ
241013                                 PIC S9(004)       BINARY.
241013       10 PI-DRPA0511-VERM-GES   PIC S9(004)       BINARY.
241013       10 PI-DRPA0511-SAM-NR     PIC S9(009)       BINARY.
241013       10 PI-DRPA0511-KNR-MANDANT
241013                                 PIC S9(009)       BINARY.
241013       10 PI-DRPA0511-KNR-RAB-ST PIC S9(004)       BINARY.
241013       10 PI-DRPA0511-ZAHLW      PIC S9(004)       BINARY.
241013       10 PI-DRPA0511-URSPR-POL-DAT
241013                                 PIC  X(010).
241013       10 PI-DRPA0511-FAELLIG-VON
241013                                 PIC  X(010).
240418       10 PI-DRPA0511-PGV        PIC S9(004)       BINARY.
151221       10 PI-DRPA0511-NEUST-KZ   PIC  X(001).
      *
       01 PO-DRPA0511.
          05 PO-DRPA0511-RC            PIC  X(003).
             88 PO-DRPA0511-RC-OK           VALUE 'OK '.
             88 PO-DRPA0511-RC-VSTAT-GEFUELLT
                                            VALUE 'VSG'.
          05 PO-DRPA0511-DATEN.
             10 PO-DRPA0511-KONDITION-EINH
                                       PIC S9(004)       BINARY.
             10 PO-DRPA0511-EINH-PMPT  PIC S9(006)V9(02) PACKED-DECIMAL.
             10 PO-DRPA0511-VM-GESCH-KZ
                                       PIC  X(001).
241013       10 PO-DRPA0511-BEGRSATZ-AP
241013                                 PIC S9(009)V9(02) PACKED-DECIMAL.
241013       10 PO-DRPA0511-BEGRSATZ-PRPFLSU
241013                                 PIC S9(009)V9(02) PACKED-DECIMAL.
241013       10 PO-DRPA0511-EH-SATZ-ALLF
241013                                 PIC S9(009)V9(02) PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************

