      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1373.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA1373.CPv  $
      *
      *   Rev 2.7   Jan 10 2014 13:11:26   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.6   Jun 13 2012 07:42:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.5   Jun 05 2012 08:49:06   t13ae37
      *FPA01271: Ermittlung des PGV-KZ bei Vorbewegung korrigieren
      *
      *   Rev 2.4   May 21 2012 08:55:00   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   Mar 30 2012 12:45:42   t13ae37
      *APA01189: vertriebsbezogene Vorbewegungsermittlung bei Belas
      *tungen
      *
      *   Rev 2.2   Mar 07 2011 10:58:36   t13ae37
      *APA01171: I191-ausgelagerte Bewegungungen heranziehen
      *
      *   Rev 2.1   May 04 2000 10:22:56   T13EX21
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1373                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1373                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    02.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!02.05.00!2.1 ! NEU                       ! STORCH   *
      * APA01171!14.02.11!2.2 ! KZ FÜR AUFRUF HISTORIE    ! BUCH     *
      * APA01101!10.04.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      * FPA01271!04.06.12!2.4 ! FÜR KORREKTUR VGES NÖTIG  ! BUCH     *
      * APA01101!13.06.12!2.10! STAND 2.8 WIEDER AKTIVIERT! STORCH   *
      * APA01526!13.12.13!2.11! KALYPSO                   ! BÖTTCHER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1373
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1373.
           05  PI-DRPA1373-PRINTE              PIC S9(015)V COMP-3.
           05  PI-DRPA1373-PGEB-GES            PIC S9(004) BINARY.
           05  PI-DRPA1373-PROD-PROD-SCHL      PIC S9(004) BINARY.
140211     05  PI-DRPA1373-KZ-PROVISION        PIC  X(001).
140211         88 PI-DRPA1373-KZ-BESTAND       VALUE SPACE.
140211         88 PI-DRPA1373-KZ-HISTORIE      VALUE '1'.
230312     05  PI-DRPA1373-KNR-MANDANT         PIC S9(004) BINARY.
131213     05  PI-DRPA1373-UNR                 PIC S9(004) BINARY.
131213     05  PI-DRPA1373-SCHICHT-NR          PIC S9(004) BINARY.
      *
       01  PO-DRPA1373.
           05  PO-DRPA1373-RC                  PIC  X(003).
               88 PO-DRPA1373-RC-OK            VALUE 'OK '.
               88 PO-DRPA1373-RC-NOT-FOUND     VALUE 'NFD'.
      *
           05  PO-DRPA1373-PROVISION           PIC S9(9)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
