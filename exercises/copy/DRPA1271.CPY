      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1271.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1271.CPv  $
      *
      *   Rev 2.2   Jan 10 2014 13:10:00   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.1   Jun 24 1999 16:55:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:20   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1271                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<UPDATE PMPT>>                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    24.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.06.99! 01 ! NEU                       ! STORCH   *
      * APA01526!14.06.13! 2.2! GeVo Leben                ! BÖTTCHER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1271                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1271.
          05 PI-DRPA1271-DATEN.
             10 PI-DRPA1271-PMPT-ID              PIC S9(015)V COMP-3.
             10 PI-DRPA1271-PMPT-VERSION         PIC S9(004) COMP.
             10 PI-DRPA1271-PMPT-LAST-UPDATE     PIC  X(026).
             10 PI-DRPA1271-PRKO-KOND-NR-KDB     PIC S9(004) COMP.
             10 PI-DRPA1271-PMPT-VSTAT           PIC  X(004).
             10 PI-DRPA1271-PMPT-GRUND-PROV      PIC S9(9)V9(2) COMP-3.
             10 PI-DRPA1271-PRKO-KOND-NR-GRP     PIC S9(004) COMP.
             10 PI-DRPA1271-PMPT-EINH            PIC S9(6)V9(2) COMP-3.
             10 PI-DRPA1271-PRKO-KOND-NR-EINH    PIC S9(004) COMP.
140613       10 PI-DRPA1271-PMPT-MA-GRD-PR-STZ   PIC S9(03)V999 COMP-3.
      *
       01 PO-DRPA1271.
          05 PO-DRPA1271-RC                         PIC  X(003).
             88 PO-DRPA1271-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1271-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1271-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
