      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1230.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1230.CPv  $
      *
      *   Rev 2.1   May 18 1999 12:23:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:08   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1230                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DAISY GEVO FÜR PF ERMITTELN>>            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    11.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!11.05.99! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1230                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1230.
          05 PI-DRPA1230-DATEN.
             10 PI-DRPA1230-DGV                     PIC S9(004) BINARY.
             10 PI-DRPA1230-PRODART                 PIC S9(004) BINARY.
      *
       01 PO-DRPA1230.
          05 PO-DRPA1230-RC                         PIC  X(003).
             88 PO-DRPA1230-RC-OK                         VALUE 'OK '.
          05 PO-DRPA1230-DATEN.
             10 PO-DRPA1230-DGV                     PIC S9(004) BINARY.
             10 PO-DRPA1230-PRODART                 PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
