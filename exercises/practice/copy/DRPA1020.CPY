      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1020.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1020.CPv  $
      *
      *   Rev 2.1   Nov 07 2006 15:58:18   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 13:42:18   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 13:59:20   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1020                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<Zulässigkeit d. Umbuchung prüfen>>       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Foltz                                            *
      *    DATUM:    17.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!17.07.98! 01 ! NEU                       ! Foltz    *
      * APA00452!07.11.06!2.01! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1020                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1020.
          05 PI-DRPA1020-DATEN.
             10 PI-DRPA1020-PGEB-GES                PIC S9(004) BINARY.
071106*      10 PI-DRPA1020-PRODSL                  PIC S9(004) BINARY.
071106       10 PI-DRPA1020-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PI-DRPA1020-STICHTAG                PIC  X(010).
             10 PI-DRPA1020-UB-DGV                  PIC S9(004) BINARY.
             10 PI-DRPA1020-UB-PA                   PIC S9(004) BINARY.
      *
       01 PO-DRPA1020.
          05 PO-DRPA1020-RC                         PIC  X(003).
             88 PO-DRPA1020-RC-UMBUCH-ZULAESS             VALUE 'UZU'.
             88 PO-DRPA1020-RC-UMBUCH-UNZUL               VALUE 'UUZ'.
          05 PO-DRPA1020-VSTAT                      PIC  X(004).
      *
      ******* ENDE COPY-MEMBER **************************************
