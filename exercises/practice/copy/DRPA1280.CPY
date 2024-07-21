      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1280.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1280.CPv  $
      *
      *   Rev 2.1   Jul 05 1999 10:37:52   T13EX21
      * 
      *
      *   Rev 2.0   Mar 30 1999 08:44:22   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1280  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.07.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1280                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE PEST-VSTAT  <<<     TABELLE PEST TBPA0119     *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1280.
          05 PI-DRPA1280-PROGRAMM              PIC  X(008).
          05 PI-DRPA1280-PEST-DATEN.
             10 PI-DRPA1280-PEST-ID           PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1280-LAST-UPDATE       PIC  X(026).
             10 PI-DRPA1280-PEST-VSTAT        PIC  X(004).
      *
       01 PO-DRPA1280.
          05 PO-DRPA1280-RC                   PIC  X(003).
             88 PO-DRPA1280-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1280-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1280-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
