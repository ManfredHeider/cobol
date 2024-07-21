      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1279.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1279.CPv  $
      *
      *   Rev 2.2   Jul 28 1999 12:31:50   T13EX21
      *APA00046 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:22   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1279                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF: UPDATE PEST-VSTATUS                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    02.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.07.99! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1279                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1279.
          05 PI-DRPA1279-PROGRAMM              PIC  X(008).
          05 PI-DRPA1279-PEST-DATEN.
             10 PI-DRPA1279-PEST-ID            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1279-LAST-UPDATE        PIC X(26).
             10 PI-DRPA1279-PEST-VSTAT         PIC X(04).
      *
       01 PO-DRPA1279.
          05 PO-DRPA1279-RC                         PIC  X(003).
             88 PO-DRPA1279-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1279-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1279-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
