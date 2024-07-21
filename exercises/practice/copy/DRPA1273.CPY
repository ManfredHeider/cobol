      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1273.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1273.CPv  $
      *
      *   Rev 2.1   Jun 24 1999 16:56:04   T13EX21
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
      *    COPY-MEMBER: DRPA1273                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<UPDATE PVER-AKT-DGV>>                    *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1273                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1273.
          05 PI-DRPA1273-DATEN.
             10 PI-DRPA1273-PRINTE               PIC S9(015)V COMP-3.
             10 PI-DRPA1273-PVER-AKT-DGV         PIC S9(004) COMP.
      *
       01 PO-DRPA1273.
          05 PO-DRPA1273-RC                         PIC  X(003).
             88 PO-DRPA1273-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1273-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1273-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
