      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0745.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0745.CPv  $
      *
      *   Rev 2.1   Nov 05 1999 12:01:20   T13EX21
      *FPA00107 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0745                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  PROVISIONSDATEN EINFÜGEN                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    05.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00107!05.11.99! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0745                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0745.
        03 PI-DRPA0745-KOPF.
          05 PI-DRPA0745-PROGRAMM              PIC  X(008).
          05 PI-DRPA0745-UFKT                  PIC  9(002).
        03 PI-DRPA0745-PDPT-PDPE-DATEN.
           COPY DRPA0451.
      *
       01 PO-DRPA0745.
          05 PO-DRPA0745-RC                         PIC  X(003).
             88 PO-DRPA0745-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0745-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
