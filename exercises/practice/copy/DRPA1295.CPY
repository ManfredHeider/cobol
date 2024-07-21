      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1295.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1295.cpv  $
      *
      *   Rev 2.1   Aug 23 1999 11:45:36   T13EX21
      *APA00049 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:26   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1295  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    16.08.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00049!16.08.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1295                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1295.
          05 PI-DRPA1295-UFKT               PIC  9(002).
          05 PI-DRPA1295-DATEN              PIC  X(553).
      *
       01 PO-DRPA1295.
          05 PO-DRPA1295-RC                 PIC  X(003).
             88 PO-DRPA1295-RC-OK           VALUE 'OK '.
      *
      ***** ENDE-COPY-MEMBER *****************************************
