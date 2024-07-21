      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRAN0061.CPv  $
      *
      *   Rev 2.1   Mar 06 2003 16:54:16   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:14:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0061  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0061                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE VIA-BADENIA TBAN0004       *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0061.
           05 PI-DRAN0061-VIA-KEY.
               10 PI-DRAN0061-PGEB-GES         PIC S9(4)  COMP.
               10 PI-DRAN0061-VIA-HVANTNUM     PIC S9(17) COMP-3.
               10 PI-DRAN0061-VIA-VDABVERM     PIC S9(9)  COMP.
           05 PI-DRAN0061-VIA-DATEN.
               10 PI-DRAN0061-VIA-PIN          PIC S9(9) COMP.
               10 PI-DRAN0061-VIA-PIN-2        PIC S9(9) COMP.
               10 PI-DRAN0061-VIA-HAND-BSV     PIC  X(1).
               10 PI-DRAN0061-VIA-HAND-TARIF   PIC  X(1).
               10 PI-DRAN0061-VIA-HAND-FRST    PIC  X(1).
               10 PI-DRAN0061-VIA-HAND-LEG     PIC  X(1).
               10 PI-DRAN0061-VIA-HAND-VL      PIC  X(1).
               10 PI-DRAN0061-VIA-HAND-VL2     PIC  X(1).
      *
       01  PO-DRAN0061.
           05 PO-DRAN0061-RC                   PIC  X(003).
             88 PO-DRAN0061-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0061-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0061-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

