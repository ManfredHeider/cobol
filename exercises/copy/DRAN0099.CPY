      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0099.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0099.CPv  $
      *
      *   Rev 2.1   Aug 06 2004 08:37:00   t13ae23
      *AAN00455: Elektronischer Antrag DWS - Programme -
      *
      *   Rev 2.0   Mar 18 2002 09:17:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0099  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0099                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE DWS TBAN0006               *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0099.
           05 PI-DRAN0099-DWS-KEY.
               10 PI-DRAN0099-PGEB-GES          PIC S9(4)  COMP.
               10 PI-DRAN0099-DWS-HVANTNUM     PIC X(9).
               10 PI-DRAN0099-DWS-VDABVERM     PIC S9(9)  COMP.
           05 PI-DRAN0099-DWS-DATEN.
               10 PI-DRAN0099-DWS-PIN          PIC S9(9) COMP.
               10 PI-DRAN0099-DWS-HAND         PIC  X(1).
      *
       01  PO-DRAN0099.
           05 PO-DRAN0099-RC                   PIC  X(003).
             88 PO-DRAN0099-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0099-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0099-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

