      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0036.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAN0036.CPv  $
      *
      *   Rev 2.2   07 May 2001 09:32:46   t13ae34
      *FAN00306-AN50:  AG8-Änderung nicht mög-
      *lich + Update PZM über Vorschlagsnummer
      *+ VB oder VM
      *
      *   Rev 2.1   07 Mar 2001 09:58:28   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:36   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:36:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0036  Funktion UPDATE Standard-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * FAN00306!07.05.01!2.2 !UPDATE TBAN0002 MIT AVERM  !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0036                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PZMA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0036.
           05 PI-DRAN0036-PZMA-KEY.
               10 PI-DRAN0036-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0036-PZM-HVANTNUM    PIC X(9).
               10 PI-DRAN0036-PZM-VDABVERM    PIC  S9(9) COMP.
           05 PI-DRAN0036-PZMA-DATEN.
               10 PI-DRAN0036-PZM-PIN         PIC S9(9) COMP.
      *        10 PI-DRAN0036-PZM-LAST-UPDATE PIC X(26).
               10 PI-DRAN0036-PZM-KENNUNG     PIC X(8).
      *
       01  PO-DRAN0036.
           05 PO-DRAN0036-RC                   PIC  X(003).
             88 PO-DRAN0036-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0036-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0036-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
