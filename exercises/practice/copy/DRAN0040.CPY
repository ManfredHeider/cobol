      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0040.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAN0040.CPv  $
      *
      *   Rev 2.2   03 May 2001 12:52:28   t13ae34
      *FAN00304-doppelte PIN bei PZM
      *
      *   Rev 2.1   07 Mar 2001 10:00:52   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:37:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0040  Funktion SELECT ÜBER GES, pin      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * FAN00304!03.05.01!2.2 ! RC KEY-NOT-UNIQUE         ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0040                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PZMA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0040.
           05 PI-DRAN0040-PZMA-KEY.
               10 PI-DRAN0040-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0040-PZM-PIN         PIC S9(9) COMP.
      *
       01  PO-DRAN0040.
           05 PO-DRAN0040-RC                   PIC  X(003).
             88 PO-DRAN0040-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0040-RC-NOT-FOUND            VALUE 'NFD'.
030501*      88 PO-DRAN0040-RC-DUPLICATE-VALUE      VALUE 'DVA'.
030501       88 PO-DRAN0040-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0040-PZMA-DATEN.
               10 PO-DRAN0040-PZM-LAST-UPDATE PIC X(26).
               10 PO-DRAN0040-PZM-KENNUNG     PIC X(8).

      *    PZM-DATEN AUS PC-DATEN
           COPY DRAN0043.

      ****** Ende Copy-Member ****************************************
