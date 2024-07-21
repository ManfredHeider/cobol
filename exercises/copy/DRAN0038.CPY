      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   dran0038.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\dran0038.cpv  $
      *
      *   Rev 2.1   07 Mar 2001 09:59:16   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:37:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0038  Funktion SELECT Standard-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!23.11.00!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0038                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PZMA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0038.
           05 PI-DRAN0038-PZMA-KEY.
               10 PI-DRAN0038-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0038-PZM-HVANTNUM    PIC X(9).
      *
       01  PO-DRAN0038.
           05 PO-DRAN0038-RC                   PIC  X(003).
             88 PO-DRAN0038-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0038-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0038-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0038-PZMA-DATEN.
               10 PO-DRAN0038-PZM-PIN         PIC S9(9) COMP.
               10 PO-DRAN0038-PZM-LAST-UPDATE PIC X(26).
               10 PO-DRAN0038-PZM-KENNUNG     PIC X(8).
      *    PZM-DATEN AUS PC-DATEN
           COPY DRAN0043.
      ****** Ende Copy-Member ****************************************
