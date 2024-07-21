      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAN0046.CPv  $
      *
      *   Rev 2.1   03 May 2001 12:52:32   t13ae34
      *FAN00304-doppelte PIN bei PZM
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:38:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0046  Funktion SELECT ÜBER GES, PIN, AVERM
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN00304!03.05.01!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0046                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PZMA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0046.
           05 PI-DRAN0046-PZMA-KEY.
               10 PI-DRAN0046-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0046-PZM-PIN         PIC S9(9) COMP.
               10 PI-DRAN0046-PZM-VDABVERM    PIC S9(9) COMP.
      *
       01  PO-DRAN0046.
           05 PO-DRAN0046-RC                   PIC  X(003).
             88 PO-DRAN0046-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0046-RC-NOT-FOUND            VALUE 'NFD'.
             88 PO-DRAN0046-RC-KEY-NOT-UNIQUE       VALUE 'KNU'.
      *
           05 PO-DRAN0046-PZMA-DATEN.
               10 PO-DRAN0046-PZM-LAST-UPDATE PIC X(26).
               10 PO-DRAN0046-PZM-KENNUNG     PIC X(8).

      *    PZM-DATEN AUS PC-DATEN
           COPY DRAN0043.

      ****** Ende Copy-Member ****************************************
