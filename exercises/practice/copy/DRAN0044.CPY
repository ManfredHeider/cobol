      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   dran0044.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\dran0044.cpv  $
      *
      *   Rev 2.1   07 Mar 2001 10:01:42   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:38   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:38:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0044  Funktion UPDATE Vertriebsdaten
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
      *    FÜR DIE FUNKTION DRAN0044                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PZMA TBAN0002              *
      *         Vertriebsdaten                                       *
      ****************************************************************
       01  PI-DRAN0044.
           05 PI-DRAN0044-PZMA-KEY.
               10 PI-DRAN0044-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0044-PZM-HVANTNUM    PIC X(9).
           05 PI-DRAN0044-PZMA-DATEN.
               10 PI-DRAN0044-PZM-PIN         PIC S9(9) COMP.
               10 PI-DRAN0044-PZM-VDABVERM    PIC S9(9) COMP.
               10 PI-DRAN0044-PZM-VDABVERMPZ  PIC X.
               10 PI-DRAN0044-PZM-VDVMNR      PIC S9(9) COMP.
               10 PI-DRAN0044-PZM-VDVMNRPZ    PIC X.
               10 PI-DRAN0044-PZM-VDDIREKT    PIC S9(4) COMP.
      *        10 PI-DRAN0044-PZM-LAST-UPDATE PIC X(26).
               10 PI-DRAN0044-PZM-KENNUNG     PIC X(8).
      *
       01  PO-DRAN0044.
           05 PO-DRAN0044-RC                   PIC  X(003).
             88 PO-DRAN0044-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0044-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0044-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
