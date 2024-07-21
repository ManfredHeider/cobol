      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0037.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAN0037.CPv  $
      *
      *   Rev 2.1   07 Mar 2001 09:56:52   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:36   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:37:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0037  Funktion DELETE Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRAN0037                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PZMA TBAN0002              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0037.
           05 PI-DRAN0037-PZMA-KEY.
               10 PI-DRAN0037-PGEB-GES        PIC S9(4) COMP.
               10 PI-DRAN0037-PZM-PIN         PIC S9(9) COMP.
      *        10 PI-DRAN0037-PZM-LAST-UPDATE PIC X(26).
      *
       01  PO-DRAN0037.
           05 PO-DRAN0037-RC                  PIC  X(003).
             88 PO-DRAN0037-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0037-RC-CHANGED             VALUE 'CHA'.
             88 PO-DRAN0037-RC-NOT-FOUND           VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
