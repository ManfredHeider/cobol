      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0010.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAN0010.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:05:30   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   07 Aug 1998 10:52:34   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   30 Jul 1997 17:26:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0010  Funktion Insert Standard-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!07.10.97! 01 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0010                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE VVNR TBAN0001              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0010.
           05 PI-DRAN0010-VVNR-KEY.
             10 PI-DRAN0010-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRAN0010-VVNR-PIN         PIC S9(009) BINARY.
             10 PI-DRAN0010-VVNR-VOR-VNR     PIC  X(015).
             10 PI-DRAN0010-KTY-PGRP         PIC S9(004) BINARY.
             10 PI-DRAN0010-KNR-PGRP         PIC S9(004) BINARY.
           05 PI-DRAN0010-VVNR-DATEN.
             10 PI-DRAN0010-VVNR-KD-BETREUER PIC S9(009) BINARY.
             10 PI-DRAN0010-VVNR-AUTOMATISCH PIC  X(001).
             10 PI-DRAN0010-VVNR-BEITRAG     PIC S9(009)V9(0002) COMP-3.
             10 PI-DRAN0010-KNR-WAEHRUNG     PIC S9(004).
             10 PI-DRAN0010-KTY-WAEHRUNG     PIC S9(004).
             10 PI-DRAN0010-VVNR-KENNUNG     PIC  X(008).
             10 PI-DRAN0010-VVNR-HINWEIS     PIC  X(004).
      *
       01  PO-DRAN0010.
           05 PO-DRAN0010-RC                   PIC  X(003).
             88 PO-DRAN0010-RC-OK                         VALUE 'OK '.
             88 PO-DRAN0010-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
