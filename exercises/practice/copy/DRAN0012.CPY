      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0012.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAN0012.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:05:30   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   07 Aug 1998 10:53:30   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   30 Jul 1997 17:33:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0012  FUNKTION DELETE STANDARD-OZP       *
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
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0012                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       VVNR TBAN0001                      *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRAN0012.
           05 PI-DRAN0012-VVNR-LAST-UPDATE    PIC  X(026).
           05 PI-DRAN0012-VVNR-KEY.
             10 PI-DRAN0012-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRAN0012-VVNR-PIN         PIC S9(009) BINARY.
             10 PI-DRAN0012-VVNR-VOR-VNR     PIC  X(015).
             10 PI-DRAN0012-KTY-PGRP         PIC S9(004) BINARY.
             10 PI-DRAN0012-KNR-PGRP         PIC S9(004) BINARY.
      *
       01  PO-DRAN0012.
           05 PO-DRAN0012-RC                  PIC  X(003).
             88 PO-DRAN0012-RC-OK                        VALUE 'OK '.
             88 PO-DRAN0012-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAN0012-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende-Copy-Member ****************************************
