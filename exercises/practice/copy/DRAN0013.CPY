      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAN0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:05:32   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   07 Aug 1998 10:53:52   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   30 Jul 1997 17:33:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0013  FUNKTION READ STANDARD-OZP         *
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
      *    FÜR DIE FUNKTION DRAN0013                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    VVNR TBAN0001                      *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRAN0013.
           05 PI-DRAN0013-VVNR-KEY.
             10 PI-DRAN0013-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRAN0013-VVNR-PIN         PIC S9(009) BINARY.
             10 PI-DRAN0013-VVNR-VOR-VNR     PIC  X(015).
             10 PI-DRAN0013-KTY-PGRP         PIC S9(004) BINARY.
             10 PI-DRAN0013-KNR-PGRP         PIC S9(004) BINARY.
      *
       01  PO-DRAN0013.
           05 PO-DRAN0013-RC                  PIC  X(003).
             88 PO-DRAN0013-RC-OK                        VALUE 'OK '.
             88 PO-DRAN0013-RC-NOT-FOUND                 VALUE 'NFD'.
           05 PO-DRAN0013-VVNR-DATEN.
             10 PO-DRAN0013-VVNR-KD-BETREUER PIC S9(009) BINARY.
             10 PO-DRAN0013-VVNR-AUTOMATISCH PIC  X(001).
             10 PO-DRAN0013-VVNR-BEITRAG     PIC S9(009)V9(0002) COMP-3.
             10 PO-DRAN0013-KNR-WAEHRUNG     PIC S9(004).
             10 PO-DRAN0013-KTY-WAEHRUNG     PIC S9(004).
             10 PO-DRAN0013-VVNR-ERSTELLUNG  PIC  X(026).
             10 PO-DRAN0013-VVNR-LAST-UPDATE PIC  X(026).
             10 PO-DRAN0013-VVNR-KENNUNG     PIC  X(008).
             10 PO-DRAN0013-VVNR-HINWEIS     PIC  X(004).
      *
      ****** Ende Copy-Member ****************************************
