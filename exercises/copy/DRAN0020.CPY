      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0020.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAN0020.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:05:32   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   07 Aug 1998 11:00:00   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   30 Jul 1997 17:34:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0020  FUNKTION LIST-OZP FÜR TABELLE VVNR *
      *    --------------------------------------------------------  *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    15.01.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00167!15.01.98! 01 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0020                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0020.
           05 PI-DRAN0020-PROGRAMM           PIC  X(008).
           05 PI-DRAN0020-UFKT-CODE          PIC  9(002).
           05 PI-DRAN0020-AUSWAHL-KRITERIEN.
              10 PI-DRAN0020-PGEB-GES        PIC S9(004).
              10 PI-DRAN0020-VVNR-VOR-VNR    PIC  X(015).
      *
        01 PO-DRAN0020.
           05 PO-DRAN0020-RC                 PIC  X(003).
             88 PO-DRAN0020-RC-OK                        VALUE 'OK '.
             88 PO-DRAN0020-RC-LEER                      VALUE 'LER'.
             88 PO-DRAN0020-RC-EOF                       VALUE 'EOF'.
           05 PO-DRAN0020-DATEN.
             10 PO-DRAN0020-VVNR-PIN         PIC S9(009).
             10 PO-DRAN0020-KNR-PGRP         PIC S9(004).
             10 PO-DRAN0020-KTY-PGRP         PIC S9(004).
             10 PO-DRAN0020-VVNR-KD-BETREUER PIC S9(009).
             10 PO-DRAN0020-VVNR-AUTOMATISCH PIC  X(001).
             10 PO-DRAN0020-VVNR-BEITRAG     PIC S9(009)V9(0002) COMP-3.
             10 PO-DRAN0020-KNR-WAEHRUNG     PIC S9(004).
             10 PO-DRAN0020-KTY-WAEHRUNG     PIC S9(004).
             10 PO-DRAN0020-VVNR-ERSTELLUNG  PIC  X(026).
             10 PO-DRAN0020-VVNR-LAST-UPDATE PIC  X(026).
             10 PO-DRAN0020-VVNR-KENNUNG     PIC  X(008).
             10 PO-DRAN0020-VVNR-HINWEIS     PIC  X(004).
      *
      ***** Ende-Copy-Member *****************************************
      *---------------------------------------------------------------*
