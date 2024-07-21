      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0818.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0818.CPv  $
      *
      *   Rev 2.0   May 26 1999 17:23:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:44:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: drpa0818  Funktion lesen root                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: opritz                                          *
      *    DATUM:    28.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.01.98! 01 ! NEUERSTELLUNG             ! opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION drpa0818                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ROOT DATEI                   *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0818.
          05 PI-DRPA0818-ROOT-DATEI            PIC X(8).
          05 PI-DRPA0818-ROOT-KEY.
              10 PI-DRPA0818-P1GES              PIC XX.
              10 PI-DRPA0818-P1VSNR             PIC 9(15).
      *
       01 PO-DRPA0818.
          05 PO-DRPA0818-RC                     PIC  X(003).
             88 PO-DRPA0818-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0818-RC-DUPKEY                     VALUE 'NVB'.
             88 PO-DRPA0818-RC-NOT-FOUND                  VALUE 'NF '.
          05 PO-DRPA0818-PVER-DATEN.
             10 PO-DRPA0818-PVER-GES            PIC  X(02).
             10 PO-DRPA0818-PVER-VNR            PIC  X(15).
             10 PO-DRPA0818-PVER-AKT-PIN        PIC S9(15).
             10 PO-DRPA0818-PVER-AKT-EINH       PIC S9(06)V9(2).
             10 PO-DRPA0818-PVER-AVERM          PIC S9(07) COMP.
             10 PO-DRPA0818-PVER-VM             PIC S9(07) COMP.
          05 PO-DRPA0818-PVPT-DATEN.
             10 PO-DRPA0818-PVPT-PPFL-BEGINN    PIC X(10).
             10 PO-DRPA0818-PVPT-PPFL-ENDE      PIC X(10).


