      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:05:34   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Sep 1997 16:59:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1017  DELETE für QSAM-OZP ZIPA0102       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: H. Grüner                                       *
      *    DATUM:    20.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!15.07.98! 01 ! Neuerstellung             ! GRÜNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1017                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1017.
          05 PI-DRPA1017-UFKT               PIC  9(002).
          05 PI-DRPA1017-DATEN.
             10 PI-DRPA1017-PMPG-ID         PIC  S9(015) COMP-3.
             10 PI-DRPA1017-REST            PIC   X(015).
      *
       01 PO-DRPA1017.
          05 PO-DRPA1017-RC                 PIC  X(003).
             88 PO-DRPA1017-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
