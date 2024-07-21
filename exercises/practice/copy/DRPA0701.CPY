      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0701.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0701.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:05:00   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Dec 1997 09:41:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0701  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                       *
      *    DATUM:    10.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.12.97! 01 ! NEUERSTELLUNG             ! SCHRIEBER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0701                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0103 PMPP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0701.
          05 PI-DRPA0701-EINGABE.
             10 PI-DRPA0701-PMPG-ID           PIC S9(015) COMP-3.
      *
       01 PO-DRPA0701.
          05 PO-DRPA0701-ANZAHL-GEL-ZEILEN    PIC S9(009)  COMP.
          05 PO-DRPA0701-RC                   PIC  X(003).
             88 PO-DRPA0701-RC-OK                         VALUE 'OK '.
      *
      ****** Ende Copy-Member ****************************************
