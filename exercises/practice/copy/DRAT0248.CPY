      ****************************************************************
      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0248.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0248.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:14   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:49:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0248  FUNKTION RESTART START             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    22.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!22.02.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FœR DIE FUNKTION DRAT0248                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  RESTART / PGM-ANMELDEN  <<<                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0248.
          05 PI-DRAT0248-KEYS.
             10 PI-DRAT0248-JOBNAME              PIC  X(008).
             10 PI-DRAT0248-PROGRAMMNAME         PIC  X(008).
      *
       01 PO-DRAT0248.
          05 PO-DRAT0248-RC                      PIC  X(003).
             88 PO-DRAT0248-RC-OK                     VALUE 'OK '.
      *
      ***      RESTART NACH UNKONTROLLIERTEM PROGRAMMABBRUCH
             88 PO-DRAT0248-RC-UNK-PGMABBRUCH         VALUE 'UPA'.
      *
      ***      RESTART NACH KONTROLLIERTEM PROGRAMMABBRUCH
             88 PO-DRAT0248-RC-K-PGMABBRUCH           VALUE 'KPA'.
          05 PO-DRAT0248-DATEN.
             10 PO-DRAT0248-COMMITHAEUFIGKEIT    PIC S9(009)
                                                      USAGE COMP.
             10 PO-DRAT0248-COMMITPOINT-NR       PIC S9(009)
                                                      USAGE COMP.
      *
      ****** ENDE COPY-MEMBER ****************************************
