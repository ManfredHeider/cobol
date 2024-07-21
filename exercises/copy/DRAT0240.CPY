      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0240.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0240.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:02   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:49:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0240  FUNKTION                           *
      *                           SELECT ALL FUER STANDARD-OZP AUF   *
      *                           VIAT0023                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    17.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!17.02.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FœR DIE FUNKTION DRAT0240                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT ALL <<< VIAT0023                              *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0240.
          05 PI-DRAT0240-KEY.
             10 PI-DRAT0240-JOBNAME             PIC  X(008).
             10 PI-DRAT0240-PROGRAMMNAME        PIC  X(008).
      *
       01 PO-DRAT0240.
          05 PO-DRAT0240-RC                     PIC  X(003).
             88 PO-DRAT0240-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0240-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRAT0240-DATEN.
             10 PO-DRAT0240-AKT-COMMITFREQUENZ  PIC S9(009)
                                                         USAGE COMP.
             10 PO-DRAT0240-COMMITPOINT-NR      PIC S9(009)
                                                         USAGE COMP.
             10 PO-DRAT0240-LAST-UPDATE         PIC  X(026).
             10 PO-DRAT0240-PROGRAMM-BEENDEN    PIC  X(001).
      *
      ****** ENDE COPY-MEMBER ****************************************
