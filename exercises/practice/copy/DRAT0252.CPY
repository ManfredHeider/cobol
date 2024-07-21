      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0252.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0252.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:20   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   Apr 03 1997 16:23:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0252  FUNKTION                           *
      *                           AKT. RESTARTDATEN SCHREIBEN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    26.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!26.02.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FœR DIE FUNKTION DRAT0252                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  RESTART / AKT. RESTARTDATEN SCHREIBEN  <<<           *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0252.
          05 PI-DRAT0252-JOBNAME              PIC  X(008).
          05 PI-DRAT0252-PROGRAMMNAME         PIC  X(008).
          05 PI-DRAT0252-LAUFENDE-NR          PIC  S9(004) USAGE COMP.
          05 PI-DRAT0252-RESTARTINFO          PIC  X(4000).
      *
       01 PO-DRAT0252.
          05 PO-DRAT0252-RC                   PIC  X(003).
             88 PO-DRAT0252-RC-OK                         VALUE 'OK '.
      *
      ****** ENDE COPY-MEMBER ****************************************
