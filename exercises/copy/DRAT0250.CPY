      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0250.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0250.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:18   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:49:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0250  FUNKTION                           *
      *                           Restart / Verarbeitungsblock       *
      *                                     abmelden                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    02.03.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!02.03.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FœR DIE FUNKTION DRAT0250                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  RESTART / VERARBEITUNGSBLOCK ABMELDEN   <<<          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0250.
          05 PI-DRAT0250-JOBNAME               PIC  X(008).
          05 PI-DRAT0250-PROGRAMMNAME          PIC  X(008).
      *
       01 PO-DRAT0250.
          05 PO-DRAT0250-RC                    PIC  X(003).
             88 PO-DRAT0250-RC-OK                         VALUE 'OK '.
          05 PO-DRAT0250-DATEN.
             10 PO-DRAT0250-COMMITPOINT-NR     PIC S9(009) USAGE COMP.
             10 PO-DRAT0250-PROGRAMM-BEENDEN   PIC  X(001).
             10 PO-DRAT0250-COMMITHAEUFIGKEIT  PIC S9(009) USAGE COMP.
      *
      ****** ENDE COPY-MEMBER ****************************************
