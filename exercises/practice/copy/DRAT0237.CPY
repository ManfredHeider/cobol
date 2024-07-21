      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0237.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0237.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:23:58   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:48:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0237  FUNKTION                           *
      *                           INSERT FUER STANDARD-OZP AUF       *
      *                           VIAT0023                           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brennecke                                       *
      *    DATUM:    17.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!17.02.98! 01 ! Neuerstellng              ! Brennecke*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FœR DIE FUNKTION DRAT0241                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<          VIAT0023                         *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0237.
          05 PI-DRAT0237-KEY.
             10 PI-DRAT0237-JOBNAME              PIC  X(008).
             10 PI-DRAT0237-PROGRAMMNAME         PIC  X(008).
          05 PI-DRAT0237-DATEN.
             10 PI-DRAT0237-AKT-COMMITFREQUENZ   PIC S9(009) USAGE COMP.
             10 PI-DRAT0237-COMMITPOINT-NR       PIC S9(009) USAGE COMP.
             10 PI-DRAT0237-LAST-UPDATE          PIC  X(026).
             10 PI-DRAT0237-PROGRAMM-BEENDEN     PIC  X(001).


      *
       01 PO-DRAT0237.
          05 PO-DRAT0237-RC                      PIC  X(003).
             88 PO-DRAT0237-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0237-RC-DUPLICATE-VALUE           VALUE 'DVA'.

      *
      ****** Ende Copy-Member ****************************************
