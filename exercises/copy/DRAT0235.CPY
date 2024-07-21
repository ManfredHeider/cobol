      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0235.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0235.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:23:56   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:48:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0235  FUNKTION                           *
      *                           IN-/OUT-PARAMETER DES LESE-OZPs    *
      *                           AUF VIAT0022                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brennecke                                       *
      *    DATUM:    12.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!12.02.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTH?LT DIE EIN- UND AUSGABEPARAMETER         *
      *    FœR DIE FUNKTION DRAT0234                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0235.
          05 pi-drat0235-key.
             10 PI-DRAT0235-JOBNAME             PIC  X(008).
             10 PI-DRAT0235-PROGRAMMNAME        PIC  X(008).
      *
       01 PO-DRAT0235.
          05 PO-DRAT0235-RC                     PIC  X(003).
             88 PO-DRAT0235-RC-OK                      VALUE 'OK '.
             88 PO-DRAT0235-RC-NOT-FOUND               VALUE 'NFD'.
          05 PO-DRAT0235-DATEN.
             10 PO-DRAT0235-COMMITFREQUENZ      PIC S9(009) USAGE COMP.
      *
      ******** Ende Copy-Member **************************************
