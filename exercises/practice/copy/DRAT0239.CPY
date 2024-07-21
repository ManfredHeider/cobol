      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0239.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0239.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:00   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:48:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0239  FUNKTION                           *
      *                           DELETE FUER STANDARD-OZP AUF       *
      *                           VIAT0023                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Brennecke                                       *
      *    DATUM:    17.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!17.02.98! 01 ! Neuerstellung             ! Brennecke*
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
      *    >>>  DELETE  <<< VIAT0023                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0239.
          05 PI-DRAT0239-KEY.
             10 PI-DRAT0239-JOBNAME          PIC X(008).
             10 PI-DRAT0239-PROGRAMMNAME     PIC X(008).
          05 PI-DRAT0239-DATEN.
             10 PI-DRAT0239-LAST-UPDATE      PIC X(026).
      *
       01 PO-DRAT0239.
          05 PO-DRAT0239-RC                  PIC X(003).
             88 PO-DRAT0239-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0239-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAT0239-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende CopyMember *****************************************
