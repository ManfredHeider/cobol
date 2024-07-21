      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0244.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0244.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:08   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:49:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0244  FUNKTION                           *
      *                           UPDATE FUER STANDARD-OZP AUF       *
      *                           VIAT0024                           *
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
      *    FœR DIE FUNKTION DRAT0242                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE <<< VIAT0024                                  *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0244.
          05 PI-DRAT0244-KEY.
             10 PI-DRAT0244-JOBNAME         PIC  X(008).
             10 PI-DRAT0244-PROGRAMMNAME    PIC  X(008).
             10 PI-DRAT0244-LAUFENDE-NR     PIC S9(004) USAGE COMP.
          05 PI-DRAT0244-DATEN.
             10 PI-DRAT0244-RESTART-ANGABEN.
                49 PI-DRAT0244-RESTART-LEN  PIC S9(004) USAGE COMP.
                49 PI-DRAT0244-RESTART-TEXT PIC  X(4000).

      *
       01 PO-DRAT0244.
          05 PO-DRAT0244-RC                  PIC X(003).
             88 PO-DRAT0244-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0244-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRAT0244-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
