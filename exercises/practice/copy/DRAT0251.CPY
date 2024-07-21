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
      *    $Workfile:   DRAT0251.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0251.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:18   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:49:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0251  FUNKTION                           *
      *                           RESTARTinformationen lesen         *
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
      *    FœR DIE FUNKTION DRAT0251                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  RESTART / RESTARTANGABEN LESEN      <<<              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0251.
          10 PI-DRAT0251-JOBNAME              PIC  X(008).
          10 PI-DRAT0251-PROGRAMMNAME         PIC  X(008).
          10 PI-DRAT0251-LAUFENDE-NR          PIC S9(004)
                                                     USAGE COMP.
      *
       01 PO-DRAT0251.
          05 PO-DRAT0251-RC                   PIC  X(003).
             88 PO-DRAT0251-RC-OK                         VALUE 'OK '.
          05 PO-DRAT0251-LAUFENDE-NR          PIC  S9(004) USAGE COMP.
          05 PO-DRAT0251-RESTARTINFO          PIC   X(4000).
      *
      ****** ENDE COPY-MEMBER ****************************************
