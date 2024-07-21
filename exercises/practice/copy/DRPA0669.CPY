      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0669.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0669.CPv  $
      *
      *   Rev 2.0   Mar 10 1999 12:11:38   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0669  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    05.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !05.12.97! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0669                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PEBD TBPA0120              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0669.
          05 PI-DRPA0669-PROGRAMM              PIC  X(008).
          05 PI-DRPA0669-UFKT                  PIC  9(002).
          05 PI-DRPA0669-PEBD-DATEN.
             10 PI-DRPA0669-PEST-ID        PIC S9(015)V USAGE COMP-3.
             10 PI-DRPA0669-PEBD-POSITIONS-NR  PIC S9(004) USAGE COMP.
             10 PI-DRPA0669-PEBD-ST-NR         PIC S9(009) USAGE COMP.
             10 PI-DRPA0669-PEBD-EMP-STF       PIC  X(004).
             10 PI-DRPA0669-PEBD-DIFF-PROV-STF PIC S9(004) USAGE COMP.
             10 PI-DRPA0669-KNR-EMP-TYP        PIC S9(004) USAGE COMP.
      *
       01 PO-DRPA0669.
          05 PO-DRPA0669-RC                   PIC  X(003).
             88 PO-DRPA0669-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0669-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
