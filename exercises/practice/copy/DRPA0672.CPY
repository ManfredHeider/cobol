      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0672.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0672.CPv  $
      *
      *   Rev 2.0   Mar 10 1999 12:11:50   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:59:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0672  Funktion Select-All Standard-OZP   *
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
      *    FÜR DIE FUNKTION DRPA0672                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT ALL  <<<   TABELLE PEBD TBPA0120              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0672.
          05 PI-DRPA0672-PROD-KEY.
             10 PI-DRPA0672-PEST-ID         PIC S9(015)V USAGE COMP-3.
             10 PI-DRPA0672-PEBD-POSITIONS-NR PIC S9(004)  USAGE COMP.
      *
       01 PO-DRPA0672.
          05 PO-DRPA0672-RC                  PIC  X(003).
             88 PO-DRPA0672-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0672-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA0672-PEDB-DATEN.
             10 PO-DRPA0672-PEBD-ST-NR         PIC S9(009) USAGE COMP.
             10 PO-DRPA0672-PEBD-EMP-STF       PIC  X(004).
             10 PO-DRPA0672-PEBD-DIFF-PROV-STF PIC S9(004) USAGE COMP.
             10 PO-DRPA0672-KNR-EMP-TYP        PIC S9(004) USAGE COMP.
      *
      ****** Ende Copy-Member ****************************************
