      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0946.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0946.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Aug 1998 09:32:30   T13EX16
      *APA00032 - PEW R2.0
      *
      *   Rev 1.0   01 Apr 1998 14:17:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0946  FUNKTION TAGE ZWISCHEN 2 DATEN     *
      *                 ERMITTELN                                    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    04.05.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!04.05.98! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0946                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DAUER ZWISCHEN ZWEI DATEN ERMITTELN  <<<             *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0946.
          05 PI-DRPA0946-DATUM-ANF             PIC  X(010).
          05 PI-DRPA0946-DATUM-END             PIC  X(010).
      *
       01 PO-DRPA0946.
          05 PO-DRPA0946-RC                    PIC  X(003).
             88 PO-DRPA0946-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0946-DAUER                 PIC S9(009) USAGE COMP.
      *
      ****** Ende Copy-Member ****************************************
