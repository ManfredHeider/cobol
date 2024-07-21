      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0591.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0591.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:30:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   22 Oct 1997 13:13:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0591  Funktion SELECT Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: E.SCHRIEBER                                     *
      *    DATUM:    03.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.11.97! 01 ! NEUERSTELLUNG             ! SCHRIEBER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0591                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0124 MPIN         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0591.
          05 PI-DRPA0591-MPIN-KEY.
             10 PI-DRPA0591-PGEB-GES          PIC S9(004) BINARY.
      *
       01 PO-DRPA0591.
          05 PO-DRPA0591-RC                   PIC  X(003).
             88 PO-DRPA0591-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0591-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA0591-MPIN-MAX-PIN         PIC S9(009) BINARY.
      *
      ****** Ende Copy-Member ****************************************
