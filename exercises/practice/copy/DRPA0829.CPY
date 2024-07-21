      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0829.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0829.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:58:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:45:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0829                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GSMT-PROV BUCHEN    >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    18.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0829                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0829.
          05 PI-DRPA0829-DATEN.
             10 PI-DRPA0829-KNR-FKT-GRUPPE          PIC S9(004) BINARY.
             10 PI-DRPA0829-KOFV-TEIL.
                COPY DRPA0847.
      *
       01 PO-DRPA0829.
          05 PO-DRPA0829-RC                         PIC  X(003).
             88 PO-DRPA0829-RC-OK                         VALUE 'OK '.
      *
      ******* ENDE COPY-MEMBER **************************************
