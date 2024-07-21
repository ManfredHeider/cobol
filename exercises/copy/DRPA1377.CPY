      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1377.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1377.CPv  $
      *
      *   Rev 2.1   16 May 2000 09:21:08   T13EX23
      *APA00090 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:57:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1377                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1377                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                 *
      *    DATUM:    11.05.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00090!11.05.00! 2.1 ! NEUERSTELLUNG            ! GRAY     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1377                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1377.
          05 PI-DRPA1377-PROGRAMM              PIC  X(008).
          05 PI-DRPA1377-UFKT                  PIC  9(002).
          05 PI-DRPA1377-DATEN.
             10 PI-DRPA1377-PGEB-GES           PIC S9(4) USAGE COMP.
             10 PI-DRPA1377-PINZ-PIN           PIC S9(9) USAGE COMP.
             10 PI-DRPA1377-PRINTE             PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1377-PINZ-PG-VBNR       PIC S9(9) USAGE COMP.
             10 PI-DRPA1377-PINZ-GUELTIG-AB    PIC  X(10).

      *
       01 PO-DRPA1377.
          05 PO-DRPA1377-RC                         PIC  X(003).
             88 PO-DRPA1377-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1377-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
