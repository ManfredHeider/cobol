      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0837.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0837.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 13:15:26   T01JM2S
      *PEG-Produktionsuebergabe
      *
      *   Rev 1.0   14 Jan 1998 12:45:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0837                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF UPDATE FÜR STANDARD-OZP ZTPA0125           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    10.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.02.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0837                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0837.
          05 PI-DRPA0837-DATEN.
             10 PI-DRPA0837-KOF.
                COPY DRPA0824.
             10 PI-DRPA0837-LAST-UPDATE             PIC  X(026).
      *
       01 PO-DRPA0837.
          05 PO-DRPA0837-RC                         PIC  X(003).
             88 PO-DRPA0837-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0837-RC-CHANGED                    VALUE 'CHA'.
      *
      ******* ENDE COPY-MEMBER **************************************
