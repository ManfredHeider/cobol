      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0791.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0791.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:20:04   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   12 Dec 1997 09:48:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0791                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG LöSCHEN                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    11.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.01.98! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0791                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0791.
          05 PI-DRPA0791-PVER-KEY.
             10 PI-DRPA0791-PRINTE             PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0791-PVER-VERSION       PIC S9(4) USAGE COMP.
             10 PI-DRPA0791-PVER-LAST-UPDATE   PIC X(26).
      *
       01 PO-DRPA0791.
          05 PO-DRPA0791-RC                         PIC  X(003).
             88 PO-DRPA0791-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0791-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0791-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
