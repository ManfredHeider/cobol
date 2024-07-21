      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0736.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0736.CPv  $
      *
      *   Rev 2.1   Jan 19 2000 08:48:30   T13EX21
      *APA00063 - DTA-Sachbearbeiter
      *
      *   Rev 2.0   Feb 27 1999 14:29:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0736  FUNKTION UPDATE VSTAT STANDARD-OZP *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    26.10.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00063!26.10.99! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0736                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE VSTAT <<<       TABELLE PMPG TBPA0100         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0736.
          05 PI-DRPA0736-PMPG-LAST-UPDATE     PIC  X(026).
          05 PI-DRPA0736-PMPG-ID              PIC S9(015) COMP-3.
          05 PI-DRPA0736-PMPG-VSTAT           PIC  X(004).
      *
       01 PO-DRPA0736.
          05 PO-DRPA0736-RC                   PIC  X(003).
             88 PO-DRPA0736-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0736-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0736-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
