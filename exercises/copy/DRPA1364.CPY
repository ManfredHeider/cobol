      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1364.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1364.CPv  $
      *
      *   Rev 2.1   Apr 04 2000 16:12:04   T13EX21
      *APA00067 - Neuerstellung
      *
      *
      *   Rev 2.0   Mar 01 2000 09:58:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1364  FUNKTION FÄLLIGKEIT GEGEN          *
      *                              MIGRATIONSDATUM PRÜFEN          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    30.03.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!30.03.00!2.1  ! NEUERSTELLUNG             ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1364.
          05 PI-DRPA1364-ZVP-VERSION             PIC S9(004) COMP.
          05 PI-DRPA1364-PGEB-GES                PIC S9(004) COMP.
          05 PI-DRPA1364-FAELLIG-VON             PIC  X(010).
          05 PI-DRPA1364-PGV                     PIC S9(004) COMP.
      *
       01 PO-DRPA1364.
          05 PO-DRPA1364-RC                     PIC  X(003).
             88 PO-DRPA1364-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1364-RC-VSTAT-GEFUELLT            VALUE 'VSG'.
      *
      ****** ENDE COPY-MEMBER ****************************************
