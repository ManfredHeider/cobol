      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1363.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1363.CPv  $
      *
      *   Rev 2.1   Apr 13 2000 15:51:38   T13EX21
      * 
      *
      *   Rev 2.0   Mar 01 2000 09:58:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1363  FUNKTION PIN PRÜFEN                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    29.03.2000                                      *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      *                                                              *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00067!29.03.00!2.1  ! NEUERSTELLUNG             ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA1363.
          05 PI-DRPA1363-ZVP-VERSION             PIC S9(004) COMP.
          05 PI-DRPA1363-PIN                     PIC S9(009) COMP.
          05 PI-DRPA1363-PGEB-GES                PIC S9(004) COMP.
      *
       01 PO-DRPA1363.
          05 PO-DRPA1363-RC                     PIC  X(003).
             88 PO-DRPA1363-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1363-RC-VSTAT-GEFUELLT            VALUE 'VSG'.
      *
      ****** ENDE COPY-MEMBER ****************************************
