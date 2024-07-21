      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1210.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1210.CPv  $
      *
      *   Rev 2.2   May 31 1999 07:52:34   T01JM2S
      *APA00012 - NEUE RC'S UPPA0100
      *
      *   Rev 2.1   May 04 1999 16:11:44   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1210
      *
      *   Rev 2.0   Mar 30 1999 08:44:02   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1210                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERMITTELN PVW/DIALOG>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    09.04.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!09.04.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!26.05.99!2.2 ! NEU PO-DRPA1210-RC-NOK    ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1210                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1210.
          05 PI-DRPA1210-DATEN.
             10 PI-DRPA1210-NPM-TEIL.
                COPY DRPA0940.
             10 PI-DRPA1210-MELD-TEIL-ALT.
                COPY DRPA0941.
             10 PI-DRPA1210-MELD-TEIL-NEU.
                COPY DRPA0941.
      *
       01 PO-DRPA1210.
          05 PO-DRPA1210-RC                         PIC  X(003).
             88 PO-DRPA1210-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1210-RC-NICHT-BUCHEN               VALUE 'NBU'.
             88 PO-DRPA1210-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA1210-DATEN.
             10 PO-DRPA1210-MAX-USED-KOFV-TEIL      PIC S9(004) BINARY.
             10 PO-DRPA1210-KOFV-TEIL               OCCURS 8.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
