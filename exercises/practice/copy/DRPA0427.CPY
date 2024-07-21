      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0427.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0427.CPv  $
      *
      *   Rev 2.1   Mar 10 2000 10:52:58   T13EX21
      *APA00066 - Redesign PVW
      *
      *   Rev 2.0   May 27 1999 13:40:40   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:35:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0427                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMBE ERSTELLEN      >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    24.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.10.97! 01 ! NEU                       ! WOLFF    *
      * APA00066!17.11.99! 02 ! PI-RFOLGE-NR ENTFERNT     ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0427                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0427.
          05 PI-DRPA0427-DATEN.
171199*      10 PI-DRPA0427-RFOLGE-NR               PIC S9(004) BINARY.
             10 PI-DRPA0427-PMPE.
                COPY DRPA0289.
             10 PI-DRPA0427-PVER.
                COPY DRPA0408.
      *
       01 PO-DRPA0427.
          05 PO-DRPA0427-RC                         PIC  X(003).
             88 PO-DRPA0427-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0427-DATEN.
             10 PO-DRPA0427-PMBE.
                COPY DRPA0411.
      *
      ******* ENDE COPY-MEMBER **************************************
