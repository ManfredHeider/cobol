      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0429.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0429.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:42:26   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:35:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0429                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMPR ERSTELLEN      >>                   *
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
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0429                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0429.
          05 PI-DRPA0429-DATEN.
             10 PI-DRPA0429-PMPP.
                COPY DRPA0297.
             10 PI-DRPA0429-PVER.
                COPY DRPA0408.
             10 PI-DRPA0429-PMBE.
                COPY DRPA0411.
      *
       01 PO-DRPA0429.
          05 PO-DRPA0429-RC                         PIC  X(003).
             88 PO-DRPA0429-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0429-DATEN.
             10 PO-DRPA0429-PMPR.
                COPY DRPA0412.
      *
      ******* ENDE COPY-MEMBER **************************************
