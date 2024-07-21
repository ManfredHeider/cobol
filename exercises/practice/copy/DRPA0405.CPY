      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0405.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0405.cpv  $
      *
      *   Rev 2.0   May 27 1999 12:24:56   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   23 Jul 1997 12:32:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0405  FUNKTION Rueckstellung ermitteln   *
      *                           (UPPA0048)                         *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    06.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !06.08.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0405                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0405.
          05 PI-DRPA0405-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRPA0405-DATEN.
             10 PI-DRPA0405-ST-NR            PIC S9(009)  BINARY.
             10 PI-DRPA0405-PROV             PIC S9(009)V99
                                             PACKED-DECIMAL.
      *
       01 PO-DRPA0405.
          05 PO-DRPA0405-RC                  PIC  X(003).
             88 PO-DRPA0405-RC-OK                    VALUE 'OK '.
             88 PO-DRPA0405-RC-VSTAT-GEFUELLT        VALUE 'VSG'.
          05 PO-DRPA0405-DATEN.
             10 PO-DRPA0405-AUSZAHL-BTRG     PIC S9(009)V99
                                             PACKED-DECIMAL.
             10 PO-DRPA0405-RCK-BTRG         PIC S9(009)V99
                                             PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************
