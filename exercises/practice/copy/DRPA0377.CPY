      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0377.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0377.cpv  $
      *
      *   Rev 2.0   May 27 1999 12:10:16   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   18 Jul 1997 15:19:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0377  FUNKTION VWF PROVISION BERECHNEN   *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    21.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !21.07.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0377                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0377.
          05 PI-DRPA0377-PZVP-VERSION        PIC S9(004)  BINARY.
      *   05 PI-DRPA0377-DATEN.
      *
       01 PO-DRPA0377.
          05 PO-DRPA0377-RC                  PIC  X(003).
             88 PO-DRPA0377-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0377-RC-NOT-CHANGED                VALUE 'NCH'.
          05 PO-DRPA0377-DATEN.
             10 PO-DRPA0377-STRK-KZ          PIC  X(001).
      *
      ******** Ende Copy-Member **************************************
