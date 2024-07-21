      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1356.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1356.CPv  $
      *
      *   Rev 2.1   12 Jul 2000 12:01:10   T13EX21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1356  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    17.03.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!17.03.00!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1356                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       PGKF TBPA0027                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1356.
          05 PI-DRPA1356-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA1356-PGKF-KEY.
             10 PI-DRPA1356-PGEB-GES             PIC S9(4) COMP.
             10 PI-DRPA1356-KTY-KONF-ART         PIC S9(4) COMP.
             10 PI-DRPA1356-KNR-KONF-ART         PIC S9(4) COMP.

          05 PI-DRPA1356-PGKF-DATEN.
             10 PI-DRPA1356-KTY-KONF-REGEL       PIC S9(4) COMP.
             10 PI-DRPA1356-KNR-KONF-REGEL       PIC S9(4) COMP.
             10 PI-DRPA1356-PGKF-KONF-Z-INFO     PIC S9(4) COMP.
             10 PI-DRPA1356-PGKF-LAST-UPDATE     PIC X(26).
             10 PI-DRPA1356-PGKF-KENNUNG         PIC X(8).
      *
       01 PO-DRPA1356.
          05 PO-DRPA1356-RC                   PIC  X(003).
             88 PO-DRPA1356-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1356-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA1356-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************************************************************
