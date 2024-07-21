      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1358.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1358.CPv  $
      *
      *   Rev 2.1   12 Jul 2000 12:01:14   T13EX21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1358  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    17.03.2000                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!17.03.00!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1358                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PGKF TBPA0027                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1358.
          05 PI-DRPA1358-PGKF-KEY.
             10 PI-DRPA1358-PGEB-GES             PIC S9(4) COMP.
             10 PI-DRPA1358-KTY-KONF-ART         PIC S9(4) COMP.
             10 PI-DRPA1358-KNR-KONF-ART         PIC S9(4) COMP.
      *
       01 PO-DRPA1358.
          05 PO-DRPA1358-RC                      PIC  X(003).
             88 PO-DRPA1358-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1358-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1358-PGKF-DATEN.
             10 PO-DRPA1358-PGEB-GES             PIC S9(4) COMP.
             10 PO-DRPA1358-KTY-KONF-ART         PIC S9(4) COMP.
             10 PO-DRPA1358-KNR-KONF-ART         PIC S9(4) COMP.
             10 PO-DRPA1358-KTY-KONF-REGEL       PIC S9(4) COMP.
             10 PO-DRPA1358-KNR-KONF-REGEL       PIC S9(4) COMP.
             10 PO-DRPA1358-PGKF-KONF-Z-INFO     PIC S9(4) COMP.
          05 PO-DRPA1358-PGKF-LAST-UPDATE        PIC X(26).
          05 PO-DRPA1358-PGKF-KENNUNG            PIC X(8).
      *
      ****** ENDE COPY-MEMBER *****************************************
