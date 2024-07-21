      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1355.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1355.CPv  $
      *
      *   Rev 2.1   12 Jul 2000 12:01:08   T13EX21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1355  FUNKTION INSERT STANDARD-OZP       *
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1355                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PGKF TBPA0027              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1355.
          05 PI-DRPA1355-UFKT                  PIC  X(002).
          05 PI-DRPA1355-PGKF-KEY.
             10 PI-DRPA1355-PGEB-GES             PIC S9(4) COMP.
             10 PI-DRPA1355-KTY-KONF-ART         PIC S9(4) COMP.
             10 PI-DRPA1355-KNR-KONF-ART         PIC S9(4) COMP.

          05 PI-DRPA1355-PGKF-DATEN.
             10 PI-DRPA1355-KTY-KONF-REGEL       PIC S9(4) COMP.
             10 PI-DRPA1355-KNR-KONF-REGEL       PIC S9(4) COMP.
             10 PI-DRPA1355-PGKF-KONF-Z-INFO     PIC S9(4) COMP.
             10 PI-DRPA1355-PGKF-LAST-UPDATE     PIC X(26).
             10 PI-DRPA1355-PGKF-KENNUNG         PIC X(8).
      *
       01 PO-DRPA1355.
          05 PO-DRPA1355-RC                   PIC  X(003).
             88 PO-DRPA1355-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1355-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ***** ENDE-COPY-MEMBER *****************************************
