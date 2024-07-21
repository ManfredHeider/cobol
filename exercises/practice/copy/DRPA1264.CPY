      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1264.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1264.CPv  $
      *
      *   Rev 2.1   Jan 18 2000 14:42:48   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Mar 30 1999 08:44:18   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1264  FUNKTION PGV PRÜFEN & FORMATIEREN  *
      *                                     FÜR UPPA0029             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1264                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  PGV PRÜFEN & FORMATIEREN   <<<                       *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1264.
          05 PI-DRPA1264-DATEN.
             10 PI-DRPA1264-PGEB-GES     PIC S9(004)  BINARY.
             10 PI-DRPA1264-PGVZ-PGV     PIC  X(008).
      *
       01 PO-DRPA1264.
          05 PO-DRPA1264-RC              PIC  X(003).
             88 PO-DRPA1264-RC-OK             VALUE 'OK '.
             88 PO-DRPA1264-RC-UNZULAESSIG    VALUE 'UNZ'.
             88 PO-DRPA1264-RC-PGV-GEN        VALUE 'GEN'.

          05 PO-DRPA1264-PGVZ-PGV        PIC  X(008).
      *
          05 PO-DRPA1264-MELDUNG.
             10 PO-DRPA1264-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1264-MELDUNG-NR         PIC S9(004) BINARY.

      ****** ENDE COPY-MEMBER ****************************************
