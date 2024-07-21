      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1259.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1259.CPv  $
      *
      *   Rev 2.2   Jan 18 2000 15:01:16   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.1   Jan 18 2000 14:38:02   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1259  FUNKTION BEENDEN PGV ZUORDNUNG     *
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
      *    FÜR DIE FUNKTION DRPA1259                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  BEENDEN  PGV <<<                                     *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1259.
          05 PI-DRPA1259-PGVZ-KEY.
             10 PI-DRPA1259-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA1259-PGVZ-PGV           PIC  X(008).
             10 PI-DRPA1259-PGVZ-VZ-POSITIV    PIC  X(001).
             10 PI-DRPA1259-PGVZ-GUELTIG-AB    PIC  X(010).
             10 PI-DRPA1259-PGVZ-LAST-UPDATE   PIC  X(026).

          05 PI-DRPA1259-PGVZ-DATEN-NEU.
             10 PI-DRPA1259-PGVZ-GUELTIG-BIS   PIC  X(010).
             10 PI-DRPA1259-KNR-PGV            PIC  9(004).
      *
       01 PO-DRPA1259.
          05 PO-DRPA1259-RC                    PIC  X(003).
             88 PO-DRPA1259-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1259-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
