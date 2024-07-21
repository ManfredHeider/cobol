      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1323.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1323.cpv  $
      *
      *   Rev 2.1   Jan 18 2000 14:44:48   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Dec 14 1999 16:42:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA1323  FUNKTION BEENDEN PGV-ZUORDNUNG
      *    -------------------------------------------------------
      *    ERSTELLT: MUND
      *    DATUM:    18.11.99
      ******************************************************************
      *    VERSION   01
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * APA00071!18.11.99! 01 ! NEUERSTELLUNG             ! MUND
      *         !        !    !                           !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1323
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  BEENDEN PGV-ZUORDNUNG  <<< PGVZ TBPA0026
      *
      ******************************************************************
      *
       01 PI-DRPA1323.
          05 PI-DRPA1323-PGVZ-KEY.
             10 PI-DRPA1323-PGEB-GES              PIC S9(004)  BINARY.
             10 PI-DRPA1323-PGVZ-PGV              PIC  X(008).
             10 PI-DRPA1323-PGVZ-VZ-POSITIV       PIC  X(001).
             10 PI-DRPA1323-PGVZ-GUELTIG-AB       PIC  X(010).
             10 PI-DRPA1323-PGVZ-LAST-UPDATE      PIC  X(026).
          05 PI-DRPA1323-PGVZ-DATEN.
             10 PI-DRPA1323-PGVZ-GUELTIG-BIS      PIC  X(010).
      *
       01 PO-DRPA1323.
          05 PO-DRPA1323-RC                       PIC  X(003).
             88 PO-DRPA1323-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1323-RC-CHANGED                 VALUE 'CHA'.
             88 PO-DRPA1323-RC-NOT-FOUND               VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ******************************************
