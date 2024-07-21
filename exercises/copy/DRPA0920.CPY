      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0920.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0920.CPv  $
      *
      *   Rev 2.1   Jan 18 2000 14:37:42   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   10 Mar 1998 15:36:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA0920  FUNKTION DELETE STANDARD-OZP
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
      *    FÜR DIE FUNKTION DRPA0920
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE <<<        PGVZ TBPA0026
      *
      ******************************************************************
      *
       01 PI-DRPA0920.
          05 PI-DRPA0920-PGVZ-KEY.
             10 PI-DRPA0920-PGEB-GES              PIC S9(004)  BINARY.
             10 PI-DRPA0920-PGVZ-PGV              PIC  X(008).
             10 PI-DRPA0920-PGVZ-VZ-POSITIV       PIC  X(001).
             10 PI-DRPA0920-PGVZ-GUELTIG-AB       PIC  X(010).
             10 PI-DRPA0920-PGVZ-LAST-UPDATE      PIC  X(026).
      *
       01 PO-DRPA0920.
          05 PO-DRPA0920-RC                       PIC  X(003).
             88 PO-DRPA0920-RC-OK                      VALUE 'OK '.
             88 PO-DRPA0920-RC-CHANGED                 VALUE 'CHA'.
             88 PO-DRPA0920-RC-NOT-FOUND               VALUE 'NFD'.


      *
      ****** ENDE COPY-MEMBER ******************************************

