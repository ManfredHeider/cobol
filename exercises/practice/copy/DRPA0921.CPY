      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0921.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0921.CPv  $
      *
      *   Rev 2.3   18 Jul 2006 09:36:28   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.2   Jan 18 2000 15:19:40   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.1   Jan 18 2000 14:37:46   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:14:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA0921  FUNKTION SELECT ALL STANDARD-OZP
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
      * APA00458!28.04.06! 2.3!PO-DRPA0921-PGVZ-PGV-TEXT  ! WANG
      *         !        !    !HINZU                      !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0921
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ALL <<<        PGVZ TBPA0026
      *
      ******************************************************************
      *
       01 PI-DRPA0921.
          05 PI-DRPA0921-PGVZ-KEY.
             10 PI-DRPA0921-PGEB-GES              PIC S9(004)  BINARY.
             10 PI-DRPA0921-PGVZ-PGV              PIC  X(008).
             10 PI-DRPA0921-PGVZ-VZ-POSITIV       PIC  X(001).
             10 PI-DRPA0921-PGVZ-GUELTIG-AB       PIC  X(010).
      *
       01 PO-DRPA0921.
          05 PO-DRPA0921-PGVZ-DATEN.
             10 PO-DRPA0921-PGEB-GES              PIC S9(004)  BINARY.
             10 PO-DRPA0921-PGVZ-PGV              PIC  X(008).
             10 PO-DRPA0921-PGVZ-VZ-POSITIV       PIC  X(001).
             10 PO-DRPA0921-PGVZ-GUELTIG-AB       PIC  X(010).
             10 PO-DRPA0921-PGVZ-GUELTIG-BIS      PIC  X(010).
             10 PO-DRPA0921-KNR-PGV               PIC S9(004)  BINARY.
             10 PO-DRPA0921-PGVZ-LAST-UPDATE      PIC  X(026).
             10 PO-DRPA0921-PGVZ-KENNUNG          PIC  X(010).
280406       10 PO-DRPA0921-PGVZ-PGV-TEXT         PIC  X(042).

          05 PO-DRPA0921-RC                       PIC  X(003).
             88 PO-DRPA0921-RC-OK                      VALUE 'OK '.
             88 PO-DRPA0921-RC-NOT-FOUND               VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ******************************************
