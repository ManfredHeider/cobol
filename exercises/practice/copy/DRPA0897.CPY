      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0897.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0897.CPv  $
      *
      *   Rev 2.2   18 Jul 2006 09:36:16   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.1   Jan 18 2000 14:37:38   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jan 1998 12:49:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRPA0897  FUNKTION INSERT STANDARD-OZP
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
      * APA00458!28.04.06! 2.2! PI-DRPA0897-PGVZ-PGV-TEXT ! WANG
      *         !        !    ! HINZU                     !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0897
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT <<<        PGVZ TBPA0026
      *
      *
      ******************************************************************
      *
       01 PI-DRPA0897.
          05 PI-DRPA0897-PGVZ-DATEN.
             10 PI-DRPA0897-PGEB-GES              PIC S9(004)  BINARY.
             10 PI-DRPA0897-PGVZ-PGV              PIC  X(008).
             10 PI-DRPA0897-PGVZ-VZ-POSITIV       PIC  X(001).
             10 PI-DRPA0897-PGVZ-GUELTIG-AB       PIC  X(010).
             10 PI-DRPA0897-PGVZ-GUELTIG-BIS      PIC  X(010).
             10 PI-DRPA0897-KNR-PGV               PIC S9(004)  BINARY.
280406       10 PI-DRPA0897-PGVZ-PGV-TEXT         PIC  X(042).
      *
       01 PO-DRPA0897.
          05 PO-DRPA0897-RC                       PIC  X(003).
             88 PO-DRPA0897-RC-OK                      VALUE 'OK '.
             88 PO-DRPA0897-RC-DUPLICATE-VALUE         VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ******************************************
