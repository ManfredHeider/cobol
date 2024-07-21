      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1016.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1016.CPv  $
      *
      *   Rev 2.2   18 Jul 2006 09:37:32   t13ae46
      *APA00458: SF44 Textanzeige für PGV
      *
      *   Rev 2.1   Jan 18 2000 14:37:56   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   14 Jul 1998 13:59:06   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
     *****************************************************************
      *    COPY-MEMBER: DRPA1016  FUNKTION INSERT PGV                *
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
      * APA00458!20.06.06!2.2 !PI-DRPA1016-PGVZ-PGV-TEXT  ! WANG     *
      *         !        !    !HINZU                      !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1016                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT PGV     <<<                                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1016.
          05 PI-DRPA1016-PGVZ-KEY.
             10 PI-DRPA1016-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA1016-PGVZ-PGV           PIC  X(008).
             10 PI-DRPA1016-PGVZ-VZ-POSITIV    PIC  X(001).
             10 PI-DRPA1016-PGVZ-GUELTIG-AB    PIC  X(010).
             10 PI-DRPA1016-KNR-PGV            PIC S9(004)  BINARY.
          05 PI-DRPA1016-PGVZ-DATEN.
             10 PI-DRPA1016-PGVZ-GUELTIG-BIS   PIC  X(010).
200606       10 PI-DRPA1016-PGVZ-PGV-TEXT      PIC  X(042).
      *
       01 PO-DRPA1016.
          05 PO-DRPA1016-RC                    PIC  X(003).
             88 PO-DRPA1016-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1016-RC-DUPLICATE-VALUE           VALUE 'DVA'.
             88 PO-DRPA1016-RC-UNZULAESSIG               VALUE 'UNZ'.
      *
          05 PO-DRPA1016-MELDUNG.
             10 PO-DRPA1016-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1016-MELDUNG-NR         PIC S9(004) BINARY.

      ****** Ende Copy-Member ****************************************
