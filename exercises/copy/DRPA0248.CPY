      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0248.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0248.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:00:04   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:14:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0248  FUNKTION UPD-DAT                   *
      *                 UPDATE DES PGVG-GUELT-BIS  DATUMS            *
      *                                    STANDARD-OZP              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    28.02.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AFPA0014 !28.02.97! 01 ! NEUERSTELLUNG             !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0248                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE PGEB-GES-GUELT-BIS<<<        PGVG TBPA0020    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0248.
          05 PI-DRPA0248-LAST-UPDATE           PIC  X(026).
          05 PI-DRPA0248-PGVG-KEY.
             10 PI-DRPA0248-PGEB-GES           PIC S9(004)  BINARY.
             10 PI-DRPA0248-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
             10 PI-DRPA0248-PGVG-GUELT-AB      PIC X(010).
          05 PI-DRPA0248-PGEB-DATEN.
             10 PI-DRPA0248-PGVG-GUELT-BIS     PIC X(010).
      *
       01 PO-DRPA0248.
          05 PO-DRPA0248-RC                  PIC  X(003).
             88 PO-DRPA0248-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0248-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0248-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
