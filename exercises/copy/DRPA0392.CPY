      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0392.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0392.CPv  $
      *
      *   Rev 2.0   May 26 1999 14:58:14   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:31:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0392  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    28.07.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !28.07.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0392                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              * T1306.GL0
      * 1.CL>>  update <<<        VGEB TBPA0021                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0392.
          05 PI-DRPA0392-LAST-UPDATE           PIC  X(026).
          05 PI-DRPA0392-PGEB-KEY.
             10 PI-DRPA0392-KNR-VERTRIEB-GES   PIC S9(004)  BINARY.
          05 PI-DRPA0392-VGES-DATEN.
             10 PI-DRPA0392-VGES-GUELT-BIS     PIC  X(010).
             10 PI-DRPA0392-VGES-VWF           PIC S9(004)  BINARY.
             10 PI-DRPA0392-VGES-VTF           PIC S9(004)  BINARY.
             10 PI-DRPA0392-KTY-VERTRIEB-GES   PIC S9(004)  BINARY.
             10 PI-DRPA0392-KNR-WAEHRUNG       PIC S9(004)  BINARY.
             10 PI-DRPA0392-KNR-AUSL           PIC S9(004)  BINARY.
      *
       01 PO-DRPA0392.
          05 PO-DRPA0392-RC                  PIC  X(003).
             88 PO-DRPA0392-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0392-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0392-RC-NOT-FOUND                  VALUE 'NFD'.
      *
