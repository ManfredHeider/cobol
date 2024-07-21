      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0394.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0394.CPv  $
      *
      *   Rev 2.0   May 26 1999 14:58:18   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:31:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0394  FUNKTION SELECT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0394                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  select akt <<<        VGEB TBPA0021                  *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0394.
          05 PI-DRPA0394-PGEB-KEY.
             10 PI-DRPA0394-KNR-VERTRIEB-GES PIC S9(004)  BINARY.
      *
       01 PO-DRPA0394.
          05 PO-DRPA0394-RC                  PIC  X(003).
             88 PO-DRPA0394-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0394-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0394-VGES-DATEN.
             10 PO-DRPA0394-VGES-GUELTIG-AB    PIC  X(026).
             10 PO-DRPA0394-VGES-GUELTIG-BIS   PIC  X(010).
             10 PO-DRPA0394-VGES-VWF           PIC S9(004)  BINARY.
             10 PO-DRPA0394-VGES-VTF           PIC S9(004)  BINARY.
             10 PO-DRPA0394-KTY-VERTRIEB-GES   PIC S9(004)  BINARY.
             10 PO-DRPA0394-KNR-WAEHRUNG       PIC S9(004)  BINARY.
             10 PO-DRPA0394-KTY-WAEHRUNG       PIC S9(004)  BINARY.
             10 PO-DRPA0394-KNR-AUSL           PIC S9(004)  BINARY.
             10 PO-DRPA0394-KTY-AUSL           PIC S9(004)  BINARY.
          05 PO-DRPA0394-VGES-LAST-UPDATE      PIC  X(026).
          05 PO-DRPA0394-VGES-KENNUNG        PIC  X(008).
      *
