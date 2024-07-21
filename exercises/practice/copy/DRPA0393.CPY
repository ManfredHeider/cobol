      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0393.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0393.CPv  $
      *
      *   Rev 2.0   May 26 1999 14:58:16   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:31:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0393  FUNKTION DELETE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0393                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<        VGEB TBPA0021                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0393.
          05 PI-DRPA0393-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0393-VGES-KEY.
             10 PI-DRPA0393-KNR-VERTRIEB-GES PIC S9(004)  BINARY.
      *
       01 PO-DRPA0393.
          05 PO-DRPA0393-RC                  PIC  X(003).
             88 PO-DRPA0393-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0393-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0393-RC-NOT-FOUND                  VALUE 'NFD'.
      *
