      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1357.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1357.CPv  $
      *
      *   Rev 2.1   12 Jul 2000 12:01:12   T13EX21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   Mar 01 2000 09:58:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1357  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    17.03.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!17.03.00!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1357                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       PGKF TBPA0027                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1357.
          05 PI-DRPA1357-LAST-UPDATE            PIC  X(026).
          05 PI-DRPA1357-PGKF-KEY.
             10 PI-DRPA1357-PGEB-GES             PIC S9(4) COMP.
             10 PI-DRPA1357-KTY-KONF-ART         PIC S9(4) COMP.
             10 PI-DRPA1357-KNR-KONF-ART         PIC S9(4) COMP.
      *
       01 PO-DRPA1357.
          05 PO-DRPA1357-RC                     PIC  X(003).
             88 PO-DRPA1357-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1357-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA1357-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
