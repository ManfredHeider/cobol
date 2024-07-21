      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1492.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1492.CPv  $
      *
      *   Rev 2.1   15 May 2001 11:41:04   t13ex21
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   10 Apr 2001 08:42:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1492 FUNKTION 'SET DIALOGSTATUS-GRUPPE'  *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    24.04.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!24.04.01!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1492                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1492.
          05 PI-DRPA1492-PGEB-GES              PIC S9(04) BINARY.
          05 PI-DRPA1492-DIALOGSTAT-GRUPPE     PIC S9(04) BINARY.
          05 PI-DRPA1492-DIALOGSTAT-ZUL        PIC S9(04) BINARY.
          05 PI-DRPA1492-PGKF-LAST-UPDATE      PIC  X(26).

       01 PO-DRPA1492.
          05 PO-DRPA1492-RC                    PIC  X(003).
             88 PO-DRPA1492-RC-OK                   VALUE 'OK '.
             88 PO-DRPA1492-RC-CHANGED              VALUE 'CHG'.
      *
      ******** ENDE COPY-MEMBER **************************************
