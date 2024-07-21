      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1367.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1367.CPv  $
      *
      *   Rev 2.1   Apr 10 2000 10:24:06   T13EX44
      *Neu
      *
      *   Rev 2.0   Mar 01 2000 09:58:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1367                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0035                        *
      *        FUNKTION: PRODART ABLEITEN AUS PGV, NUR AP-PROVISIONEN *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    07.04.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00067!07.04.00!2.00! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1367                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1367.
          05 PI-DRPA1367-ZVP-VERSION        PIC S9(004) BINARY.
          05 PI-DRPA1367-PGV                PIC S9(004) BINARY.
      *
       01 PO-DRPA1367.
          05 PO-DRPA1367-RC                 PIC   X(003).
             88 PO-DRPA1367-RC-OK              VALUE  'OK '.
          05 PO-DRPA1367-PRODART            PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

