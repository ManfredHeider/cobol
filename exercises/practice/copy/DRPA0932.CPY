      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0932.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0932.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:37:48   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0932  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    17.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!17.04.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0932                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0932.
          05 PI-DRPA0932-PROGRAMM           PIC  X(008).
          05 PI-DRPA0932-UFKT               PIC  9(002).
          05 PI-DRPA0932-DATEN.
             10 PI-DRPA0932-PRINTE          PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0932.
          05 PO-DRPA0932-RC                 PIC  X(003).
             88 PO-DRPA0932-RC-OK           VALUE 'OK '.
             88 PO-DRPA0932-RC-LEER         VALUE 'LER'.
             88 PO-DRPA0932-RC-EOF          VALUE 'EOF'.
          05 PO-DRPA0932-DATEN.
             10 PO-DRPA0932-PVER-VERSION    PIC S9(004) BINARY.
      *
      ***** ENDE-COPY-MEMBER *****************************************
