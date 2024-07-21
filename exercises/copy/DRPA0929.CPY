      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0929.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0929.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:37:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0926  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: bohlinger                                       *
      *    DATUM:    24.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.11.98! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0929                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST PVER <<<                                        *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0929.
          05 PI-DRPA0929-PROGRAMM           PIC  X(008).
          05 PI-DRPA0929-UFKT               PIC  9(002).
          05 PI-DRPA0929-DATEN.
             10 PI-DRPA0929-PRINTE          PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0929.
          05 PO-DRPA0929-RC                 PIC  X(003).
             88 PO-DRPA0929-RC-OK           VALUE 'OK '.
             88 PO-DRPA0929-RC-LEER         VALUE 'LER'.
             88 PO-DRPA0929-RC-EOF          VALUE 'EOF'.
          05 PO-DRPA0929-DATEN.
             COPY DRPA0408.
      *
      ***** ENDE-COPY-MEMBER *****************************************
