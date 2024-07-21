      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1526.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1526.CPv  $
      *
      *   Rev 2.1   Apr 15 2002 16:39:46   T13ex22
      *FPA00346: Neue Funktionen für CRPA0108
      *
      *   Rev 2.0   04 Jul 2001 11:11:14   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1526  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: THOMAS MUND                                     *
      *    DATUM:    28.02.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00346!28.02.02! 01 ! NEUERSTELLUNG             ! MUND     *
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
      *    >>>  LIST PRINTEN ZU REOB-VERTRÄGEN MIT VERSION > 1       *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1526.
          05 PI-DRPA1526-PROGRAMM           PIC  X(008).
          05 PI-DRPA1526-UFKT               PIC  9(002).
          05 PI-DRPA1526-DATEN.
             10 PI-DRPA1526-PRINTE          PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA1526.
          05 PO-DRPA1526-RC                 PIC  X(003).
             88 PO-DRPA1526-RC-OK           VALUE 'OK '.
             88 PO-DRPA1526-RC-LEER         VALUE 'LER'.
             88 PO-DRPA1526-RC-EOF          VALUE 'EOF'.
          05 PO-DRPA1526-DATEN.
             COPY DRPA0408.
      *
      ***** ENDE-COPY-MEMBER *****************************************
