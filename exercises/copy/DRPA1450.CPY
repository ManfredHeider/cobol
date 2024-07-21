      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1450.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1450.CPv  $
      *
      *   Rev 2.4   May 22 2018 08:42:38   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.3   Mar 10 2014 16:49:02   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jul 26 2013 11:43:34   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   05 Feb 2001 10:20:12   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 47
      *(BFG IMMOINVEST)
      *
      *   Rev 2.0   12 Jul 2000 12:33:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1450  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: A. Brenner                                      *
      *    DATUM:    01.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! Neuerstellung             ! Brenner  *
260713* APA01539!26.07.13!2.2 ! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA02579!24.04.18!2.4 ! ERWEITERUNG FUER FRIWI    ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1450                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1450.
          05 PI-DRPA1450-UFKT               PIC  9(002).
240418    05 PI-DRPA1450-DATEN              PIC  X(791).
      *
       01 PO-DRPA1450.
          05 PO-DRPA1450-RC                 PIC  X(003).
             88 PO-DRPA1450-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************

