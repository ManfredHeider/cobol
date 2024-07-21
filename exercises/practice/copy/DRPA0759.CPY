      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0759.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0759.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:08:22   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0759  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    23.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0759                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0759.
          05 PI-DRPA0759-UFKT               PIC  9(002).
          05 PI-DRPA0759-DATEN              PIC  X(076).
      *
       01 PO-DRPA0759.
          05 PO-DRPA0759-RC                 PIC  X(003).
             88 PO-DRPA0759-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
