      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0726.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA0726.CPv  $
      *
      *   Rev 2.4   Mar 10 2014 16:46:18   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.3   Jul 26 2013 11:41:28   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Nov 05 2003 09:17:20   t13ex44
      *FPA00644 - Korrektur Länge SST DRPA0726
      *
      *   Rev 2.1   Jun 04 1999 17:57:14   T13EX44
      *Korrektur Laenge
      *
      *   Rev 2.0   May 26 1999 15:15:50   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:43:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0726  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.OPRITZ                                        *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0022!22.12.97! 01 ! NEUERSTELLUNG             ! J.OPRITZ *
      *         !        !    !                           !          *
      * apa00012! 9.09.98! 1.1! Anpassung Datenlänge an   ! Parent   *
      *         !        !    ! DCLGEN CTPA0111           !          *
      *         !        !    !                           !          *
      * APA00012!04.06.99! 2.1! Korrektur Laenge          ! Scheidel *
      * FPA00644!05.11.03! 2.2! KORREKTUR LAENGE          ! SCHEIDEL *
260713* APA01539!26.07.13! 2.3! NEUE LÄNGE 0205 F. KALYPSO! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0726                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0726.
          05 PI-DRPA0726-UFKT               PIC  9(002).
090998*   05 PI-DRPA0726-DATEN              PIC  X(3962).
051103*   05 PI-DRPA0726-DATEN              PIC  X(0201).
260713*   05 PI-DRPA0726-DATEN              PIC  X(0202).
260713    05 PI-DRPA0726-DATEN              PIC  X(0205).
      *
       01 PO-DRPA0726.
          05 PO-DRPA0726-RC                 PIC  X(003).
             88 PO-DRPA0726-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
