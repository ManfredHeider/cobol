      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1600.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1600.CPv  $
      *
      *   Rev 2.6   Oct 12 2018 13:35:30   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.5   Apr 15 2016 13:23:52   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.4   Nov 24 2015 09:30:24   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.3   Mar 10 2014 16:50:06   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Feb 28 2014 15:53:28   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.1   Oct 12 2004 08:14:34   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 15:40:48   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1600  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA01539!28.02.14! 2.2! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA01932!24.11.15! 2.3! ERWEITERUNG FÜR LUCY      ! KÖPRÜCÜ  *
      * APA02220!15.04.16! 2.5! NEUE LÄNGE 204 /4 VOR-UND ! WANG     *
      *         !        !    ! NACHKOMMASTELLEN/PROVSATZ !          *
      * APA02579!12.10.18! 2.6! NEUE LÄNGE FriWind        ! Buch     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1600                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT-QSAM <<<                                      *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1600.
           05 PI-DRPA1600-UFKT         PIC  9(002).
280214*    05 PI-DRPA1600-DATEN        PIC  X(189).
280214*    05 PI-DRPA1600-DATEN        PIC  X(193).
150416*    05 PI-DRPA1600-DATEN        PIC  X(203).
150416*    05 PI-DRPA1600-DATEN        PIC  X(204).
121018     05 PI-DRPA1600-DATEN        PIC  X(214).
      *
        01 PO-DRPA1600.
           05 PO-DRPA1600-RC           PIC  X(003).
              88 PO-DRPA1600-RC-OK          VALUE 'OK '.
 
 

