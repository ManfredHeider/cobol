      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1769.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1769.CPv  $
      *
      *   Rev 2.3   Apr 07 2016 10:14:20   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.2   Mar 10 2014 16:51:34   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Dec 11 2013 15:36:30   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.0   Oct 01 2013 10:29:42   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1769                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF   LIST ZU PRKL                             *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA01539!02.10.13! 2.1 ! NEUERSTELLUNG            ! RAHNER   *
      * APA02220!10.03.16! 2.3 ! ÄNDERUNG FORMAT %-SÄTZE  ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1769                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1769.
          05 PI-DRPA1769-PROGRAMM       PIC  X(008).
          05 PI-DRPA1769-UFKT           PIC  9(002).
      *
       01 PO-DRPA1769.
          05 PO-DRPA1769-RC             PIC  X(03).
             88 PO-DRPA1769-RC-OK           VALUE 'OK '.
             88 PO-DRPA1769-RC-EOF          VALUE 'EOF'.
             88 PO-DRPA1769-RC-LEER         VALUE 'LER'.
          05 PO-DRPA1769-DATEN.
             10 PO-DRPA1769-PGEB-GES             PIC S9(4) COMP.
             10 PO-DRPA1769-PROD-PROD-SCHL       PIC S9(4) COMP.
             10 PO-DRPA1769-PRKL-TARSCHL-GRP     PIC S9(9) COMP.
             10 PO-DRPA1769-PRKL-TARSCHL         PIC S9(9) COMP.
             10 PO-DRPA1769-KNR-TARIF-KZ         PIC S9(4) COMP.
             10 PO-DRPA1769-PRKL-LZ-VON          PIC S9(4) COMP.
             10 PO-DRPA1769-PRKL-LZ-BIS          PIC S9(4) COMP.
             10 PO-DRPA1769-PRKL-GUELTIG-VON     PIC X(10).
             10 PO-DRPA1769-PRKL-GUELTIG-BIS     PIC X(10).
             10 PO-DRPA1769-PRKL-KOND-NR         PIC S9(4) COMP.
100316*      10 PO-DRPA1769-PRKL-GRUND-PROV-MA   PIC S9(3)V9(3) COMP-3.
100316       10 PO-DRPA1769-PRKL-GRUND-PROV-MA   PIC S9(4)V9(4) COMP-3.
100316*      10 PO-DRPA1769-PRKL-PROV-SATZ-VWF   PIC S9(3)V9(3) COMP-3.
100316       10 PO-DRPA1769-PRKL-PROV-SATZ-VWF   PIC S9(4)V9(4) COMP-3.
100316*      10 PO-DRPA1769-PRKL-PROV-SATZ-VTF   PIC S9(3)V9(3) COMP-3.
100316       10 PO-DRPA1769-PRKL-PROV-SATZ-VTF   PIC S9(4)V9(4) COMP-3.
             10 PO-DRPA1769-PRKL-KUERZUNG-ART    PIC X(1).
             10 PO-DRPA1769-PRKL-KUERZUNG-AUF    PIC S9(4) COMP.
             10 PO-DRPA1769-KNR-PVT-KZ           PIC S9(4) COMP.
             10 PO-DRPA1769-PRKL-PROV-MIN        PIC S9(9) COMP.
             10 PO-DRPA1769-PRKL-PROV-MAX        PIC S9(9) COMP.
             10 PO-DRPA1769-KNR-KOND-REGEL-GRP   PIC S9(4) COMP.
             10 PO-DRPA1769-KNR-KOND-REGEL-GSP   PIC S9(4) COMP.


