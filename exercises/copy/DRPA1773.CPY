      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1773.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1773.CPv  $
      *
      *   Rev 2.3   Apr 07 2016 10:14:48   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.2   Mar 10 2014 16:51:56   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.1   Dec 11 2013 15:36:46   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.0   Oct 02 2013 11:16:34   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1773  FUNKTION LIST-OZP FÜR TABELLE PRKO *
      *                                                 TBPA0033     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01539!02.10.13!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * APA02220!10.03.16!2.3 ! ÄNDERUNG FORMAT %-SÄTZE   ! STORCH
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1773                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1773.
          05 PI-DRPA1773-PROGRAMM            PIC  X(008).
          05 PI-DRPA1773-UFKT-CODE           PIC  9(002).
           05  PI-DRPA1773-AUSWAHL-KRITERIEN.
               10 PI-DRPA1773-PGEB-GES       PIC  S9(04) COMP.
      *
       01 PO-DRPA1773.
          05 PO-DRPA1773-RC                  PIC  X(003).
             88 PO-DRPA1773-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1773-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA1773-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA1773-DATEN.
             10 PO-DRPA1773-PGEB-GES             PIC S9(4) COMP.
             10 PO-DRPA1773-PROD-PROD-SCHL       PIC S9(4) COMP.
             10 PO-DRPA1773-PRKL-TARSCHL-GRP     PIC S9(9) COMP.
             10 PO-DRPA1773-PRKL-TARSCHL         PIC S9(9) COMP.
             10 PO-DRPA1773-KNR-TARIF-KZ         PIC S9(4) COMP.
             10 PO-DRPA1773-PRKL-LZ-VON          PIC S9(4) COMP.
             10 PO-DRPA1773-PRKL-LZ-BIS          PIC S9(4) COMP.
             10 PO-DRPA1773-PRKL-GUELTIG-VON     PIC X(10).
             10 PO-DRPA1773-PRKL-GUELTIG-BIS     PIC X(10).
             10 PO-DRPA1773-PRKL-KOND-NR         PIC S9(4) COMP.
100316*      10 PO-DRPA1773-PRKL-GRUND-PROV-MA   PIC S9(3)V9(3) COMP-3.
100316       10 PO-DRPA1773-PRKL-GRUND-PROV-MA   PIC S9(4)V9(4) COMP-3.
100316*      10 PO-DRPA1773-PRKL-PROV-SATZ-VWF   PIC S9(3)V9(3) COMP-3.
100316       10 PO-DRPA1773-PRKL-PROV-SATZ-VWF   PIC S9(4)V9(4) COMP-3.
100316*      10 PO-DRPA1773-PRKL-PROV-SATZ-VTF   PIC S9(3)V9(3) COMP-3.
100316       10 PO-DRPA1773-PRKL-PROV-SATZ-VTF   PIC S9(4)V9(4) COMP-3.
             10 PO-DRPA1773-PRKL-KUERZUNG-ART    PIC X(1).
             10 PO-DRPA1773-PRKL-KUERZUNG-AUF    PIC S9(4) COMP.
             10 PO-DRPA1773-KNR-PVT-KZ           PIC S9(4) COMP.
             10 PO-DRPA1773-PRKL-PROV-MIN        PIC S9(9) COMP.
             10 PO-DRPA1773-PRKL-PROV-MAX        PIC S9(9) COMP.
             10 PO-DRPA1773-KNR-KOND-REGEL-GRP   PIC S9(4) COMP.
             10 PO-DRPA1773-KNR-KOND-REGEL-GSP   PIC S9(4) COMP.

