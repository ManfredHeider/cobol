      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1596.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA1596.CPv  $
      *
      *   Rev 2.8   Mar 24 2020 06:51:26   t13ex67
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - OZP's
      *
      *   Rev 2.7   Oct 12 2018 13:35:12   t13ae37
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.6   Oct 02 2015 09:15:10   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.5   May 04 2015 11:32:02   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System OZP's
      *
      *   Rev 2.4   Mar 10 2014 16:49:52   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.3   Feb 28 2014 15:52:54   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.2   Dec 10 2007 13:31:22   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Oct 12 2004 08:13:58   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 15:03:00   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1596  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA00622!04.06.07! 2.2! ERWEITERUNG UM 2 STELLEN  ! STORCH   *
      *         !        !    ! WG.NEUEM ATTRIBUT KNR-VGES!          *
      * APA01539!28.02.14! 2.3! ERWEITERUNG FÜR KALYPSO   ! RAHNER   *
      * APA01932!31.03.15! 2.4! SATZLÄNGE ANPASSEN (LUCY) ! KÖPRÜCÜ  *
      *         !02.10.15! 2.5! SATZLÄNGE ANPASSEN (VP-NR)! KÖPRÜCÜ  *
      * APA02579!12.10.18! 2.7! SATZLÄNGE ANPASSEN (FRIWI)! BUCH     *
      * APA02884!19.11.19! 2.8! SATZLÄNGE ANPASSEN        ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1596                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1596.
           05 PI-DRPA1596-UFKT         PIC  9(002).
310315*    05 PI-DRPA1596-DATEN        PIC  X(572).
121018*    05 PI-DRPA1596-DATEN        PIC  X(596).
191119     05 PI-DRPA1596-DATEN        PIC  X(598).
      *
        01 PO-DRPA1596.
           05 PO-DRPA1596-RC           PIC  X(003).
              88 PO-DRPA1596-RC-OK          VALUE 'OK '.
 

