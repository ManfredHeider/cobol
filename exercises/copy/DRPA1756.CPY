      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1756.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1756.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 16:50:44   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Dec 11 2013 15:33:12   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *- Kollektivv
      *erträge-
      *
      *   Rev 2.1   Aug 21 2013 14:13:24   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *- Kollektivv
      *erträge-
      *
      *   Rev 2.0   Aug 19 2013 10:01:40   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1756                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1756                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01539!19.08.13!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1756
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1756.
           05 PI-DRPA1756-PRSS-VERM-GES    PIC S9(004) BINARY.
           05 PI-DRPA1756-KNR-RAB-ST       PIC S9(004) BINARY.
           05 PI-DRPA1756-KNR-ZAHLW        PIC S9(004) BINARY.
           05 PI-DRPA1756-PRSS-GUELTIG-VON PIC X(010).
           05 PI-DRPA1756-PRSS-GUELTIG-BIS PIC X(010).

       01  PO-DRPA1756.
           05 PO-DRPA1756-RC              PIC X(003).
              88 PO-DRPA1756-RC-OK            VALUE 'OK '.
              88 PO-DRPA1756-RC-NOT-FOUND     VALUE 'NFD'.
      *
           05 PO-DRPA1756-PRSS-SATZ-AP       PIC S9(3)V9(6) COMP-3.
           05 PO-DRPA1756-PRSS-SATZ-PRO-RATA PIC S9(3)V9(6) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

