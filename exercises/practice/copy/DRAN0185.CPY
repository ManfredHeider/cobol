      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0185.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0185.CPv  $
      *
      *   Rev 2.4   Dec 14 2011 16:59:06   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.3   Feb 13 2007 19:34:20   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 13 2007 18:33:28   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Feb 13 2007 17:27:52   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   08 Jan 2007 09:54:26   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0185                                     *
      *    FUNKTION   : PLAUSIBILITÄTEN MASKENFELDER ANTRAG ALLGEMEIN*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!15.11.06!2.1 !NEU                        !HEIDER    *
      * AAN01033!        !2.4 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0185                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0185.
             10 PI-DRAN0185-KOPF.
                COPY DRAN0141.
      *
             10 PI-DRAN0185-PRODUKTE.
                15 PI-DRAN0185-PRODUKT-TABELLE OCCURS 15.
                COPY DRAN0143.

      *
       01 PO-DRAN0185.
          05 PO-DRAN0185-RC                      PIC  X(003).
             88 PO-DRAN0185-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0185-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0185-DATEN.
             10 PO-DRAN0185-EINHEITEN.
                15 PO-DRAN0185-EINHEITEN-TAB OCCURS 15.
                   20 ANTP-EINH-AVERM            PIC S9(6)V9(2) COMP-3.
             10 PO-DRAN0185-FEHLERFLAG.
                15 PO-DRAN0185-FEHLERFLAG-TAB OCCURS 15.
                   20 PO-DRAN0185-FEHLERFLAG-T   PIC  X(001).
                   88 PO-DRAN0185-FLAG-OK             VALUE 'O'.
                   88 PO-DRAN0185-FLAG-NOK            VALUE 'N'.

             10 PO-DRAN0185-HINWEISTEXT          PIC  X(079).
      ******** ENDE COPY-MEMBER **************************************
