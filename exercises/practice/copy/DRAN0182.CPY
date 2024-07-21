      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.12.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0182.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0182.CPv  $
      *
      *   Rev 2.2   Dec 14 2011 16:58:58   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.1   22 Dec 2006 21:01:16   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   11 Dec 2006 14:47:12   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0182                                     *
      *    FUNKTION   : PLAUSIBILITÄTEN MASKENFELDER ANTRAG ALLGEMEIN*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!15.11.06!2.1 !NEU                        !HEIDER    *
      * AAN01033!        !2.2 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0182                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0182.
             10 PI-DRAN0182-KOPF.
                COPY DRAN0141.
      *
             10 PI-DRAN0182-PRODUKTE.
                15 PI-DRAN0182-PRODUKT-TABELLE OCCURS 15.
                COPY DRAN0143.

      *
       01 PO-DRAN0182.
          05 PO-DRAN0182-RC                      PIC  X(003).
             88 PO-DRAN0182-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0182-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0182-DATEN.
             10 PO-DRAN0182-EINHEITEN.
                15 PO-DRAN0182-EINHEITEN-TAB OCCURS 15.
                   20 ANTP-EINH-AVERM            PIC S9(6)V9(2) COMP-3.
             10 PO-DRAN0182-FEHLERFLAG.
                15 PO-DRAN0182-FEHLERFLAG-TAB OCCURS 15.
                   20 PO-DRAN0182-FEHLERFLAG-T   PIC  X(001).
                   88 PO-DRAN0182-FLAG-OK             VALUE 'O'.
                   88 PO-DRAN0182-FLAG-NOK            VALUE 'N'.

             10 PO-DRAN0182-HINWEISTEXT          PIC  X(079).
      ******** ENDE COPY-MEMBER **************************************
