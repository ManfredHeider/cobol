      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0186.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0186.CPv  $
      *
      *   Rev 2.3   Feb 13 2007 19:34:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Feb 13 2007 18:33:42   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Feb 13 2007 17:28:06   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Jan 25 2007 09:57:06   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0186                                     *
      *    FUNKTION   : PLAUSIBILITÄTEN BERECHTIGUNG VB AUF PRODUKTE *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!25.01.07!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0186                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0186.
          05 PI-DRAN0186-DATEN.
             10 PI-DRAN0186-VB-NR                PIC S9(009) COMP.
      *
             10 PI-DRAN0186-PRODUKTE.
                15 PI-DRAN0186-PRODUKT-TABELLE OCCURS 15.
                COPY DRAN0143.

      *
       01 PO-DRAN0186.
          05 PO-DRAN0186-RC                      PIC  X(003).
             88 PO-DRAN0186-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0186-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0186-DATEN.
             10 PO-DRAN0186-FEHLERFLAG.
                15 PO-DRAN0186-FEHLERFLAG-TAB OCCURS 15.
                   20 PO-DRAN0186-FEHLERFLAG-T   PIC  X(001).
                   88 PO-DRAN0186-FLAG-OK             VALUE 'O'.
                   88 PO-DRAN0186-FLAG-NOK            VALUE 'N'.

             10 PO-DRAN0186-HINWEISTEXT          PIC  X(079).
      ******** ENDE COPY-MEMBER **************************************
