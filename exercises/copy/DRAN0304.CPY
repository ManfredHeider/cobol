      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0304.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0304.CPv  $
      *
      *   Rev 2.1   Aug 21 2015 12:03:00   t13ae34
      *AAN01219: Testprogramm für Funktionen
      *
      *   Rev 2.0   Aug 19 2015 11:06:44   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0304                                     *
      *    FUNKTION   : ERMITTLUNG KUNDENNUMMER AUS ANTRAGSBESTAND   *
      *                 MIT KI-VERTRAGSNUMMER UND/ODER KI-PIN        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                        *
      *    DATUM:    19.08.2015                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01219!19.08.15!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0304                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0304.
          05 PI-DRAN0304-DATEN.
             10 PI-DRAN0304-PGEB-GES             PIC S9(004) COMP.
             10 PI-DRAN0304-KI-VNR               PIC  X(015).
             10 PI-DRAN0304-KI-PIN               PIC S9(009) COMP.
      *
       01 PO-DRAN0304.
          05 PO-DRAN0304-RC                      PIC  X(003).
             88 PO-DRAN0304-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0304-RC-NFD                    VALUE 'NFD'.
          05 PO-DRAN0304-DATEN.
             10 PO-DRAN0304-KD-NR                PIC  S9(18)V COMP-3.
      ******** ENDE COPY-MEMBER **************************************
 

