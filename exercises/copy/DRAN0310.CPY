      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0310.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0310.CPv  $
      *
      *   Rev 2.1   Jul 04 2016 10:05:56   t13ae34
      *AAN01269: Anträge Fondsdepotbank - Berücksichtigung Fremdfonds
      *
      *   Rev 2.0   Jul 01 2016 14:44:06   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0310                                     *
      *    FUNKTION   : ERMITTLUNG DVAG-PRODUKTSCHLÜSSEL ZU FREMDFONS*
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                        *
      *    DATUM:    01.07.2016                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01269!01.07.16!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0310                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0310.
          05 PI-DRAN0310-DATEN.
             10 PI-DRAN0310-PGEB-GES-QUELLE      PIC S9(004) COMP.
             10 PI-DRAN0310-PS-QUELLE            PIC S9(004) COMP.
             10 PI-DRAN0310-PGEB-GES-ZIEL        PIC S9(004) COMP.
      *
       01 PO-DRAN0310.
          05 PO-DRAN0310-RC                      PIC  X(003).
             88 PO-DRAN0310-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0310-RC-NFD                    VALUE 'NFD'.
          05 PO-DRAN0310-DATEN.
             10 PO-DRAN0310-PS-ZIEL              PIC S9(004) COMP.
      ******** ENDE COPY-MEMBER **************************************



