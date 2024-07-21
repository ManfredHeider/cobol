      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0426.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0426.CPv  $
      *
      *   Rev 2.1   04 Dec 2006 14:30:00   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   16 Nov 2006 19:06:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0426                                     *
      *    FUNKTION   : POSTLEITZAHLEN - ERMITTELN ERSTEN ORTSNAMEN  *
      *                 ZU EINER  POSTLEITZAHL                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!16.11.06!2.1 !NEU                        ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0304                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0426.
          05 PI-DRAT0426-DATEN.
      *         MUSS-FELD
             10 PI-DRAT0426-PLZ                  PIC  X(005).
      *
       01 PO-DRAT0426.
          05 PO-DRAT0426-RC                      PIC  X(003).
      *         EIN ODER MEHRERE ORTE
             88 PO-DRAT0426-RC-OK                     VALUE 'OK '.
      *         KEIN ORT GEFUNDEN
             88 PO-DRAT0426-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRAT0426-DATEN.
             10 PO-DRAT0426-ORT                  PIC  X(035).
      *
      ******** ENDE COPY-MEMBER **************************************

