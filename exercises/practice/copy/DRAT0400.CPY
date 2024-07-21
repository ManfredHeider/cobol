      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0400.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0400.CPv  $
      *
      *   Rev 2.1   Jun 21 2004 16:39:22   t13ae34
      *Werkzeug für Auswertungen im eingereichten Geschäft
      *
      *   Rev 2.0   26 Jun 2001 12:55:22   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0400 LIST ZXAT0017                       *
      *                          JEW. JÜNGSTEN EINTRAG PGEB          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    21.05.2004                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00492!21.05.04!2.2 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0400                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0400.
           05  PI-DRAT0400-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0400-AUSWAHL-KRITERIEN.
             10 FILLER                         PIC  X(1).

      *
       01 PO-DRAT0400.
          05 PO-DRAT0400-RC                    PIC  X(003).
             88 PO-DRAT0400-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0400-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0400-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0400-PGES-DATEN.
             10 PO-DRAT0400-KNR-PROD-GB-GES    PIC S9(4) USAGE COMP.
             10 PO-DRAT0400-PGES-DAT-BIS       PIC  X(10).
      *
      ******** ENDE COPY-MEMBER **************************************
