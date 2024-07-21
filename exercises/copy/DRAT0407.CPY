      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.11.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0407.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0407.CPv  $
      *
      *   Rev 2.3   Feb 21 2007 12:35:16   t13ae34
      *ACV00054: Change Request CTV-Service-Funktion
      *
      *   Rev 2.2   Feb 15 2007 10:24:58   t13ae34
      *ACV00054: Anpassung Driver-Rahmen -> über AAN00655
      *
      *   Rev 2.1   Jan 27 2006 11:38:02   t13ae34
      *ACV00054: Anpassung Driver-Rahmen
      *
      *   Rev 2.0   Nov 24 2005 17:30:52   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0407 LIST ZXAT0008                       *
      *                          MIT KTY-NR, KNR-BEZ10               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    28.11.0599                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00054!28.11.05!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0407                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0407.
           05  PI-DRAT0407-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0407-AUSWAHL-KRITERIEN.
             10 PI-DRAT0407-KTY-NR             PIC S9(4) USAGE COMP.
             10 PI-DRAT0407-KNR-BEZ10          PIC  X(10).

      *
       01 PO-DRAT0407.
          05 PO-DRAT0407-RC                    PIC  X(003).
             88 PO-DRAT0407-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0407-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0407-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0407-PGES-DATEN.
             10 PO-DRAT0407-KNR-NR             PIC S9(4) USAGE COMP.
             10 PO-DRAT0407-KNR-BEZ            PIC X(80).
             10 PO-DRAT0407-KNR-BEZ-KURZ       PIC X(4).
             10 PO-DRAT0407-KNR-HINDERNIS      PIC X(1).
             10 PO-DRAT0407-KNR-LAST-UPDATE    PIC X(26).
      *
      ******** ENDE COPY-MEMBER **************************************
