      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0132.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0132.CPv  $
      *
      *   Rev 2.3   Dec 14 2011 16:58:36   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.2   04 Dec 2006 12:50:52   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 06 2006 10:56:00   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 13 2006 12:37:42   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0132 WORKFLOW ANTRAG ALT                 *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!13.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!28.11.06!2.2 ! TIMESTAMP                 ! HEIDER   *
      * AAN01033!        !2.3 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY IST EIN WORKFLOW DER ANTRAG-DATEN             *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *!!!ACHTUNG!!! -> BEI ÄNDERUNG AUCH DRAN0145 ANPASSEN!!!       *
      ****************************************************************
       01  WF-ANTRAG-ALT.
           05 WF-ANTRAG-DATEN-ALT.
             10 WF-ANTRAG-KOPF-ALT.
                COPY DRAN0141.
      *
             10 WF-ANTRAG-PE-STRUKTUR-ALT.
                COPY DRAN0142.
      *
             10 WF-ANTRAG-PRODUKTE-ALT.
                15 WF-ANTRAG-PRODUKT-TABELLE-ALT OCCURS 15.
                COPY DRAN0143.

      *         TIMESTAMP WORKFLOW
             10 WF-ANTA-LAST-UPDATE-ALT    PIC X(26).
      *
      *         ZU VERARBEITENDE DATEN AUS ELEKTRONISCHEN VORSCHLÄGEN
             10 WF-ANTRAG-VORSCHLAG-ALT    PIC X(1000).
      *         -> ZUSATZDATEN JE NACH PRODUKTGEBER
      *
      ******** Ende Copy-Member **************************************
