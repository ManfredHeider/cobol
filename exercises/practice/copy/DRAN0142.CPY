      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0142.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0142.CPv  $
      *
      *   Rev 2.1   Sep 26 2006 11:15:02   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 21 2006 09:46:10   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0142 WORKFLOW ANTRAG - PE-STRUKTURDATEN  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!21.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
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
      ****************************************************************
             30 ANTA-ANTRAG-PE-STRUKTUR-DATEN.
                35 ANTRAG-STRUKTUR                     OCCURS 20
                                                    INDEXED BY IX-PE-S.
                   40 MA-ST-NR                      PIC S9(009) BINARY.
                   40 PRZ                           PIC  X(001).
                   40 EMP-STUFE                     PIC  X(004).
                   40 DIFF-PROV-STUFE               PIC S9(004) BINARY.
                   40 KNR-EMP-TYP                   PIC S9(004) BINARY.
      *
