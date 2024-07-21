      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0143.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0143.CPv  $
      *
      *   Rev 2.3   15 Nov 2006 09:30:42   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   14 Nov 2006 13:43:32   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 11:19:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 21 2006 09:51:58   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0143 WORKFLOW ANTRAG - PRODUKTDATEN      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!21.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!14.11.06!2.2 ! NEUE FELDER               ! HEIDER   *
      * AAN00655!15.11.06!2.3 ! NEUE FELDER               ! HEIDER   *
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
                 40 ANTP-ANTRAG-PRODUKT-DATEN.
                    45 PGEB-GES                 PIC S9(4) COMP.
                    45 ANTP-NR                  PIC S9(15)V COMP-3.
                    45 PROD-PROD-SCHL           PIC S9(4) COMP.
                    45 ANTP-LFD-NR              PIC S9(4) COMP.
                    45 ANTP-TARIF               PIC X(15).
                    45 ANTP-BETR                PIC S9(9)V9(2) COMP-3.
                    45 ANTP-SUMME               PIC S9(9)V9(2) COMP-3.
                    45 KNR-WAEHRUNG             PIC S9(4) COMP.
                    45 ANTP-ZAHLWEISE           PIC S9(4) COMP.
                    45 ANTP-LAUFZEIT            PIC S9(4) COMP.
                    45 ANTP-DYNAMIK             PIC S9(3)V9(3) COMP-3.
                    45 ANTP-ZUSATZ-1            PIC S9(9)V9(3) COMP-3.
                    45 ANTP-ZUSATZ-2            PIC S9(9)V9(3) COMP-3.
                    45 ANTP-EINH-AVERM          PIC S9(6)V9(2) COMP-3.
