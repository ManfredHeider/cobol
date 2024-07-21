      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  26.11.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0348.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0348.CPv  $
      *
      *   Rev 2.4   Nov 13 2019 18:30:50   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System 
      *
      *   Rev 2.3   Sep 12 2019 09:54:40   t13ae23
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.2   Aug 27 2019 11:20:44   t13ae23
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.1   Nov 27 2018 16:17:42   t13ae34
      *AAN01396: Antragserfassung Geiger Gold in einem System
      *
      *   Rev 2.0   Nov 26 2018 09:02:48   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0348                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST VORP SORTIERUNG VORP_LAUFENDE_NR >> *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01396!26.11.18!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0348                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRAN0348.
          05 PI-DRAN0348-PROGRAMM             PIC  X(008).
          05 PI-DRAN0348-UFKT-CODE            PIC  9(002).
          05 PI-DRAN0348-KEY.
             10 PI-DRAN0348-PGEB-GES          PIC S9(4) COMP.
             10 PI-DRAN0348-VORP-VORSCHLAG-NR PIC  X(0017).
      *
       01 PO-DRAN0348.
          05 PO-DRAN0348-RC                   PIC  X(003).
             88 PO-DRAN0348-RC-OK                  VALUE 'OK '.
             88 PO-DRAN0348-RC-EOF                 VALUE 'EOF'.
             88 PO-DRAN0348-RC-LEER                VALUE 'LER'.
          05 PO-DRAN0348-DATEN.
             10 PO-DRAN0348-VORP-LFD-NR       PIC S9(4) COMP.
             10 PO-DRAN0348-KNR-WAEHRUNG      PIC S9(4) COMP.
             10 PO-DRAN0348-PROD-PROD-SCHL    PIC S9(4) COMP.
             10 PO-DRAN0348-VORP-BETR         PIC S9(9)V9(2) COMP-3.
             10 PO-DRAN0348-VORP-EINH-AVERM   PIC S9(6)V9(2) COMP-3.
             10 PO-DRAN0348-VORP-KENNUNG-INS  PIC  X(0008).
             10 PO-DRAN0348-VORP-LAUFZEIT     PIC S9(4) COMP.
             10 PO-DRAN0348-VORP-SUMME        PIC S9(9)V9(2) COMP-3.
             10 PO-DRAN0348-VORP-TARIF        PIC  X(0015).
             10 PO-DRAN0348-VORP-ZAHLWEISE    PIC S9(4) COMP.
             10 PO-DRAN0348-VORP-ZEIT-INSERT  PIC  X(0026).
             10 PO-DRAN0348-VORP-ZUSATZ-A-1   PIC  X(0050).
             10 PO-DRAN0348-VORP-ZUSATZ-A-2   PIC  X(0050).
             10 PO-DRAN0348-VORP-ZUSATZ-N-1   PIC S9(9)V9(3) COMP-3.
             10 PO-DRAN0348-VORP-ZUSATZ-N-2   PIC S9(9)V9(3) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

