      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1651.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1651.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:07:36   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:45:46   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jun 10 2005 09:49:52   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.0   Jun 07 2005 14:53:28   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1651                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN ZU VSTAT-KLASSE SORTIERT NACH      *
      *    FAELLIGKEIT UND VERTRAGSNUMMER>>                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M.SCHEIDEL                                       *
      *    DATUM:    02.06.2005                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00353!02.06.05! 2.0! NEU                       ! SCHEIDEL *
      * APA01526!15.07.13! 2.1! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!22.07.15! 2.2! ERWEITERUNG PO-BEREICH    ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1651                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1651.
          05 PI-DRPA1651-PROGRAMM      PIC  X(008).
          05 PI-DRPA1651-UFKT          PIC  9(002).
          05 PI-DRPA1651-DATEN.
             10 PI-DRPA1651-PDTA-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1651-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1651-PMPE-VSTAT-KLASSE
                                       PIC  X(001).
      *
       01 PO-DRPA1651.
          05 PO-DRPA1651-RC            PIC  X(003).
             88 PO-DRPA1651-RC-OK           VALUE 'OK '.
             88 PO-DRPA1651-RC-EOF          VALUE 'EOF'.
             88 PO-DRPA1651-RC-LEER         VALUE 'LER'.
          05 PO-DRPA1651-DATEN.
             10 PO-DRPA1651-PMPG-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1651-PMPG-VERSION
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1651-PMPE-VNR   PIC  X(015).
             10 PO-DRPA1651-PMPE-FAELLIG-VON
                                       PIC  X(010).
             10 PO-DRPA1651-PMPE-FAELLIG-BIS
                                       PIC  X(010).
             10 PO-DRPA1651-PMPE-VSTAT PIC  X(004).
             10 PO-DRPA1651-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1651-PMPP-PG-GSMT-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
             10 PO-DRPA1651-PMPP-PGV   PIC S9(004)       BINARY.
150713       10 PO-DRPA1651-PMPE-UNR        PIC S9(04) COMP.
150713       10 PO-DRPA1651-PMPE-SCHICHT-NR PIC S9(04) COMP.
220715       10 PO-DRPA1651-PMPP-PG-UST-BETRAG
220715                                 PIC S9(009)V9(02) PACKED-DECIMAL.

