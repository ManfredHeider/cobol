      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1650.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1650.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:07:22   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:45:38   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jun 10 2005 09:49:44   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.0   Jun 07 2005 14:53:16   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1650                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN ZU VSTAT-KLASSE SORTIERT NACH      *
      *    PRODUKT UND VERTRAGSNUMMER>>                               *
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
      * APA01932!22.07.15! 2.1! ERWEITERUNG PO-BEREICH    ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1650                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1650.
          05 PI-DRPA1650-PROGRAMM      PIC  X(008).
          05 PI-DRPA1650-UFKT          PIC  9(002).
          05 PI-DRPA1650-DATEN.
             10 PI-DRPA1650-PDTA-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1650-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1650-PMPE-VSTAT-KLASSE
                                       PIC  X(001).
      *
       01 PO-DRPA1650.
          05 PO-DRPA1650-RC            PIC  X(003).
             88 PO-DRPA1650-RC-OK           VALUE 'OK '.
             88 PO-DRPA1650-RC-EOF          VALUE 'EOF'.
             88 PO-DRPA1650-RC-LEER         VALUE 'LER'.
          05 PO-DRPA1650-DATEN.
             10 PO-DRPA1650-PMPG-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1650-PMPG-VERSION
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1650-PMPE-VNR   PIC  X(015).
             10 PO-DRPA1650-PMPE-FAELLIG-VON
                                       PIC  X(010).
             10 PO-DRPA1650-PMPE-FAELLIG-BIS
                                       PIC  X(010).
             10 PO-DRPA1650-PMPE-VSTAT PIC  X(004).
             10 PO-DRPA1650-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1650-PMPP-PG-GSMT-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
             10 PO-DRPA1650-PMPP-PGV   PIC S9(004)       BINARY.
150713       10 PO-DRPA1650-PMPE-UNR        PIC S9(04) COMP.
150713       10 PO-DRPA1650-PMPE-SCHICHT-NR PIC S9(04) COMP.
220715       10 PO-DRPA1650-PMPP-PG-UST-BETRAG
220715                                 PIC S9(009)V9(02) PACKED-DECIMAL.

