      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1849.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1849.CPv  $
      *
      *   Rev 2.1   Sep 27 2016 09:32:12   t13ae46
      *APA02280: Dialoganpassungen PG14
      *
      *   Rev 2.0   Jul 05 2016 12:04:12   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1849                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN ZU VSTAT-KLASSE SORTIERT NACH      *
      *    GESAMTBETRAG UND VERTRAGSNUMMER>>                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.WANG                                           *
      *    DATUM:    04.07.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02280!04.07.16! 2.0! NEU                       ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1849                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1849.
          05 PI-DRPA1849-PROGRAMM      PIC  X(008).
          05 PI-DRPA1849-UFKT          PIC  9(002).
          05 PI-DRPA1849-DATEN.
             10 PI-DRPA1849-PDTA-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1849-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1849-PMPE-VSTAT-KLASSE
                                       PIC  X(001).
      *
       01 PO-DRPA1849.
          05 PO-DRPA1849-RC            PIC  X(003).
             88 PO-DRPA1849-RC-OK           VALUE 'OK '.
             88 PO-DRPA1849-RC-EOF          VALUE 'EOF'.
             88 PO-DRPA1849-RC-LEER         VALUE 'LER'.
          05 PO-DRPA1849-DATEN.
             10 PO-DRPA1849-PMPG-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1849-PMPG-VERSION
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1849-PMPE-VNR   PIC  X(015).
             10 PO-DRPA1849-PMPE-FAELLIG-VON
                                       PIC  X(010).
             10 PO-DRPA1849-PMPE-FAELLIG-BIS
                                       PIC  X(010).
             10 PO-DRPA1849-PMPE-VSTAT PIC  X(004).
             10 PO-DRPA1849-PROD-PROD-SCHL
                                       PIC S9(004)       BINARY.
             10 PO-DRPA1849-PMPP-PG-GSMT-PROV
                                       PIC S9(009)V9(02) PACKED-DECIMAL.
             10 PO-DRPA1849-PMPP-PGV   PIC S9(004)       BINARY.
             10 PO-DRPA1849-PMPE-UNR        PIC S9(04) COMP.
             10 PO-DRPA1849-PMPE-SCHICHT-NR PIC S9(04) COMP.
             10 PO-DRPA1849-PMPP-PG-UST-BETRAG
                                       PIC S9(009)V9(02) PACKED-DECIMAL.

