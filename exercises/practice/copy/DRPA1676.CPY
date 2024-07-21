      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1676.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1676.CPv  $
      *
      *   Rev 2.3   Jun 19 2012 09:31:50   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:56:46   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Apr 21 2008 13:24:50   t13ex21
      *FPA01015: Advocard: Korrektur Buchungen
      *
      *   Rev 2.0   Apr 09 2008 11:10:58   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1676                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF BEREITSTELLEN UMBUCHUNGEN FÜR KORREKTUR    *
      *                    ADVOCARD                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                        *
      *    DATUM:    09.04.2008                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA01015!09.04.08!    ! NEU                       ! STORCH   *
      * APA01101!27.02.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1676                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1676.
          05 PI-DRPA1676-PROGRAMM             PIC  X(08).
          05 PI-DRPA1676-UFKT                 PIC  9(02).
          05 PI-DRPA1676-AUSWAHLKRITERIEN.
             10 PI-DRPA1676-PGEB-GES          PIC S9(04) USAGE COMP.
      *
       01 PO-DRPA1676.
          05 PO-DRPA1676-RC                   PIC X(03).
             88 PO-DRPA1676-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1676-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA1676-RC-LEER               VALUE 'LER'.
          05 PO-DRPA1676-ANZAHL-ERG-ZEILEN    PIC 9(05).

          05 PO-DRPA1676-ERGEBNISZEILE.
             10 PO-DRPA1676-PMPE-VNR          PIC X(15).
             10 PO-DRPA1676-PMPE-PG-VBNR      PIC S9(09) USAGE COMP.
             10 PO-DRPA1676-PDPT-ID           PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1676-PMBE-ID           PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1676-PEST-AVERM        PIC S9(09) USAGE COMP.
             10 PO-DRPA1676-PVPT-KDBETR-REDUZ PIC X(1).
             10 PO-DRPA1676-KNR-MANDANT       PIC S9(04) USAGE COMP.
             10 PO-DRPA1676-PROD-PG-DATEN     PIC X(30).
      *
      ******* ENDE COPY-MEMBER **************************************
