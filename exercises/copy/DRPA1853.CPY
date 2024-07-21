      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1853.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1853.CPv  $
      *
      *   Rev 2.1   Sep 27 2016 09:32:32   t13ae46
      *APA02280: Dialoganpassungen PG14
      *
      *   Rev 2.0   Jul 21 2016 11:06:40   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA01853                                     *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE UND PMPP MIT PRODUKTFILTER  *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02280!20.07.16!2.0 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA01853                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1853.
          05 PI-DRPA1853-PROGRAMM              PIC  X(008).
          05 PI-DRPA1853-UFKT                  PIC  9(002).
          05 PI-DRPA1853-SCHALTER              PIC  9.
          05 PI-DRPA1853-DATEN.
             10 PI-DRPA1853-PDTA-ID            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1853-PGEB-GES           PIC S9(04) COMP.
             10 PI-DRPA1853-PROD-PROD-SCHL     PIC S9(04) COMP.
             10 PI-DRPA1853-PMPE-VSTAT-KLASSE  PIC X(001).
      *
       01 PO-DRPA1853.
          05 PO-DRPA1853-RC                    PIC  X(003).
             88 PO-DRPA1853-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1853-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1853-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1853-DATEN.
             10 PO-DRPA1853-PMPG-ID            PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1853-PMPG-VERSION       PIC S9(04) COMP.
             10 PO-DRPA1853-PMPE-VNR           PIC X(15).
             10 PO-DRPA1853-PMPE-FAELLIG-VON   PIC X(10).
             10 PO-DRPA1853-PMPE-FAELLIG-BIS   PIC X(10).
             10 PO-DRPA1853-PMPE-VSTAT         PIC X(04).
             10 PO-DRPA1853-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PO-DRPA1853-PMPP-PG-GSMT-PROV  PIC S9(9)V9(2)
                                                   USAGE COMP-3.
             10 PO-DRPA1853-PMPP-PGV           PIC S9(04) COMP.
             10 PO-DRPA1853-PMPE-UNR           PIC S9(04) COMP.
             10 PO-DRPA1853-PMPE-SCHICHT-NR    PIC S9(04) COMP.
             10 PO-DRPA1853-PMPP-PG-UST-BETRAG PIC S9(9)V9(2)
                                                  USAGE COMP-3.

