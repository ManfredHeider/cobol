      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.07.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1858.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1858.CPv  $
      *
      *   Rev 2.1   Sep 27 2016 09:33:04   t13ae46
      *APA02280: Dialoganpassungen PG14
      *
      *   Rev 2.0   Jul 22 2016 11:19:44   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA01858                                     *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE UND PMPP MIT PRODUKTFILTER  *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02280!22.07.16!2.0 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA01858                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1858.
          05 PI-DRPA1858-PROGRAMM              PIC  X(008).
          05 PI-DRPA1858-UFKT                  PIC  9(002).
          05 PI-DRPA1858-SCHALTER              PIC  9.
          05 PI-DRPA1858-DATEN.
             10 PI-DRPA1858-PDTA-ID            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1858-PGEB-GES           PIC S9(04) COMP.
             10 PI-DRPA1858-PROD-PROD-SCHL     PIC S9(04) COMP.
             10 PI-DRPA1858-PMPE-VSTAT         PIC X(001).
      *
       01 PO-DRPA1858.
          05 PO-DRPA1858-RC                    PIC  X(003).
             88 PO-DRPA1858-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1858-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1858-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1858-DATEN.
             10 PO-DRPA1858-PMPG-ID            PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1858-PMPG-VERSION       PIC S9(04) COMP.
             10 PO-DRPA1858-PMPE-VNR           PIC X(15).
             10 PO-DRPA1858-PMPE-FAELLIG-VON   PIC X(10).
             10 PO-DRPA1858-PMPE-FAELLIG-BIS   PIC X(10).
             10 PO-DRPA1858-PMPE-VSTAT         PIC X(04).
             10 PO-DRPA1858-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PO-DRPA1858-PMPP-PG-GSMT-PROV  PIC S9(9)V9(2)
                                                   USAGE COMP-3.
             10 PO-DRPA1858-PMPP-PGV           PIC S9(04) COMP.
             10 PO-DRPA1858-PMPE-UNR           PIC S9(04) COMP.
             10 PO-DRPA1858-PMPE-SCHICHT-NR    PIC S9(04) COMP.
             10 PO-DRPA1858-PMPP-PG-UST-BETRAG PIC S9(9)V9(2)
                                                  USAGE COMP-3.

