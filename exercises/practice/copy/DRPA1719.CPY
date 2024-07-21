      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1719.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1719.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:08:12   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:46:10   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jan 07 2011 11:30:00   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.0   Aug 02 2010 11:45:24   i13az32
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRPA01719                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PMPE UND PMPP MIT PRODUKTFILTER  *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01129!07.01.11!2.1 ! NEU                       ! RAHNER   *
      * APA01526!15.07.13!2.2 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!22.07.15!2.3 ! ERWEITERUNG PO-BEREICH    ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA01719                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1719.
          05 PI-DRPA1719-PROGRAMM              PIC  X(008).
          05 PI-DRPA1719-UFKT                  PIC  9(002).
          05 PI-DRPA1719-SCHALTER              PIC  9.
          05 PI-DRPA1719-DATEN.
             10 PI-DRPA1719-PDTA-ID            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1719-PGEB-GES           PIC S9(04) COMP.
             10 PI-DRPA1719-PROD-PROD-SCHL     PIC S9(04) COMP.
             10 PI-DRPA1719-PMPP-PGV           PIC S9(04) COMP.
      *
       01 PO-DRPA1719.
          05 PO-DRPA1719-RC                    PIC  X(003).
             88 PO-DRPA1719-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1719-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1719-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1719-DATEN.
             10 PO-DRPA1719-PMPG-ID            PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1719-PMPG-VERSION       PIC S9(04) COMP.
             10 PO-DRPA1719-PMPE-VNR           PIC X(15).
             10 PO-DRPA1719-PMPE-FAELLIG-VON   PIC X(10).
             10 PO-DRPA1719-PMPE-FAELLIG-BIS   PIC X(10).
             10 PO-DRPA1719-PMPE-VSTAT         PIC X(04).
             10 PO-DRPA1719-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PO-DRPA1719-PMPP-PG-GSMT-PROV  PIC S9(9)V9(2)
                                                   USAGE COMP-3.
             10 PO-DRPA1719-PMPP-PGV           PIC S9(04) COMP.
150713       10 PO-DRPA1719-PMPE-UNR           PIC S9(04) COMP.
150713       10 PO-DRPA1719-PMPE-SCHICHT-NR    PIC S9(04) COMP.
220715       10 PO-DRPA1719-PMPP-PG-UST-BETRAG PIC S9(9)V9(2)
220715                                             USAGE COMP-3.
      *

