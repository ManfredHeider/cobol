      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1662.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1662.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:07:52   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:45:54   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jan 07 2011 11:29:46   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.0   13 Apr 2007 08:57:16   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA01662                                      *
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
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA01662                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1662.
          05 PI-DRPA1662-PROGRAMM              PIC  X(008).
          05 PI-DRPA1662-UFKT                  PIC  9(002).
          05 PI-DRPA1662-SCHALTER              PIC  9.
          05 PI-DRPA1662-DATEN.
             10 PI-DRPA1662-PDTA-ID            PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1662-PGEB-GES           PIC S9(04) COMP.
             10 PI-DRPA1662-PROD-PROD-SCHL     PIC S9(04) COMP.
             10 PI-DRPA1662-PMPE-VSTAT-KLASSE  PIC X(001).
      *
       01 PO-DRPA1662.
          05 PO-DRPA1662-RC                    PIC  X(003).
             88 PO-DRPA1662-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1662-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1662-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1662-DATEN.
             10 PO-DRPA1662-PMPG-ID            PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1662-PMPG-VERSION       PIC S9(04) COMP.
             10 PO-DRPA1662-PMPE-VNR           PIC X(15).
             10 PO-DRPA1662-PMPE-FAELLIG-VON   PIC X(10).
             10 PO-DRPA1662-PMPE-FAELLIG-BIS   PIC X(10).
             10 PO-DRPA1662-PMPE-VSTAT         PIC X(04).
             10 PO-DRPA1662-PROD-PROD-SCHL     PIC S9(4) USAGE COMP.
             10 PO-DRPA1662-PMPP-PG-GSMT-PROV  PIC S9(9)V9(2)
                                                   USAGE COMP-3.
             10 PO-DRPA1662-PMPP-PGV           PIC S9(04) COMP.
150713       10 PO-DRPA1662-PMPE-UNR           PIC S9(04) COMP.
150713       10 PO-DRPA1662-PMPE-SCHICHT-NR    PIC S9(04) COMP.
220715       10 PO-DRPA1662-PMPP-PG-UST-BETRAG PIC S9(9)V9(2)
220715                                            USAGE COMP-3.


