      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.11.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1681.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1681.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:06:02   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:46:02   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jan 07 2011 11:29:52   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.0   Nov 10 2008 09:43:44   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1681                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1681                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01129!07.01.11! 2.1! NEU                       ! RAHNER   *
      * APA01526!15.07.13! 2.2! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01932!22.07.15! 2.3! ERWEITERUNG PO-BEREICH    ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1681
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1681 DES BUFFER-OZP ZYPA0005 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PG14, AUSWAHLLISTE NPM
      *
      ****************************************************************
      *
       01  PI-DRPA1681.
           05  PI-DRPA1681-UFKT                PIC  9(02).
           05  PI-DRPA1681-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1681-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1681-PDTA-ID             PIC S9(15)V COMP-3.
           05  PI-DRPA1681-PGEB-GES            PIC S9(04) COMP.
           05  PI-DRPA1681-PROD-PROD-SCHL      PIC S9(04) COMP.
           05  PI-DRPA1681-PMPE-VSTAT-KLASSE   PIC  X(01).
           05  PI-DRPA1681-SORTIERUNG          PIC  X(01).
      *
       01  PO-DRPA1681.
           05  PO-DRPA1681-RC                  PIC  X(03).
               88 PO-DRPA1681-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1681-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1681-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1681-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA1681-ERGEBNISZEILE.
               10 PO-DRPA1681-PMPG-ID          PIC S9(15)V COMP-3.
               10 PO-DRPA1681-PMPG-VERSION     PIC S9(04) COMP.
               10 PO-DRPA1681-VNR              PIC  X(15).
               10 PO-DRPA1681-VSTAT            PIC  X(04).
               10 PO-DRPA1681-PROD-PROD-SCHL-1 PIC S9(04) BINARY.
               10 PO-DRPA1681-PROD-PROD-SCHL-2 PIC S9(04) BINARY.
               10 PO-DRPA1681-PROD-PROD-SCHL-3 PIC S9(04) BINARY.
               10 PO-DRPA1681-PG-GSMT-PROV     PIC S9(9)V9(2) COMP-3.
               10 PO-DRPA1681-PGV              PIC S9(4) COMP.
               10 PO-DRPA1681-FAELLIG-VON      PIC  X(10).
               10 PO-DRPA1681-FAELLIG-BIS      PIC  X(10).
               10 PO-DRPA1681-PRODSL-KZ        PIC  X.
150713         10 PO-DRPA1681-PMPE-UNR         PIC S9(04) COMP.
150713         10 PO-DRPA1681-PMPE-SCHICHT-NR  PIC S9(04) COMP.
220715         10 PO-DRPA1681-PG-UST-BETRAG    PIC S9(9)V9(2) COMP-3.
      *

