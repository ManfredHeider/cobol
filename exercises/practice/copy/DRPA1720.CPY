      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.12.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1720.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1720.CPv  $
      *
      *   Rev 2.3   Jul 30 2015 09:06:12   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.2   Dec 17 2013 10:46:18   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PG14
      *
      *   Rev 2.1   Jan 07 2011 11:30:06   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.0   Dec 22 2010 09:07:46   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1720                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1720                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01129!07.01.11!2.1 ! NEU                       ! RAHNER   *
      * APA01526!15.07.13!2.2 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01526!22.07.15!2.3 ! ERWEITERUNG PO-BEREICH    ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1720
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1720 DES BUFFER-OZP ZYPA0005 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PG14, AUSWAHLLISTE NPM +
      *    FILTER MIT VORGEGBENEM PRODUKT
      *
      ****************************************************************
      *
       01  PI-DRPA1720.
           05  PI-DRPA1720-UFKT                PIC  9(02).
           05  PI-DRPA1720-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1720-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1720-PDTA-ID             PIC S9(15)V  COMP-3.
           05  PI-DRPA1720-PGEB-GES            PIC S9(04) COMP.
           05  PI-DRPA1720-PMPP-PGV            PIC S9(04) COMP.
           05  PI-DRPA1720-SORTIERUNG          PIC  X(01).
           05  PI-DRPA1720-PRODSCHL            PIC S9(04) COMP.
      *
       01  PO-DRPA1720.
           05  PO-DRPA1720-RC                  PIC  X(03).
               88 PO-DRPA1720-RC-OK                 VALUE 'OK '.
               88 PO-DRPA1720-RC-EOF                VALUE 'EOF'.
               88 PO-DRPA1720-RC-LEER               VALUE 'LER'.
           05  PO-DRPA1720-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA1720-ERGEBNISZEILE.
               10 PO-DRPA1720-PMPG-ID          PIC S9(15)V  COMP-3.
               10 PO-DRPA1720-PMPG-VERSION     PIC S9(04) COMP.
               10 PO-DRPA1720-VNR              PIC  X(15).
               10 PO-DRPA1720-VSTAT            PIC  X(04).
               10 PO-DRPA1720-PROD-PROD-SCHL-1 PIC S9(04) BINARY.
               10 PO-DRPA1720-PROD-PROD-SCHL-2 PIC S9(04) BINARY.
               10 PO-DRPA1720-PROD-PROD-SCHL-3 PIC S9(04) BINARY.
               10 PO-DRPA1720-PG-GSMT-PROV     PIC S9(9)V9(2) COMP-3.
               10 PO-DRPA1720-PGV              PIC S9(04) COMP.
               10 PO-DRPA1720-FAELLIG-VON      PIC  X(10).
               10 PO-DRPA1720-FAELLIG-BIS      PIC  X(10).
               10 PO-DRPA1720-PRODSL-KZ        PIC  X.
150713         10 PO-DRPA1720-PMPE-UNR         PIC S9(04) COMP.
150713         10 PO-DRPA1720-PMPE-SCHICHT-NR  PIC S9(04) COMP.
220715         10 PO-DRPA1720-PG-UST-BETRAG    PIC S9(9)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

