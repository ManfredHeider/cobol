      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1721.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1721.CPv  $
      *
      *   Rev 2.3   Nov 01 2016 15:49:10   t13ae46
      *FPA01543: Filterung nach Produktschlüssel im PG14 nicht mögl
      *ich 
      *
      *   Rev 2.2   Jul 30 2015 09:06:20   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11)
      *
      *   Rev 2.1   Jan 07 2011 11:30:12   t13ae23
      *APA01129: Selektion PG14 erweitern
      *
      *   Rev 2.0   Jan 05 2011 14:20:28   t13ae23
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
      * APA01526!22.07.15! 2.2! ERWEITERUNG PO-BEREICH    ! WANG     *
      * FPA01543!01.11.16! 2.3! ERW. UNR, SCHICHT-NR      ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1721
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1721 DES BUFFER-OZP ZYPA0005 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PG14, AUSWAHLLISTE NPM
      *
      ****************************************************************
      *
       01  PI-DRPA1721.
           05  PI-DRPA1721-UFKT                PIC  9(02).
           05  PI-DRPA1721-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1721-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1721-PDTA-ID             PIC S9(15)V USAGE COMP-3.
           05  PI-DRPA1721-PGEB-GES            PIC S9(04) COMP.
           05  PI-DRPA1721-PROD-PROD-SCHL      PIC S9(04) COMP.
           05  PI-DRPA1721-PMPE-VSTAT          PIC  X(04).
           05  PI-DRPA1721-SORTIERUNG          PIC  X(01).
      *
       01  PO-DRPA1721.
           05  PO-DRPA1721-RC                  PIC  X(03).
               88 PO-DRPA1721-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1721-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1721-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1721-ANZAHL-ERG-ZEILEN   PIC  9(05).
           05  PO-DRPA1721-ERGEBNISZEILE.
               10 PO-DRPA1721-PMPG-ID          PIC S9(15)V USAGE COMP-3.
               10 PO-DRPA1721-PMPG-VERSION     PIC S9(04) COMP.
               10 PO-DRPA1721-VNR              PIC  X(15).
               10 PO-DRPA1721-VSTAT            PIC  X(04).
               10 PO-DRPA1721-PROD-PROD-SCHL-1 PIC S9(04) BINARY.
               10 PO-DRPA1721-PROD-PROD-SCHL-2 PIC S9(04) BINARY.
               10 PO-DRPA1721-PROD-PROD-SCHL-3 PIC S9(04) BINARY.
               10 PO-DRPA1721-PG-GSMT-PROV     PIC S9(9)V9(2) COMP-3.
               10 PO-DRPA1721-PGV              PIC S9(4) COMP.
               10 PO-DRPA1721-FAELLIG-VON      PIC  X(10).
               10 PO-DRPA1721-FAELLIG-BIS      PIC  X(10).
               10 PO-DRPA1721-PRODSL-KZ        PIC  X.
011116         10 PO-DRPA1721-PMPE-UNR         PIC S9(04) COMP.
011116         10 PO-DRPA1721-PMPE-SCHICHT-NR  PIC S9(04) COMP.
220715         10 PO-DRPA1721-PG-UST-BETRAG    PIC S9(9)V9(2) COMP-3.
      *


