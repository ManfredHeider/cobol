      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1032.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1032.CPv  $
      *
      *   Rev 2.1   Dec 12 2011 14:26:14   t13ex13
      *ASL00399: MAP für FVD
      *
      *   Rev 2.0   Nov 10 2011 17:21:06   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1032                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0028                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: RALF GUTFLEISCH                                  *
      *    DATUM:    11.11.2011                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00399!11.11.11!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1032 - ERMITTELN OFFENE ANSPRÜCHE AUS MAPFVD  *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1032.
           05  PI-DRVB1032-DATEN.
               10  PI-DRVB1032-UFKT-CODE          PIC  9(002).
               10  PI-DRVB1032-KNR-GESELL-TYP     PIC S9(004) COMP.
               10  PI-DRVB1032-MAPFVD-STATUS      PIC S9(004) COMP.
               10  PI-DRVB1032-TURNUS             PIC S9(009) COMP.
      *
       01  PO-DRVB1032.
           05  PO-DRVB1032-RC                     PIC  X(003).
               88  PO-DRVB1032-RC-OK                  VALUE 'OK '.
               88  PO-DRVB1032-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB1032-DATEN.
               10 PO-DRVB1032-MA-ST-NR       PIC S9(9) USAGE COMP.
               10 PO-DRVB1032-MAPFVD-REF-VB  PIC S9(9) USAGE COMP.
               10 PO-DRVB1032-MAPFVD-TURNUS      PIC S9(9) USAGE COMP.
               10 PO-DRVB1032-MAPFVD-LFDNR       PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-MAPFVD-REF-KZ      PIC X(1).
               10 PO-DRVB1032-MAPFVD-BETRAG PIC S9(9)V9(2) USAGE COMP-3.
               10 PO-DRVB1032-KTY-WAEHRUNG       PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-KNR-WAEHRUNG       PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-KNR-ANSPR-GRUND    PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-KTY-ANSPR-GRUND    PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-MAPFVD-EH     PIC S9(9)V9(1) USAGE COMP-3.
               10 PO-DRVB1032-KTY-ANSPR-STATUS   PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-KNR-ANSPR-STATUS   PIC S9(4) USAGE COMP.
               10 PO-DRVB1032-MAPFVD-LAST-UPD    PIC X(26).
      ****
      **** ENDE COPY DRVB1032 ****************************************
      ****
