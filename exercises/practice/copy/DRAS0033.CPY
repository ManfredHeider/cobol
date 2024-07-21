      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.10.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0033.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0033.CPv  $
      *
      *   Rev 2.1   Oct 29 2010 16:07:06   t13ae36
      *AAS00927: Neue 300 Einheiten Regel für alle
      *
      *   Rev 2.0   Oct 13 2010 12:07:48   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0033                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0033                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    13.10.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00927!13.10.10!2.1 ! NEU                       ! FRANKE    *
      *         !28.10.10!    ! REV                       ! FRANKE    *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAS0033
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRAS0033.
           05  PI-DRAS0033-HNP-VBNR        PIC  9(07)     VALUE  ZERO.
           05  PI-DRAS0033-HNP-DAT-BIS     PIC  X(10)     VALUE SPACE.
           05  PI-DRAS0033-HNP-EH-NE       PIC S9(09)V9   VALUE  ZERO.
           05  PI-DRAS0033-HNP-EH-NG       PIC S9(09)V9   VALUE  ZERO.
      *
       01  PO-DRAS0033.
           05  PO-DRAS0033-RC                 PIC  X(003).
               88 PO-DRAS0033-RC-OK                VALUE 'OK '.
               88 PO-DRAS0033-RC-NOK               VALUE 'NOK'.
               88 PO-DRAS0033-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRAS0033-RC-ERR               VALUE 'ERR'.
      *
           05  PO-DRAS0033-MA-ST-NR           PIC S9(09)       BINARY.
           05  PO-DRAS0033-HNP-MONAT          PIC  X(10).
           05  PO-DRAS0033-KTY-HSTATUS        PIC S9(04)       BINARY.
           05  PO-DRAS0033-KNR-HSTATUS        PIC S9(04)       BINARY.
           05  PO-DRAS0033-HNP-LAST-UPDATE    PIC  X(26).
           05  PO-DRAS0033-HNP-HUERDE-BASIS   PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EEG            PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-HUERDE-ABS     PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-HUERDE-REST    PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-EIGEN-KUM   PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-EIGEN       PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-EIGEN-H     PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-EIGEN-W     PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-GRUPPE      PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-GESAMT      PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-HNP-EH-GESAMT-W    PIC S9(09)V9(01) COMP-3.
           05  PO-DRAS0033-KTY-SATZ-ART       PIC S9(04)       BINARY.
           05  PO-DRAS0033-KNR-SATZ-ART       PIC S9(04)       BINARY.
           05  PO-DRAS0033-HNP-BEMERKUNG      PIC  X(20).
           05  PO-DRAS0033-AMPEL              PIC  X(05).
               88 PO-DRAS0033-AMPEL-ROT            VALUE 'ROT'.
               88 PO-DRAS0033-AMPEL-GELB           VALUE 'GELB'.
               88 PO-DRAS0033-AMPEL-GRUEN          VALUE 'GRUEN'.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRAS0033   --------------------------*
      *---------------------------------------------------------------*

