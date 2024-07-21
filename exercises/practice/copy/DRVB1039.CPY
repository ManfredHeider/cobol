      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1039.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1039.CPv  $
      *
      *   Rev 2.1   Feb 01 2012 15:45:24   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.0   Jan 23 2012 14:44:02   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1039                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR PMSL0094                                 *
      *    Ermittlung aller VB mit einer bestimmten Beschränkung      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.Ober                                           *
      *    DATUM:    23.01.2012                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00420!23.01.12! 2.1! Neu, alle direkten und    ! K.Ober   *
      *         !        !    ! indirekten Partner        !          *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1039                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1039.
          05 PI-DRVB1039-PROGRAMM           PIC  X(008).
          05 PI-DRVB1039-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1039-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB1039-BES-NR             PIC S9(003) BINARY.
      *
       01 PO-DRVB1039.

          05 PO-DRVB1039-RC                 PIC  X(003).
             88 PO-DRVB1039-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1039-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1039-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1039-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1039-DATEN.
             10 PO-DRVB1039-UNTST-NR        PIC S9(009) BINARY.
             10 PO-DRVB1039-EBENE           PIC S9(002) BINARY.

      *
      ***** ENDE COPY-MEMBER DRVB1039 *****************************
