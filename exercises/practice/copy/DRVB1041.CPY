      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1041.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1041.CPv  $
      *
      *   Rev 2.1   Feb 01 2012 15:45:46   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.0   Jan 24 2012 11:03:32   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1041                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR PMSL0094                                 *
      *    Ermittlung aller VB mit einer bestimmten Beschränkung      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.Ober                                           *
      *    DATUM:    24.01.2012                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00420!24.01.12! 2.1! Neu, Anzahl direkte und   ! K.Ober   *
      *         !        !    ! indirekte Partner         !          *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1041                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1041.
          05 PI-DRVB1041-PROGRAMM           PIC  X(008).
          05 PI-DRVB1041-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1041-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB1041-BES-NR             PIC S9(003) BINARY.
      *
       01 PO-DRVB1041.

          05 PO-DRVB1041-RC                 PIC  X(003).
             88 PO-DRVB1041-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1041-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1041-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1041-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1041-DATEN.
             10 PO-DRVB1041-ANZAHL-PARTNER  PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB1041 *****************************
