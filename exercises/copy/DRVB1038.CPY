      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1038.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1038.CPv  $
      *
      *   Rev 2.1   Feb 01 2012 15:45:00   t13ae14
      *ASL00420: Partnerbonus - DLGEN, Lese OZP, Schreibmodule
      *
      *   Rev 2.0   Jan 18 2012 12:47:56   t13ae14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1038                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR PMSL0094                                 *
      *    Ermittlung aller VB mit einer bestimmten Beschränkung      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: K.Ober                                           *
      *    DATUM:    18.01.2012                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00420!18.01.12! 2.1! Neu, alle VB mit einer    ! K.Ober   *
      *         !        !    ! bestimmten Beschränkung   !          *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1038                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1038.
          05 PI-DRVB1038-PROGRAMM           PIC  X(008).
          05 PI-DRVB1038-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1038-MS-NR-VON          PIC S9(004) BINARY.
          05 PI-DRVB1038-BES-NR             PIC S9(003) BINARY.
          05 PI-DRVB1038-ZEITPUNKT-ISO      PIC  X(010).
      *
       01 PO-DRVB1038.

          05 PO-DRVB1038-RC                 PIC  X(003).
             88 PO-DRVB1038-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1038-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1038-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1038-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1038-DATEN.
             10 PO-DRVB1038-MA-ST-NR        PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB1038 *****************************
