      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1081.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1081.CPv  $
      *
      *   Rev 2.1   Jun 03 2015 12:40:50   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.0   Mar 03 2015 10:25:16   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1039                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR ZJVB0037                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    04.03.2015                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB01332!04.03.15! 2.1! Neu                       ! S. Walb  *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1081                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB1081.
          05 PI-DRVB1081-PROGRAMM           PIC  X(008).
          05 PI-DRVB1081-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1081-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB1081.

          05 PO-DRVB1081-RC                 PIC  X(003).
             88 PO-DRVB1081-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1081-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1081-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1081-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1081-DATEN.
             10 PO-DRVB1081-UNTST-NR        PIC S9(009) BINARY.
             10 PO-DRVB1081-EBENE           PIC S9(002) BINARY.
             10 PO-DRVB1081-STUFE           PIC S9(004) BINARY.

      *
      ***** ENDE COPY-MEMBER DRVB1081 *****************************

