      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0821.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0821.CPv  $
      *
      *   Rev 2.1   Apr 18 2006 12:02:46   t13ae13
      *ASL00226: Dimension 7 Zukunft - MAP: OZP für MAPA
      *
      *   Rev 2.0   Mar 30 2006 11:06:04   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0821                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0091                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    30.03.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00226!30.03.06!2.1 ! NEU                       ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0821                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0821.
          05 PI-DRVB0821-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0821-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0821-MAP-STUFE          PIC  X(010).
          05 PI-DRVB0821-MAP-ANSPR-VON      PIC  X(010).
          05 PI-DRVB0821-MAP-ANSPR-BIS      PIC  X(010).
          05 PI-DRVB0821-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
       01 PO-DRVB0821.
          05 PO-DRVB0821-RC                 PIC  X(003).
             88 PO-DRVB0821-RC-OK                VALUE 'OK '.
             88 PO-DRVB0821-RC-EOF               VALUE 'EOF'.
          05 PO-DRVB0821-DATEN.
             10 PO-DRVB0821-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0821-MAP-ANSPR-VOM      PIC  X(010).
             10 PO-DRVB0821-MAP-STUFE          PIC  X(010).
             10 PO-DRVB0821-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0821-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0821-MAP-LAST-UPDATE    PIC  X(026).
             10 PO-DRVB0821-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0821-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0821-MAP-ANSPR-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0821-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0821-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0821-MAPA-UNTST         PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0821 *****************************
      *
