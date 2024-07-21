      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0820.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0820.CPv  $
      *
      *   Rev 2.1   Apr 18 2006 12:15:10   t13ae13
      *ASL00226: Dimension 7 Zukunft - MAP: OZP für MAPA
      *
      *   Rev 2.0   Mar 29 2006 13:21:54   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0820                                      *
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
      *    FÜR DIE FUNKTION DRVB0820                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0820.
          05 PI-DRVB0820-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0820-MAPA-UNTST         PIC S9(009) BINARY.
          05 PI-DRVB0820-MAP-STUFE          PIC  X(010).
          05 PI-DRVB0820-MAP-ANSPR-VON      PIC  X(010).
          05 PI-DRVB0820-MAP-ANSPR-BIS      PIC  X(010).
          05 PI-DRVB0820-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
       01 PO-DRVB0820.
          05 PO-DRVB0820-RC                 PIC  X(003).
             88 PO-DRVB0820-RC-OK                VALUE 'OK '.
             88 PO-DRVB0820-RC-EOF               VALUE 'EOF'.
          05 PO-DRVB0820-DATEN.
             10 PO-DRVB0820-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0820-MAP-ANSPR-VOM      PIC  X(010).
             10 PO-DRVB0820-MAP-STUFE          PIC  X(010).
             10 PO-DRVB0820-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0820-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0820-MAP-LAST-UPDATE    PIC  X(026).
             10 PO-DRVB0820-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0820-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0820-MAP-ANSPR-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0820-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0820-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0820-MAPA-UNTST         PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0820 *****************************
      *
