      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.04.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0822.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0822.CPv  $
      *
      *   Rev 2.1   Apr 18 2006 12:20:28   t13ae13
      *ASL00226: Dimension 7 Zukunft - MAP: OZP für MAPA
      *
      *   Rev 2.0   Apr 10 2006 12:11:40   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0822                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0091                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    10.04.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00226!10.04.06!2.1 ! NEU                       ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0822                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0822.
          05 PI-DRVB0822-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0822-MAP-ANSPR-VON      PIC  X(010).
          05 PI-DRVB0822-MAP-ANSPR-BIS      PIC  X(010).
          05 PI-DRVB0822-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
          05 PI-DRVB0822-KNR-ANSPR-STATUS-2 PIC S9(004) BINARY.
      *
       01 PO-DRVB0822.
          05 PO-DRVB0822-RC                 PIC  X(003).
             88 PO-DRVB0822-RC-OK                VALUE 'OK '.
             88 PO-DRVB0822-RC-EOF               VALUE 'EOF'.
          05 PO-DRVB0822-DATEN.
             10 PO-DRVB0822-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0822-MAP-ANSPR-VOM      PIC  X(010).
             10 PO-DRVB0822-MAP-STUFE          PIC  X(010).
             10 PO-DRVB0822-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0822-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0822-MAP-LAST-UPDATE    PIC  X(026).
             10 PO-DRVB0822-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0822-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0822-MAP-ANSPR-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0822-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0822-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0822-MAPA-UNTST         PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0822 *****************************
      *
