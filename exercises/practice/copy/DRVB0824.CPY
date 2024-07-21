      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0824.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0824.CPv  $
      *
      *   Rev 2.1   Jun 21 2006 12:55:28   t13ae13
      *ASL00224: SL04 - Dimension 7 Aufbaubonus
      *
      *   Rev 2.0   May 08 2006 10:28:16   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0824                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0057                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    08.05.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00224!08.05.06!2.1 ! NEU                       ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0824                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0824.
          05 PI-DRVB0824-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0824-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0824-ABBA-PART-VBNR     PIC S9(009) BINARY.
          05 PI-DRVB0824-ABBA-ANSPR-VON     PIC  X(010).
          05 PI-DRVB0824-ABBA-ANSPR-BIS     PIC  X(010).
          05 PI-DRVB0824-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
       01 PO-DRVB0824.
          05 PO-DRVB0824-RC                 PIC  X(003).
             88 PO-DRVB0824-RC-OK                VALUE 'OK '.
             88 PO-DRVB0824-RC-EOF               VALUE 'EOF'.
          05 PO-DRVB0824-DATEN.
             10 PO-DRVB0824-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0824-ABBA-ANSPR-VOM     PIC  X(010).
             10 PO-DRVB0824-ABBA-PART-VBNR     PIC S9(009) BINARY.
             10 PO-DRVB0824-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0824-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0824-ABBA-LAST-UPDATE   PIC  X(026).
             10 PO-DRVB0824-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0824-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0824-ABBA-PART-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0824-ABBA-PART-EH       PIC S9(09)V9 COMP-3.
             10 PO-DRVB0824-ABBA-PART-SEIT     PIC  X(010).
             10 PO-DRVB0824-ABBA-BETREUT-SEIT  PIC  X(010).
             10 PO-DRVB0824-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0824-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0824 *****************************
      *
