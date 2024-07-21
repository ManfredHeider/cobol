      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0709.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0709.CPv  $
      *
      *   Rev 2.3   May 14 2004 09:30:50   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.2   May 13 2004 16:31:18   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.1   May 11 2004 15:04:42   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.0   May 10 2004 14:07:24   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0709                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0091                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.05.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!10.05.04!2.1 ! NEU                       ! KLEEMEIER *
130504* ASL00183!13.05.04!2.2 ! PI-KNR-ANSPR-GRUND HINZU  ! KLEEMEIER *
140504* ASL00183!14.05.04!2.3 ! ANSPR-STATUS STATT -GRUND ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0709                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0709.
          05 PI-DRVB0709-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0709-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0709-MAP-STUFE          PIC  X(010).
          05 PI-DRVB0709-MAP-ANSPR-VON      PIC  X(010).
          05 PI-DRVB0709-MAP-ANSPR-BIS      PIC  X(010).
140504****05 PI-DRVB0709-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
140504    05 PI-DRVB0709-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
       01 PO-DRVB0709.
          05 PO-DRVB0709-RC                 PIC  X(003).
             88 PO-DRVB0709-RC-OK                VALUE 'OK '.
             88 PO-DRVB0709-RC-EOF               VALUE 'EOF'.
          05 PO-DRVB0709-DATEN.
             10 PO-DRVB0709-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0709-MAP-ANSPR-VOM      PIC  X(010).
             10 PO-DRVB0709-MAP-STUFE          PIC  X(010).
             10 PO-DRVB0709-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0709-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0709-MAP-LAST-UPDATE    PIC  X(026).
             10 PO-DRVB0709-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0709-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0709-MAP-ANSPR-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0709-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0709-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0709 *****************************
      *
