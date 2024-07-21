      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0713.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0713.CPv  $
      *
      *   Rev 2.2   May 27 2004 09:09:42   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.1   May 24 2004 12:48:50   t13ex43
      *PGM - MAP-Manageraufbauplan
      *
      *   Rev 2.0   May 13 2004 09:42:46   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0713                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0091                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    24.05.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00183!24.05.04!2.1 ! NEU                       ! KLEEMEIER *
270504* ASL00183!27.05.04!2.2 ! PI-ANSPR-STATUS-2 HINZU   ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0713                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0713.
          05 PI-DRVB0713-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0713-MAP-ANSPR-VON      PIC  X(010).
          05 PI-DRVB0713-MAP-ANSPR-BIS      PIC  X(010).
          05 PI-DRVB0713-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
270504    05 PI-DRVB0713-KNR-ANSPR-STATUS-2 PIC S9(004) BINARY.
      *
       01 PO-DRVB0713.
          05 PO-DRVB0713-RC                 PIC  X(003).
             88 PO-DRVB0713-RC-OK                VALUE 'OK '.
             88 PO-DRVB0713-RC-EOF               VALUE 'EOF'.
          05 PO-DRVB0713-DATEN.
             10 PO-DRVB0713-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0713-MAP-ANSPR-VOM      PIC  X(010).
             10 PO-DRVB0713-MAP-STUFE          PIC  X(010).
             10 PO-DRVB0713-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0713-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0713-MAP-LAST-UPDATE    PIC  X(026).
             10 PO-DRVB0713-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0713-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0713-MAP-ANSPR-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0713-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0713-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0713 *****************************
      *
