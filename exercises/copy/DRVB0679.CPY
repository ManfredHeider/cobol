      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0679.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0679.CPv  $
      *
      *   Rev 2.1   Sep 30 2003 12:12:26   t13ex43
      *ASL00169 : Zusatzbonus 2003
      *
      *   Rev 2.0   Feb 17 2003 10:59:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0679                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0090                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    04.09.2003                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00169!04.09.03!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0679                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0679.
          05 PI-DRVB0679-ZBOA-ST-NR         PIC S9(009) BINARY.
          05 PI-DRVB0679-ZBOA-ANSPR-VOM     PIC  X(010).
          05 PI-DRVB0679-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
      *
       01 PO-DRVB0679.
          05 PO-DRVB0679-RC                 PIC  X(003).
             88 PO-DRVB0679-RC-OK                VALUE 'OK '.
             88 PO-DRVB0679-RC-LEER              VALUE 'LER'.
             88 PO-DRVB0679-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB0679-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0679-DATEN.
             10 PO-DRVB0679-ZBOA-TUPEL-GESAMT PIC  X(078).
      *
      ***** ENDE COPY-MEMBER DRVB0679 *****************************
      *
