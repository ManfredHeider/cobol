      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0728.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0728.CPv  $
      *
      *   Rev 2.1   Sep 20 2004 15:18:20   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.0   Aug 16 2004 11:41:26   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0728                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0093                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    16.08.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00184!16.08.04! 2.1! NEU                       ! T.MUND   *
      *         !        !    !                           !          *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0728                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0728.
          05 PI-DRVB0728-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0728-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB0728-ANSPR-STATUS       PIC  S9(004) BINARY.
          05 PI-DRVB0728-LZUA-ST-NR         PIC  S9(009) BINARY.
          05 PI-DRVB0728-KNR-OLZ-TYP        PIC  S9(004) BINARY.
      *
       01 PO-DRVB0728.

          05 PO-DRVB0728-RC                 PIC  X(003).
             88 PO-DRVB0728-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0728-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0728-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0728-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0728-DATEN.
             10 PO-DRVB0728-LZUA-ANSPR-VOM     PIC  X(010).
             10 PO-DRVB0728-LZUA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0728-KNR-WAEHRUNG       PIC  X(003).
      *
      ***** ENDE COPY-MEMBER DRVB0728 *****************************
