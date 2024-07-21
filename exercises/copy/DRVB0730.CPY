      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0730.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0730.CPv  $
      *
      *   Rev 2.2   Sep 23 2004 10:59:10   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *Satzl
      *ängenkorrektur DRVB0730
      *
      *   Rev 2.1   Sep 20 2004 15:18:36   t13ex22
      *ASL00184: Bestandsleistungszuschuss Ermittlung & SL01
      *
      *   Rev 2.0   Aug 16 2004 15:14:46   t13ex22
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0730                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0093                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *                                                               *
      *    GESAMTES TUPEL AUS LZUA                                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.MUND                                           *
      *    DATUM:    11.08.2004                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00184!11.08.04! 2.1! NEU                       ! MUND     *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0730                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRVB0730.
          05 PI-DRVB0730-LZUA-ST-NR         PIC S9(009) BINARY.
          05 PI-DRVB0730-LZUA-ANSPR-VOM     PIC  X(010).
          05 PI-DRVB0730-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
          05 PI-DRVB0730-KNR-OLZ-TYP        PIC S9(004) BINARY.
      *
       01 PO-DRVB0730.

          05 PO-DRVB0730-RC                 PIC  X(003).
             88 PO-DRVB0730-RC-OK                VALUE 'OK '.
             88 PO-DRVB0730-RC-LEER              VALUE 'LER'.
             88 PO-DRVB0730-RC-EOF               VALUE 'EOF'.
             88 PO-DRVB0730-RC-NOT-FOUND         VALUE 'NFD'.

          05 PO-DRVB0730-DATEN.
             10 PO-DRVB0730-LZUA-TUPEL-GESAMT PIC  X(086).
      *
      ***** ENDE COPY-MEMBER DRVB0730 *****************************
