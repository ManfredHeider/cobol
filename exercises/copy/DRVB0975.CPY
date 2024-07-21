      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0975.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0975.CPv  $
      *
      *   Rev 2.1   Apr 06 2010 13:31:56   t13ex13
      *ASL00344: RGT: SL08
      *
      *   Rev 2.0   Mar 12 2010 10:41:12   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0975                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0200                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    12.03.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00344!12.03.10!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0975                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0975.
          05 PI-DRVB0975-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0975-CBA-ANSPR-VOM      PIC  X(010).
      *
       01 PO-DRVB0975.
          05 PO-DRVB0975-RC                 PIC  X(003).
             88 PO-DRVB0975-RC-OK                VALUE 'OK '.
             88 PO-DRVB0975-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0975-DATEN.
             10 PO-DRVB0975-CBA-PART-VBNR   PIC S9(009) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0975 *****************************
      *
