      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0742.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0742.CPv  $
      *
      *   Rev 2.1   Nov 04 2004 10:36:02   t13ex43
      *AVB00771: Z-PGMe - Steuernummer auf der VB-Abrechnung
      *
      *   Rev 2.0   Nov 03 2004 10:55:44   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0742                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0004                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    03.11.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00771!03.11.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0742                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0742.
          05 PI-DRVB0742-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB0742.
          05 PO-DRVB0742-RC                 PIC  X(003).
             88 PO-DRVB0742-RC-OK                VALUE 'OK '.
             88 PO-DRVB0742-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0742-DATEN.
             10 PO-DRVB0742-MA-STEUER-NR    PIC  X(015).
      *
      ***** ENDE COPY-MEMBER DRVB0742 *****************************
      *
