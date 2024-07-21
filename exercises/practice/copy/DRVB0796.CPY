      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0796.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0796.CPv  $
      *
      *   Rev 2.1   Mar 20 2006 16:25:06   t13ex43
      *ACV00054: PGM - Change Request CTV-Service-Funktion
      *
      *   Rev 2.0   Sep 28 2005 14:08:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0796                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0004                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    20.03.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ACV00054!20.03.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0796                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0796.
          05 PI-DRVB0796-MA-ST-NR           PIC S9(009) BINARY.
      *
       01 PO-DRVB0796.
          05 PO-DRVB0796-RC                 PIC  X(003).
             88 PO-DRVB0796-RC-OK                VALUE 'OK '.
             88 PO-DRVB0796-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0796-DATEN.
             10 PO-DRVB0796-MA-AVAG-DVAG    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0796 *****************************
      *
