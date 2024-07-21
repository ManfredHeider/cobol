      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.09.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0795.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0795.CPv  $
      *
      *   Rev 2.1   Nov 28 2005 15:51:28   t13ex43
      *AVB00828: PGM - Adressänderungen für PC-Besitzer bereitstellen
      *
      *   Rev 2.0   Sep 28 2005 14:08:10   t13ex43
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0795                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0031                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    25.11.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00828!25.11.05!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0795                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0795.
          05 PI-DRVB0795-MA-ST-NR           PIC S9(009) BINARY.
          05 PI-DRVB0795-KNR-PCZ-TYP-1      PIC S9(004) BINARY.
          05 PI-DRVB0795-KNR-PCZ-TYP-2      PIC S9(004) BINARY.
          05 PI-DRVB0795-KNR-PCZ-TYP-3      PIC S9(004) BINARY.
          05 PI-DRVB0795-KNR-PCZ-TYP-4      PIC S9(004) BINARY.
          05 PI-DRVB0795-KNR-PCZ-TYP-5      PIC S9(004) BINARY.
      *
       01 PO-DRVB0795.
          05 PO-DRVB0795-RC                 PIC  X(003).
             88 PO-DRVB0795-RC-OK                VALUE 'OK '.
             88 PO-DRVB0795-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB0795-DATEN.
             10 PO-DRVB0795-PC-ANZAHL       PIC  9(001).
      *
      ***** ENDE COPY-MEMBER DRVB0795 *****************************
      *
