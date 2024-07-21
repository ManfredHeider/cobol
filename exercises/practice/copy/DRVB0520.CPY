      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0520.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0520.CPv  $
      *
      *   Rev 2.1   30 Mar 2001 06:59:56   T13AE13
      *ASL00068 - ORG für GS-D analog ORG für RGS
      *
      *   Rev 2.0   07 Sep 2000 11:32:28   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0520                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    09.02.00                                         *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00068!13.02.01! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0520                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0520.
          05 PI-DRVB0520-PROGRAMM           PIC  X(008).
          05 PI-DRVB0520-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0520-BETR-NR            PIC  S9(009).
          05 PI-DRVB0520-STUFE-AB           PIC  S9(004).
          05 PI-DRVB0520-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0520.

          05 PO-DRVB0520-RC                 PIC  X(003).
             88 PO-DRVB0520-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0520-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0520-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0520-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0520-DATEN.
             10 PO-DRVB0520-BVH-UNTST-NR     PIC  S9(009) COMP.
             10 PO-DRVB0520-MS-NR            PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0520 *****************************
