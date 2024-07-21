      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0493.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0493.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 09:35:32   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *
      *   Rev 2.0   19 May 2000 09:50:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0493                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    28.06.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!28.06.00! 01 ! NEUERSTELLUNG             ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0493                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0493.
          05 PI-DRVB0493-MS-NR              PIC S9(004) COMP.
      *
       01 PO-DRVB0493.

          05 PO-DRVB0493-RC                 PIC  X(003).
             88 PO-DRVB0493-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0493-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0493-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0493-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0493-DATEN.
             10 PO-DRVB0493-MS-EH-EIG-PZTR     PIC 9(009)V9.
             10 PO-DRVB0493-MS-EH-GRU-PZTR     PIC 9(009)V9.
             10 PO-DRVB0493-MS-EH-GES-PZTR     PIC 9(009)V9.
      *
      ***** ENDE COPY-MEMBER DRVB0493 *****************************
