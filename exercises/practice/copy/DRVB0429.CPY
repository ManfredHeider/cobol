      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0429.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0429.CPv  $
      *
      *   Rev 2.2   Feb 29 2000 17:02:02   T13AE30
      *ASL00050 - Dimension 7 'OLB'
      *
      *   Rev 2.1   Feb 22 2000 16:27:44   T13AE30
      *ASL00047 - Neuerstellung
      *
      *   Rev 2.0   Jun 02 1999 14:44:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0429                                      *
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
      * ASL00047!09.02.00! 01 ! NEUERSTELLUNG             ! WALB     *
      * ASL00050!28.02.00! 02 ! ERWEITERUNG PO-BEREICH    ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0429                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0429.
          05 PI-DRVB0429-PROGRAMM           PIC  X(008).
          05 PI-DRVB0429-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0429-BETR-NR            PIC  S9(009).
          05 PI-DRVB0429-STUFE-AB           PIC  S9(004).
          05 PI-DRVB0429-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0429.

          05 PO-DRVB0429-RC                 PIC  X(003).
             88 PO-DRVB0429-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0429-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0429-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0429-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0429-DATEN.
             10 PO-DRVB0429-BVH-UNTST-NR     PIC  S9(009) COMP.
280200       10 PO-DRVB0429-MS-NR            PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0429 *****************************
