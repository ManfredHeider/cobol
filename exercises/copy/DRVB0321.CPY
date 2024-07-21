      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0321.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0321.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Feb 1999 14:19:12   T13AE42
      *ASL00025 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:26   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0321                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    15.09.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL0002 !15.09.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0321                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0321.
          05 PI-DRVB0321-PROGRAMM           PIC  X(008).
          05 PI-DRVB0321-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0321-BVH-BETR-NR        PIC  S9(009).
          05 PI-DRVB0321-ZEITPUNKT-STAT     PIC  X(010).
      *
       01 PO-DRVB0321.

          05 PO-DRVB0321-RC                 PIC  X(003).
             88 PO-DRVB0321-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0321-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0321-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0321-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0321-DATEN.
             10 PO-DRVB0321-BVH-UNTST-NR     PIC  S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0321 *****************************
