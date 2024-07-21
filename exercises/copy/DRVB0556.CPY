      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0556.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0556.CPv  $
      *
      *   Rev 2.1   24 Aug 2001 15:02:56   t13ex57
      *AVB00534 - ERSTELLEN SCHNITTSTELLE
      * FÜR VB PORTAL
      *
      *   Rev 2.0   15 May 2001 13:33:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0556                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0055                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: UTE BOLL                                         *
      *    DATUM:    31.07.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!31.07.01! 01 ! NEUERSTELLUNG             !U. BOLL   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0556                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0556.
          05 PI-DRVB0556-MA-ST-NR                PIC  9(007) comp-3.
          05 PI-DRVB0556-KNR-MAST-TYP            PIC S9(4) USAGE COMP.
          05 PI-DRVB0556-KTY-MAST-TYP            PIC S9(4) USAGE COMP.
      *
       01 PO-DRVB0556.

          05 PO-DRVB0556-RC                 PIC  X(003).
             88 PO-DRVB0556-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0556-RC-NFD                       VALUE 'NFD'.

      ***** Ende Copy-Member DRVB0556 *****************************
