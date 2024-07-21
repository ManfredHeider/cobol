      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0265.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0265.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:46   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:23:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0265                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0055                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGUR-VUJIC                                       *
      *    DATUM:    29.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!22.06.98! 01 ! NEUERSTELLUNG             !ZGUR-VUJIC*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0265                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0265.
          05 PI-DRVB0265-MA-ST-NR                PIC  9(007) comp-3.
          05 PI-DRVB0265-KNR-MAST-TYP            PIC S9(4) USAGE COMP.
          05 PI-DRVB0265-KTY-MAST-TYP            PIC S9(4) USAGE COMP.
      *
       01 PO-DRVB0265.

          05 PO-DRVB0265-RC                 PIC  X(003).
             88 PO-DRVB0265-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0265-RC-NFD                       VALUE 'NFD'.

      ***** Ende Copy-Member DRVB0265 *****************************
