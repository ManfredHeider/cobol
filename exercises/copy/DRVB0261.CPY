      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0261.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0261.CPv  $
      *
      *   Rev 2.1   Oct 21 1999 14:55:00   T13EX50
      *AKI0003 - Produktionsübergabe :
      *                Neues Bestellverfahren 
      *                "Der Vermögensberater"
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:38   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:23:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0261                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        *
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
      * AKI00003!20.10.99! 02 ! AUSGABE MAD-DAT-BIS       !HERWEG    *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0261                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0261.
          05 PI-DRVB0261-PROGRAMM                PIC  X(008).
          05 PI-DRVB0261-MA-ST-NR                PIC  9(007) comp-3.
      *
       01 PO-DRVB0261.

          05 PO-DRVB0261-RC                 PIC  X(003).
             88 PO-DRVB0261-RC-OK                        VALUE 'OK '.

          05 PO-DRVB0261-DATEN.
             10 PO-DRVB0261-MAT-DAT-BIS     PIC  x(010).
      *
      ***** Ende Copy-Member DRVB0261 *****************************
