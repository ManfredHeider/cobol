      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0252.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0252.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:24   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:22:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0252                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    29.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!29.06.98! 01 ! NEUERSTELLUNG             ! Zgur-Vujic
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0252                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0252.
          05 PI-DRVB0252-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0252.

          05 PO-DRVB0252-RC                 PIC  X(003).
             88 PO-DRVB0252-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0252-RC-NFD                       VALUE 'NFD'.

          05 PO-DRVB0252-DATEN.
             10 PO-DRVB0252-VBP-NR           PIC  S9(004) COMP.
             10 PO-DRVB0252-VBPZ-DAT-BIS     PIC   x(010).
      *
      ***** ENDE COPY-MEMBER DRVB0252 *****************************
