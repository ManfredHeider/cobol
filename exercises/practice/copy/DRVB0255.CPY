      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0255.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0255.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:30   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:22:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0255                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0014                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGUR-VUJIC                                       *
      *    DATUM:    23.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!23.06.98! 01 ! NEUERSTELLUNG             !ZGUR-VUJIC*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0255                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0255.
           05  PI-DRVB0255-MAN-KEY.
               10 PI-DRVB0255-MAN-BEZUGS-NR   PIC S9(007) BINARY.
      *
       01  PO-DRVB0255.
           05  PO-DRVB0255-RC                 PIC  X(003).
               88 PO-DRVB0255-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0255-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0255-MAN-TITEL-AKAD     PIC  X(010).
           05  PO-DRVB0255-MAN-VORNAME        PIC  X(020).
           05  PO-DRVB0255-MAN-NAME           PIC  X(040).
           05  PO-DRVB0255-ANREDE             PIC  s9(4) usage comp.
           05  PO-DRVB0255-geburtsdat         PIC  X(010).
           05  PO-DRVB0255-ni-geburtsdat      PIC  s9(4) usage comp.
      *
      ***** Ende Copy-Member DRVB0255 *****************************
