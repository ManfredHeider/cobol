      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0208.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0208.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 May 1998 15:20:42   T13EX30
      *AAS00005 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:15:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0208                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0006                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    18.02.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!18.02.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0208                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0208.
          05 PI-DRVB0208-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0208.

          05 PO-DRVB0208-RC                 PIC  X(003).
             88 PO-DRVB0208-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0208-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0208-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0208-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0208-DATEN.
             10 PO-DRVB0208-DIR-NR          PIC S9(004)  COMP.
      *
      ***** Ende Copy-Member DRVB0208 *****************************
