      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0194.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0194.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Apr 1998 16:27:44   T13EX30
      *AAS00005 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:13:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0194                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0018                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    04.12.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!04.12.97! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0194                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0194.
          05 PI-DRVB0194-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0194-DIR-NR             PIC S9(004) COMP.
      *
       01 PO-DRVB0194.

          05 PO-DRVB0194-RC                 PIC  X(003).
             88 PO-DRVB0194-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0194-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0194-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0194-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0194-DATEN.
             10 PO-DRVB0194-VBNR            PIC S9(009)  COMP.
      *
      ***** Ende Copy-Member DRVB0194 *****************************
