      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0379.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0379.CPv  $
      *
      *   Rev 2.1   Apr 26 1999 12:18:04   T13AE42
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0379                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0029                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    15.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00060!15.04.99! 01 ! NEUERSTELLUNG             ! SCHMIDT,B*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0379                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0379.
          05 PI-DRVB0379-PROGRAMM           PIC  X(008).
          05 PI-DRVB0379-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0379.

          05 PO-DRVB0379-RC                 PIC  X(003).
             88 PO-DRVB0379-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0379-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0379-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0379-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0379-DATEN.
             10 PO-DRVB0379-MA-ST-NR          PIC S9(9)    COMP.
             10 PO-DRVB0379-KTY-SLZ-TYP       PIC S9(4)    COMP.
             10 PO-DRVB0379-KNR-SLZ-TYP       PIC S9(4)    COMP.
             10 PO-DRVB0379-SLZ-LAST-UPDATE   PIC X(026).
             10 PO-DRVB0379-SLZ-DAT-VON       PIC X(010).
             10 PO-DRVB0379-SLZ-BESCHREIBUNG  PIC X(010).
      *
      ***** ENDE-COPY-MEMBER *****************************************

