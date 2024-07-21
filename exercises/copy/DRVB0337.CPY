      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0337.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0337.CPv  $
      *
      *   Rev 2.2   Jun 25 1999 08:20:42   T13AE30
      *AAS00051 - Stammdaten -DTA ---> Aachen
      *
      *   Rev 2.1   Jun 25 1999 08:11:32   T13AE30
      * 
      *
      *   Rev 2.0   Apr 13 1999 11:43:26   T13AE30
      * 
      *
      *   Rev 1.0   27 Oct 1998 16:35:12   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0337                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    04.11.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASS00051!04.11.98! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0337                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0337.
          05 PI-DRVB0337-PROGRAMM           PIC  X(008).
          05 PI-DRVB0337-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0337.

          05 PO-DRVB0337-RC                 PIC  X(003).
             88 PO-DRVB0337-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0337-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0337-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0337-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0337-DATEN.
             10 PO-DRVB0337-ST-NR           PIC S9(9)    COMP.
             10 PO-DRVB0337-ST-PRUEFZIFFER  PIC  X(1)        .
             10 PO-DRVB0337-KNR-ST-TYP      PIC S9(4)    COMP.
             10 PO-DRVB0337-KTY-ST-TYP      PIC S9(4)    COMP.
             10 PO-DRVB0337-KNR-GESELL-TYP  PIC S9(4)    COMP.
             10 PO-DRVB0337-KTY-GESELL-TYP  PIC S9(4)    COMP.
      *
      ***** Ende-Copy-Member *****************************************
