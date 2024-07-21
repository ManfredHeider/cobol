      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0609.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0609.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:29:56   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.0   Nov 05 2002 11:35:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0609                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr, P.                                         *
      *    DATUM:    09.02.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00149!00.11.02! 01 ! NEUERSTELLUNG             ! Dürr     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0609                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0609.
          05 PI-DRVB0609-PROGRAMM           PIC  X(008).
          05 PI-DRVB0609-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0609-DAT-VON            PIC  X(026).
          05 PI-DRVB0609-DAT-BIS            PIC  X(026).
      *
       01 PO-DRVB0609.

          05 PO-DRVB0609-RC                 PIC  X(003).
             88 PO-DRVB0609-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0609-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0609-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0609-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0609-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0609-DATEN.
             10 PO-DRVB0609-MA-ST-NR        PIC  S9(009)    COMP.
             10 PO-DRVB0609-DAT-VON         PIC  X(010).
             10 PO-DRVB0609-CURSOR-KZ       PIC  X(001).
      *
      ***** Ende-Copy-Member *****************************************

