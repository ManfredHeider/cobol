      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0656.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0656.CPv  $
      *
      *   Rev 2.1   Jun 10 2003 14:21:28   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.0   Feb 17 2003 10:57:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0656                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    03.06.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!03.06.03! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0656                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0656.
          05 PI-DRVB0656-PROGRAMM           PIC  X(008).
          05 PI-DRVB0656-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0656-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0656-DAT-VON            PIC  X(010).
          05 PI-DRVB0656-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0656.

          05 PO-DRVB0656-RC                 PIC  X(003).
             88 PO-DRVB0656-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0656-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0656-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0656-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0656-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0656-DATEN.
             10 PO-DRVB0656-GEEV-MONAT      PIC   X(010).
             10 PO-DRVB0656-EH-GS-XX-GR     PIC  S9(009)V99 COMP-3.
      *
      ***** ENDE-COPY-MEMBER *****************************************
