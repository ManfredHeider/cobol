      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0501.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0501.CPv  $
      *
      *   Rev 2.1   20 Sep 2000 15:48:34   T13AE27
      *DIM7-AVBG
      *
      *   Rev 2.0   07 Sep 2000 11:31:58   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0501                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    04.09.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00060!04.09.00! 01 ! NEUERSTELLUNG             ! P.Dürr   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0501                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0501.
          05 PI-DRVB0501-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0501.

          05 PO-DRVB0501-RC                 PIC  X(003).
             88 PO-DRVB0501-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0501-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0501-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0501-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0501-DATEN.
             10 PO-DRVB0501-RFT-ST-NR       PIC S9(009)  COMP.
             10 PO-DRVB0501-RFT-DAT-VON     PIC X(010).
             10 PO-DRVB0501-RFT-DAT-BIS     PIC X(010).
      *


