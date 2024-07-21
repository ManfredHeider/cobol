      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0485.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0485.CPv  $
      *
      *   Rev 2.1   08 Jun 2000 15:37:18   T13AE27
      *neue Funktion
      *
      *   Rev 2.0   19 May 2000 09:49:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0485                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0040                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    05.06.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FSL00026!05.06.00! 01 ! NEUERSTELLUNG             ! P.Dürr   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0485                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0485.
          05 PI-DRVB0485-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0485-KNR-KL-TYP         PIC  S9(004) COMP.
          05 PI-DRVB0485-KTY-KL-TYP         PIC  S9(004) COMP.
      *
       01 PO-DRVB0485.

          05 PO-DRVB0485-RC                 PIC  X(003).
             88 PO-DRVB0485-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0485-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0485-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0485-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0485-DATEN.
             10 PO-DRVB0485-KL-ST-NR        PIC S9(009)  COMP.
      *


