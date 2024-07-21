      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0754.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0754.CPv  $
      *
      *   Rev 2.1   Mar 14 2005 09:43:50   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.0   Feb 16 2005 15:23:56   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0754                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    01.02.2005                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00531!01.02.05! 01 ! NEUERSTELLUNG             ! P.Duerr  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0748                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0754.
          05 PI-DRVB0754-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0754-STUFE-AB           PIC  S9(004) COMP.
          05 PI-DRVB0754-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0754-DATUM-VON          PIC  X(010).
          05 PI-DRVB0754-DATUM-BIS          PIC  X(010).
      *
       01 PO-DRVB0754.

          05 PO-DRVB0754-RC                 PIC  X(003).
             88 PO-DRVB0754-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0754-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0754-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0754-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0754-DATEN.
             10 PO-DRVB0754-MA-ST-NR        PIC S9(009)  COMP.
      *      10 PO-DRVB0754-MSBZ-DAT-VON    PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0748 *****************************
