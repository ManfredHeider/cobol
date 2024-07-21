      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0800.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0800.CPv  $
      *
      *   Rev 2.1   Nov 04 2005 11:57:28   T13AE27
      *AAS00572: Siegeradler 2005
      *
      *   Rev 2.0   Oct 12 2005 13:24:18   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0800                                      *
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
      *    FÜR DIE FUNKTION DRVB0800                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0800.
          05 PI-DRVB0800-PROGRAMM           PIC  X(008).
          05 PI-DRVB0800-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0800-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0800-DAT-VON            PIC  X(010).
          05 PI-DRVB0800-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0800.

          05 PO-DRVB0800-RC                 PIC  X(003).
             88 PO-DRVB0800-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0800-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0800-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0800-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0800-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0800-DATEN.
             10 PO-DRVB0800-GEEV-MONAT      PIC   X(010).
             10 PO-DRVB0800-EH-GS-XX-GR-NEU PIC  S9(009)V99 COMP-3.
      *
      ***** ENDE-COPY-MEMBER *****************************************
