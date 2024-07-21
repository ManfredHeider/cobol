      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0817.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0817.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 09:47:42   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Feb 20 2006 10:40:56   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0817                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    06.08.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00595!23.01.06! 01 ! NEUERSTELLUNG             ! P. Duerr *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0817                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0817.
          05 PI-DRVB0817-BVH-UNTST-NR       PIC S9(009) COMP.
          05 PI-DRVB0817-BVH-BETR-NR        PIC S9(009) COMP.
          05 PI-DRVB0817-ZEITPUNKT-VON      PIC X(010).
          05 PI-DRVB0817-ZEITPUNKT-BIS      PIC X(010).
      *
       01 PO-DRVB0817.

          05 PO-DRVB0817-RC                 PIC  X(003).
             88 PO-DRVB0817-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0817-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0817-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0817-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0817-DATEN.
             10 PO-DRVB0817-ANZAHL          PIC S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0817 *****************************
