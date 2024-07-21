      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.02.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0815.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0815.CPv  $
      *
      *   Rev 2.1   Feb 27 2006 08:58:38   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Feb 02 2006 16:01:50   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0815                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    21.03.2006                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00595!21.03.06! 01 ! NEUERSTELLUNG             ! P.Duerr  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0815                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0815.
          05 PI-DRVB0815-BETR               PIC  S9(009) COMP.
          05 PI-DRVB0815-KNR-WB-TYP         PIC  S9(004) COMP.
          05 PI-DRVB0815-KNR-WERTUNGS-TYP   PIC  S9(004) COMP.
          05 PI-DRVB0815-DAT-VON            PIC  X(010).
          05 PI-DRVB0815-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0815.

          05 PO-DRVB0815-RC                 PIC  X(003).
             88 PO-DRVB0815-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0815-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0815-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0815-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0815-DATEN.
             10 PO-DRVB0815-PARTEH-EH-DIR  PIC S9(009)V9 COMP-3.
             10 PO-DRVB0815-PARTEH-EH-INDIR PIC S9(009)V9 COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0815 *****************************
