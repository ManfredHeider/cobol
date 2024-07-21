      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0757.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0757.CPv  $
      *
      *   Rev 2.1   Mar 14 2005 09:46:22   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.0   Feb 28 2005 13:14:16   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0757                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    05.03.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00531!28.02.05! 01 ! NEUERSTELLUNG             ! P. Duerr *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0757                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0757.
          05 PI-DRVB0757-BETR               PIC  S9(009) COMP.
          05 PI-DRVB0757-KNR-WB-TYP         PIC  S9(004) COMP.
          05 PI-DRVB0757-KNR-WERTUNGS-TYP   PIC  S9(004) COMP.
          05 PI-DRVB0757-GESELL             PIC  S9(004) COMP.
          05 PI-DRVB0757-DAT-VON            PIC  X(010).
          05 PI-DRVB0757-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0757.

          05 PO-DRVB0757-RC                 PIC  X(003).
             88 PO-DRVB0757-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0757-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0757-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0757-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0757-DATEN.
             10 PO-DRVB0757-PARTEH-EH      PIC S9(009)V9 COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0748 *****************************
