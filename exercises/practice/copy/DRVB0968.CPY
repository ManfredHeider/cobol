      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0968.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0968.CPv  $
      *
      *   Rev 2.2   May 07 2012 09:08:54   t13ae36
      *AVB01219: Leinen Los (Bonuseinheiten): Übernahme der Stückli
      *ste 2 aus AVB1091
      *
      *   Rev 2.1   May 03 2010 08:03:48   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung 
      *
      *   Rev 2.0   Mar 02 2010 08:30:56   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0968                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0117                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    03.03.2010                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01031!03.03.10!2.1 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0968                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0968.
          05 PI-DRVB0968-EDW-DL-NEU         PIC S9(009) COMP.
          05 PI-DRVB0968-EDW-UNT-ST-NR      PIC S9(009) COMP.
          05 PI-DRVB0968-KNR-EDW-STATUS     PIC S9(004) COMP.
      *
       01 PO-DRVB0968.

          05 PO-DRVB0968-RC                 PIC  X(003).
             88 PO-DRVB0968-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0968-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0968-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0968-DATEN.
             10 PO-DRVB0968-NI-VIVB0117     PIC X(020).
             10 PO-DRVB0968-DCL-VIVB0117    PIC X(066).
      *
      ***** ENDE COPY-MEMBER DRVB0968 *****************************
