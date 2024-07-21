      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0818.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0818.CPv  $
      *
      *   Rev 2.3   Sep 10 2008 11:51:02   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Batch)
      *
      *   Rev 2.2   Oct 18 2006 11:40:56   t13ae30
      *AVB00882: F&A-SST, Vers.Nr. für VBZ
      *
      *   Rev 2.1   Mar 09 2006 14:30:36   t13ae35
      *ACV00078: 2 neue CTV-Onlinebriefe für Rückstufungen
      *
      *   Rev 2.0   Mar 08 2006 16:31:28   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0024
      *    ERSTELLT: Hering
      *    DATUM:    08.03.2006                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00078!08.03.06! 2.1! NEU                       ! J.Hering *
      * AVB00882!05.09.06! 2.2! NEUE FUNKTION DRVB0833    ! TH.WILLIG*
170708* ASL00309!17.07.08! 2.3! NEUE FUNKTION DRVB0909    ! KLEEMEIER*
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-ZWVB0024.
           05  PI-ZWVB0024-FUNKTION            PIC  X(008).
               88 PI-ZWVB0024-DRVB0819              VALUE 'DRVB0819'.
               88 PI-ZWVB0024-DRVB0833              VALUE 'DRVB0833'.
170708         88 PI-ZWVB0024-DRVB0909              VALUE 'DRVB0909'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
