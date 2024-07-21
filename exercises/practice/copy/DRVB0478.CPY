      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0478.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0478.CPv  $
      *
      *   Rev 2.2   16 Jun 2000 15:00:40   T13AE27
      *neue Funktion DRVB0488
      *
      *   Rev 2.1   22 May 2000 14:10:14   T13AE27
      *neue Funktion für ZWVB0076
      *
      *   Rev 2.0   Apr 03 2000 14:11:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0178                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0076                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    05.05.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00054!05.05.00! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * ASL00061!15.06.00! 02 ! NEUE Funktion DRVB0488    ! P.Dürr   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0076.
           05  PI-ZWVB0076-FUNKTION            PIC  X(008).
               88 PI-ZWVB0076-DRVB0476              VALUE 'DRVB0476'.
               88 PI-ZWVB0076-DRVB0477              VALUE 'DRVB0477'.
150600         88 PI-ZWVB0076-DRVB0488              VALUE 'DRVB0488'.
      *
      ******* ENDE COPY-MEMBER DRVB0478 *****************************

