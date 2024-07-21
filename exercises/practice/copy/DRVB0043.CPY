      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0043.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0043.CPv  $
      *
      *   Rev 2.3   Jan 23 2018 12:40:00   t13ae13
      *AVB01479: Einführung Generali - Copies und Upros (swl)
      *
      *   Rev 2.2   Mar 18 2008 07:56:52   t13ae13
      *AVB00961: FVD Stufe 1 -> VB15, VB22, VB21 (swl)
      *
      *   Rev 2.1   30 Oct 2007 11:06:46   t13ae30
      *AVB00905: Allfinanz - Tabellenerweiterungen TWI
      *
      *   Rev 2.0   Jul 26 1999 09:19:44   T13EX21
      *APA00046 - Einführung DAISY-Batch
      *
      *   Rev 1.1   Apr 09 1997 16:55:52   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Nov 04 1995 09:11:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * A C H T U N G: Die Programme des VB-Bereichs in Produktion
      * laufen z. Zt. nicht mit dieser Version der Copystrecke, da
      * sonst sehr viele Programme neu umgewandelt werden müßten und
      * die betroffene Konstante dort eh nicht angesprochen wird.
      * Festgelegt von Hr. Moll / Jäger-Sokoluk im Juni 1998
      ****************************************************************
      *    COPY-MEMBER: DRVB0043  VB-SONDERNUMMERN                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                 *
      *    DATUM:    13.08.1993                                      *
      ****************************************************************
      *    VERSION   1    31.03.94 17:56:13
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !13.08.93! 01 ! NEU                       ! SCHOELLH.*
      * AFVB0044!29.11.93!    ! AVBG-AUFGENOMMEN,         ! GIESE    *
      *         !        !    ! K-VB-SDNR-8000000 ERSETZT ! SCHÖLLH. *
      *         !30.03.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * DAISY   !14.02.97! 03 ! erweiterung               ! Giese    *
      * APA00012!29.06.98! 04 ! Korrektur von 03          ! Jäger    *
      * AVB00905!06.06.07! 05 ! EINFÜHRUNG ALLFINANZ      ! WILLIG   *
      * AVB00961!28.02.08!2.2 ! EINFÜHRUNG FVD            ! WALB     *
      * AVB01479!23.01.18!2.3 ! EINFÜHRUNG Allfinanz AG   ! WALB     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    VB-SONDERNUMMERN
      *
      *01  K-VB-SDNR-8000000          PIC S9(009) COMP   VALUE 8000000.
       01  K-DVAG-VERWALTUNGSFOND     PIC S9(009) COMP   VALUE 8000000.
       01  K-DVAG-BEREINIGUNGSNR      PIC S9(009) COMP   VALUE 8008001.
140297*01  K-DVAG-VERTRIEBSFOND       PIC S9(009) COMP   VALUE 8008020.
290698 01  K-DVAG-VERTRIEBSFOND       PIC S9(009) COMP   VALUE 8000020.
       01  K-AVBG-VERWALTUNGSFOND     PIC S9(009) COMP   VALUE 5000000.
       01  K-AVBG-VERTRIEBSFOND       PIC S9(009) COMP   VALUE 5000020.
060607 01  K-ALLFIN-VERWALTUNGSFOND   PIC S9(009) COMP   VALUE 1000000.
060607 01  K-ALLFIN-VERTRIEBSFOND     PIC S9(009) COMP   VALUE 1000020.
280208 01  K-FVD-VERWALTUNGSFOND      PIC S9(009) COMP   VALUE 1700000.
280208 01  K-FVD-VERTRIEBSFOND        PIC S9(009) COMP   VALUE 1700020.
230118 01  K-ALLFAG-VERWALTUNGSFOND   PIC S9(009) COMP   VALUE 2000000.
230118 01  K-ALLFAG-VERTRIEBSFOND     PIC S9(009) COMP   VALUE 2000020.
      ******** ENDE COPY-MEMBER DRVB0043 (VB-SONDERNUMMERN) ***********

