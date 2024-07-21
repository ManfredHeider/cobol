      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0685.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0685.CPv  $
      *
      *   Rev 2.2   May 27 2008 13:43:56   t13ae27
      *AAS00756: Wettbewerb 2008 der Allfinanz / Team- und Zusatzbo
      *nus
      *
      *   Rev 2.1   Dec 10 2003 09:58:08   t13ae27
      *OLB-Auszahlung
      *
      *   Rev 2.0   Feb 17 2003 11:00:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0685                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0088                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    14.10.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00172!14.10.03! 2.0! neu                       ! P.Duerr  *
      * ASL00756!02.05.08! 2.1! neue Funktion DRVB0902    ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
       01  PI-ZXVB0082.
           05  PI-ZXVB0082-FUNKTION            PIC  X(008).
               88 PI-ZXVB0082-DRVB0686              VALUE 'DRVB0686'.
020508         88 PI-ZXVB0082-DRVB0902              VALUE 'DRVB0902'.
      *
      ******* ENDE COPY-MEMBER **************************************
