      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0746.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRVB0746.CPv  $
      *
      *   Rev 2.8   Jul 10 2015 08:52:36   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI/2 
      *
      *   Rev 2.7   Jun 03 2015 12:41:24   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.6   Mar 24 2009 14:21:48   t13ae27
      *AAS00813: Einheiten neuer Partner
      *
      *   Rev 2.5   Feb 27 2006 08:42:20   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.4   May 23 2005 10:29:52   t13ae27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.3   Mar 24 2005 16:10:48   T13AE27
      *FAS00154: Jahres-WBW 2005 Wien
      *
      *   Rev 2.2   Mar 14 2005 09:36:08   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.1   Feb 11 2005 11:39:40   T13AE27
      *AAS00513: Jubiläums-Wettbewerbs/Mittelmeer-Schiffsreise 2005.
      *
      *   Rev 2.0   Dec 06 2004 12:25:14   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0746                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS PROGRAMM ZWVB0038                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                         *
      *    DATUM:    01.12.2004                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00513!01.12.04! 01 ! NEU                       ! P. Duerr *
      * AAS00531!28.02.05! 02 ! neue Funktion DRVB0757    ! P. Duerr *
      * FAS00154!24.03.05! 03 ! neue Funktion DRVB0767    ! P. Duerr *
      * AAS00531!14.04.05! 04 ! neue Funktion DRVB0775    ! P. Duerr *
      * AAS00595!23.01.06! 05 ! neue Funktion DRVB0809    ! P. Duerr *
      *         !        !    ! neue Funktion DRVB0813    ! P. Duerr *
      *         !        !    ! neue Funktion DRVB0814    ! P. Duerr *
      *         !        !    ! neue Funktion DRVB0815    ! P. Duerr *
      *         !        !    !                           !          *
      * AAS00813!12.03.09! 06 ! neue Funktion DRVB0930    ! P. Duerr *
      *         !        !    ! neue Funktion DRVB0931    ! P. Duerr *
      *         !        !    ! neue Funktion DRVB0932    ! P. Duerr *
      * AVB01332!13.04.15!2.7 ! neue Funktionen DRVB1090- ! S. Walb  *
      *         !        !    ! DRVB1095                  !          *
      * AVB01332!01.07.15!2.8 ! neue Funktion DRVB1110    ! S. Walb  *
      *         !        !    !                           !          *
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
       01  PI-ZWVB0038.
           05  PI-ZWVB0038-FUNKTION            PIC  X(008).
               88 PI-ZWVB0038-DRVB0747              VALUE 'DRVB0747'.
               88 PI-ZWVB0038-DRVB0749              VALUE 'DRVB0749'.
               88 PI-ZWVB0038-DRVB0757              VALUE 'DRVB0757'.
240305         88 PI-ZWVB0038-DRVB0767              VALUE 'DRVB0767'.
140405         88 PI-ZWVB0038-DRVB0775              VALUE 'DRVB0775'.
230106         88 PI-ZWVB0038-DRVB0809              VALUE 'DRVB0809'.
230106         88 PI-ZWVB0038-DRVB0813              VALUE 'DRVB0813'.
230106         88 PI-ZWVB0038-DRVB0814              VALUE 'DRVB0814'.
230106         88 PI-ZWVB0038-DRVB0815              VALUE 'DRVB0815'.
120309         88 PI-ZWVB0038-DRVB0930              VALUE 'DRVB0930'.
120309         88 PI-ZWVB0038-DRVB0931              VALUE 'DRVB0931'.
120309         88 PI-ZWVB0038-DRVB0932              VALUE 'DRVB0932'.
130415         88 PI-ZWVB0038-DRVB1090              VALUE 'DRVB1090'.
130415         88 PI-ZWVB0038-DRVB1091              VALUE 'DRVB1091'.
130415         88 PI-ZWVB0038-DRVB1092              VALUE 'DRVB1092'.
130415         88 PI-ZWVB0038-DRVB1093              VALUE 'DRVB1093'.
130415         88 PI-ZWVB0038-DRVB1094              VALUE 'DRVB1094'.
130415         88 PI-ZWVB0038-DRVB1095              VALUE 'DRVB1095'.
010715         88 PI-ZWVB0038-DRVB1110              VALUE 'DRVB1110'.
      *
      ******* ENDE COPY-MEMBER **************************************


