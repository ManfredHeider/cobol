      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0811.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0811.CPv  $
      *
      *   Rev 2.3   Jun 03 2015 12:41:00   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.2   Jan 10 2007 11:01:00   t13ae27
      *AAS00667: JWB 2007 AIDAdiva - ERG-Ermittlung
      *
      *   Rev 2.1   Feb 27 2006 08:49:30   T13AE27
      *AAS00595: Jahreswettbewerb 2006
      *
      *   Rev 2.0   Jan 30 2006 12:16:58   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0811                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0038                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Dürr                                          *
      *    DATUM:    23.01.2006                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00595!23.01.06! 01 ! NEUERSTELLUNG             ! P.Dürr   *
      * AAS00667!28.12.06! 02 ! Neue Funktion DRVB0841    ! P.Dürr   *
      * AVB01332!24.03.15!2.3 ! Neue Funktion DRVB1088    ! S. Walb  *
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
       01  PI-ZXVB0038.
           05  PI-ZXVB0038-FUNKTION            PIC  X(008).
               88 PI-ZXVB0038-DRVB0812              VALUE 'DRVB0812'.
281206         88 PI-ZXVB0038-DRVB0841              VALUE 'DRVB0841'.
240315         88 PI-ZXVB0038-DRVB1088              VALUE 'DRVB1088'.
      *
      ******* ENDE COPY-MEMBER DRVB0811 *****************************
      *

