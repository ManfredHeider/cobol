      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0034.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAS0034.CPv  $
      *
      *   Rev 2.4   Feb 12 2018 12:32:18   t13ae30
      *FAS00330: Siegeradler DVAG, Wien EH Korrektur 
      *
      *   Rev 2.3   Jan 17 2018 14:14:28   t13ae30
      *AAS01225: Programme - TWI
      *
      *   Rev 2.2   Sep 20 2017 14:02:00   T13AE30
      *FAS00328: Siegeradler - Korr. UPAS0009
      *
      *   Rev 2.1   May 03 2012 16:47:58   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.0   Mar 08 2012 15:59:12   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAS0034                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DAS UPRO UPAS0009                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    08.03.2012                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01008!08.03.12! 01 ! NEU                       !TH.WILLIG *
      * FAS00328!20.09.17! 02 ! NEUE FUNKTION DRAS0045    !TH.WILLIG *
      * AAS01225!10.01.18! 03 ! NEUE FUNKT. DRAS0046/47   !TH.WILLIG *
      * FAS00330!06.02.18! 04 ! NEUE FUNKT. DRAS0048/49   !TH.WILLIG *
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
       01  PI-UPAS0009.
           05  PI-UPAS0009-FUNKTION            PIC  X(008).
               88 PI-UPAS0009-DRAS0035              VALUE 'DRAS0035'.
               88 PI-UPAS0009-DRAS0045              VALUE 'DRAS0045'.
100118         88 PI-UPAS0009-DRAS0046              VALUE 'DRAS0046'.
100118         88 PI-UPAS0009-DRAS0047              VALUE 'DRAS0047'.
060218         88 PI-UPAS0009-DRAS0048              VALUE 'DRAS0048'.
060218         88 PI-UPAS0009-DRAS0049              VALUE 'DRAS0049'.

      ******* ENDE COPY-MEMBER **************************************

