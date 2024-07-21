      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1080.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1080.CPv  $
      *
      *   Rev 2.2   Jun 24 2019 12:52:56   t13ae30
      *AVB01556: Allfinanz 1 & 2: Anpassung Beförderungsprüfung Str
      *uktur (VB-System)
      *
      *   Rev 2.1   Jun 03 2015 12:40:46   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VBI
      *
      *   Rev 2.0   Mar 03 2015 10:25:10   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1080                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0037                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    04.03.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB01332!04.03.15!2.1 ! NEU                       ! S. Walb  *
      * --------+--------+----+---------------------------+----------*
      * AVB01556!28.05.19!2.2 ! NEUE FUNKTION DRVB1215    !TH.WILLIG *
      * --------+--------+----+---------------------------+----------*
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
       01  PI-ZJVB0037.
           05 PI-ZJVB0037-FUNKTION            PIC X(008).
              88 PI-ZJVB0037-DRVB1081             VALUE 'DRVB1081'.
280519        88 PI-ZJVB0037-DRVB1215             VALUE 'DRVB1215'.


