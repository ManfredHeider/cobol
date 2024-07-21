      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0914.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRVB0914.CPv  $
      *
      *   Rev 2.8   Nov 20 2012 14:49:42   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.7   Nov 20 2012 13:28:18   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.6   Nov 20 2012 12:44:54   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.5   Nov 20 2012 12:31:44   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.4   Nov 20 2012 12:07:34   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.3   Nov 20 2012 11:56:34   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.2   Nov 20 2012 11:32:34   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.1   Sep 10 2008 11:51:28   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Batch)
      *
      *   Rev 2.0   Aug 12 2008 08:14:52   t13ex43
      *Archiverstellung
      *
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0914                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0115                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.08.2008                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!12.08.08!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00466!05.10.12!2.2 ! NEUE FUNKTION DRVB1064    ! TH.WILLIG *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************
      *
       01  PI-ZWVB0115.
           05  PI-ZWVB0115-FUNKTION            PIC  X(008).
               88 PI-ZWVB0115-DRVB0915              VALUE 'DRVB0915'.
               88 PI-ZWVB0115-DRVB1064              VALUE 'DRVB1064'.
      *
      ******* ENDE COPY-MEMBER ****************************************
      *
