      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0785.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0785.CPv  $
      *
      *   Rev 2.4   May 22 2014 10:19:42   t13ae13
      *ASL00509: FAP Dialogänderung Vb 31
      *
      *   Rev 2.3   Aug 19 2008 11:34:06   t13ex13
      *ASL00304: Familien-Absicherungsplan
      *
      *   Rev 2.2   31 Jul 2006 15:38:04   t13ex13
      *FSL00184: Korrektur Versicherungssummen: RGT
      *
      *   Rev 2.1   Jul 05 2005 14:07:16   t13ex43
      *ASL00206: PGM 2 - Familien-Absicherungs-Plan
      *
      *   Rev 2.0   Jun 27 2005 10:03:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0785                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0027                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.06.2005                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00206!27.06.05!2.1 ! NEU                       ! KLEEMEIER *
      * FSL00184!25.07.06!2.2 ! RÜCKGABE AKTUELLSTES TUPEL!GUTFLEISCH *
      * ASL00184!27.06.08!2.3 ! Neues Tupel (+Beitrag)    !GUTFLEISCH *
      * ASL00509!04.02.14!2.4 ! Neues Tupel für ZVVBD027  ! S. Walb   *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZWVB0027.
           05  PI-ZWVB0027-FUNKTION            PIC  X(008).
               88 PI-ZWVB0027-DRVB0786              VALUE 'DRVB0786'.
               88 PI-ZWVB0027-DRVB0829              VALUE 'DRVB0829'.
               88 PI-ZWVB0027-DRVB0906              VALUE 'DRVB0906'.
               88 PI-ZWVB0027-DRVB1073              VALUE 'DRVB1073'.
      *
      ******* ENDE COPY-MEMBER **************************************
      *
