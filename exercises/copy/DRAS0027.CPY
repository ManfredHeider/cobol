      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAS0027.CPv  $
      *
      *   Rev 2.1   Jul 07 2010 14:43:42   t13ae36
      *ASL00351: Columbusbonus - 300EEG-Regel
      *
      *   Rev 2.0   Jul 01 2010 09:38:56   t13ae36
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRAS0027                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Service-Modul UPAS0006               *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Franke                                           *
      *    DATUM:    02.07.2010                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00351!02.07.10! 01 ! NEU                       ! Franke   *
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
       01  PI-UPAS0006.
           05  PI-UPAS0006-FUNKTION            PIC  X(008).
               88 PI-UPAS0006-DRAS0028              VALUE 'DRAS0028'.
      *
      *---------------------------------------------------------------*
