      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.11.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1619.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1619.CPv  $
      *
      *   Rev 2.2   Apr 11 2008 10:36:08   t13ex48
      *FKI01150: Korrektur des KD-Übertragungs-Upros für Provisions
      *-Dialog
      *
      *   Rev 2.1   Mar 15 2004 15:43:28   T13EX39
      *FKI00388.UE001 NEU DAISY
      *
      *   Rev 2.0   Nov 27 2003 13:51:16   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1619                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FüR DAS UPRO UPKI0164                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN       *
      *    --------------------------------------------------------  *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00388!01.12.03! 1.0! NEU                       ! HORNUNG  *
      * -------------------------------------------------------------*
100408* FKI01150!10.04.08! 2.0! UPKI0164 durch UPKI0214   ! HSE      *
      *         !        !    ! ersetzt                   !          *
      * --------!--------!----!---------------------------!----------*
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
100408*
100408*01  PI-UPKI0164.
100408*    05  PI-UPKI0164-FUNKTION            PIC  X(008).
100408*
100408*      88   PI-UPKI0164-DRKI1620     VALUE 'DRKI1620'.
100408 01  PI-UPKI0214.
100408     05  PI-UPKI0214-FUNKTION            PIC  X(008).
100408
100408       88   PI-UPKI0214-DRKI1620     VALUE 'DRKI1620'.
      *
      ******* ENDE COPY-MEMBER **************************************

