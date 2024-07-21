      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1504.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1504.CPv  $
      *
      *   Rev 2.2   Sep 18 2012 15:45:54   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Sonstige 
      *Programme
      *
      *   Rev 2.1   Jan 18 2007 11:22:38   t13ex14
      *FKI01032: Central Fehlerbereinigungen 
      *
      *   Rev 2.1   31 Jan 2001 14:25:54   T13EX39
      *AKI00068 KBI
      *
      *   Rev 1.1   08 Feb 1999 18:19:30   T13EX47
      *Funktionsauswahl fuer LIST-OZP ZXKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:31:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI1504                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:   15.02.2003                                        *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00697!15.02.03! 1.0! NEU                       ! WHO      *
      * AKI00886!29.11.11!2.2 ! Neue Kundenaktualisierung ! IFR      *
      *         !        !    ! neue Funktion             !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
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
       01  PI-ZWKI0345.
           05  PI-ZWKI0345-FUNKTION            PIC  X(008).
               88 PI-ZWKI0345-DRKI1505              VALUE 'DRKI1505'.
               88 PI-ZWKI0345-DRKI0540              VALUE 'DRKI0540'.
      *
      ******* ENDE COPY-MEMBER DRKI1501 *****************************
