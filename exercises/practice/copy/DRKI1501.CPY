      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1501.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1501.CPv  $
      *
      *   Rev 2.1   Mar 14 2003 13:22:32   T13EX39
      *FKI00694 zus. Protokoll-Datei
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
      *    COPY-MEMBER: DRKI1501                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:    8.01.2001                                        *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00694!21.01.03! 01 ! NEU                       ! HORNUNG  *
      *         !        !    ! Select mit CON_COD_ID     !          *
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
       01  PI-ZWKI0816.
           05  PI-ZWKI0816-FUNKTION            PIC  X(008).
               88 PI-ZWKI0816-DRKI1502              VALUE 'DRKI1502'.
      *
      ******* ENDE COPY-MEMBER DRKI1501 *****************************
