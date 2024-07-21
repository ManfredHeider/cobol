      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0537.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0537.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:10:50   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.2   08 Aug 1998 16:14:16   P13SP13
      *Neuerstellung
      *
      *   Rev 1.1   04 Aug 1998 14:10:44   T13EX28
      *APA00032 - Uebergabe Konditionsupro fuer PEW Release 2.0
      *
      *   Rev 1.0   12 Sep 1997 17:00:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0537                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS OZP  ZJPA0022                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    21.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97!01.01!NEU                       ! BOHLINGER*
      *         !31.10.97!01.02!NEUE FUNKTION DRPA0586    ! BOHLINGER*
      * APA00032!08.08.98!02.00! ZJPA0013 umbenannt in    !          *
      *         !        !     ! ZJPA0022 wegen Probleme  ! AUERBACH *
      *         !        !     ! prodübergabe             !          *
      *         !05.11.98!03.00!NEUE FUNKTION DRPA1120    ! BOHLINGER*
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZJPA0022.
           05  PI-ZJPA0022-FUNKTION            PIC  X(008).
               88 PI-ZJPA0022-DRPA0538              VALUE 'DRPA0538'.
               88 PI-ZJPA0022-DRPA0544              VALUE 'DRPA0544'.
311097         88 PI-ZJPA0022-DRPA0586              VALUE 'DRPA0586'.
051198         88 PI-ZJPA0022-DRPA1120              VALUE 'DRPA1120'.
      *
      ******* ENDE COPY-MEMBER **************************************
