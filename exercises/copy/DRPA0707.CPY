      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0707.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/MF/CPY/DRPA0707.CPv  $
      *
      *   Rev 2.5   Apr 22 2003 08:37:56   t13ae46
      *APA00228: Neue Funktion DRPA1559
      *
      *   Rev 2.4   Feb 07 2000 12:03:10   T13EX22
      *APA00077: Neue Funktion
      *
      *   Rev 2.3   Sep 30 1999 08:42:18   T13EX21
      *FPA00070 - Neue Funktion
      *
      *   Rev 2.2   Jun 02 1999 11:17:48   T13EX22
      * 
      *
      *   Rev 2.1   May 21 1999 13:05:12   T13EX26
      *PNAP Änderungen u.a neue Funktion DRPA1240
      *
      *   Rev 2.0   May 11 1999 08:15:06   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:41:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0707                                      *
      *    --------------------------------------------------------   *
      *    Funktionen für das Lese-OZP ZWPA0105                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    12.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01! NEU                       ! Jäger   *
      *         !        !     !                           !         *
      * AFPA0012!10.08.98!01.02! NEUE FUNKTION DRPA1038    !SCHRIEBER*
      *         !        !     !                           !         *
      * APA00012!19.05.99! 2.0 ! NEUE FUNKTION DRPA1239    !PARENT   *
      * APA00012!19.05.99! 2.0 ! NEUE FUNKTION DRPA1248    !MUND     *
      * FPA00070!08.08.99!     ! NEUE FUNKTION DRPA0010    ! STORCH  *
      * APA00077!14.01.00! 2.3 ! NEUE FUNKTION DRPA1329    ! MUND    *
      * APA00228!26.03.03! 2.4 ! NEUE FUNKTION DRPA1559    ! WANG    *
      *         !        !     !                           !         *
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
       01  PI-ZWPA0105.
           05  PI-ZWPA0105-FUNKTION            PIC  X(008).
               88 PI-ZWPA0105-DRPA0702              VALUE 'DRPA0702'.
               88 PI-ZWPA0105-DRPA0635              VALUE 'DRPA0635'.
               88 PI-ZWPA0105-DRPA0709              VALUE 'DRPA0709'.
               88 PI-ZWPA0105-DRPA1038              VALUE 'DRPA1038'.
190599         88 PI-ZWPA0105-DRPA1239              VALUE 'DRPA1239'.
310599         88 PI-ZWPA0105-DRPA1248              VALUE 'DRPA1248'.
080999         88 PI-ZWPA0105-DRPA0010              VALUE 'DRPA0010'.
140100         88 PI-ZWPA0105-DRPA1329              VALUE 'DRPA1329'.
260303         88 PI-ZWPA0105-DRPA1559              VALUE 'DRPA1559'.
      *
      ******* ENDE COPY-MEMBER **************************************
