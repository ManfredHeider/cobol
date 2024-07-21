      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0196.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0196.CPv  $
      *
      *   Rev 2.1   Feb 03 2004 11:44:48   t13ae34
      *FAN00485-Korrektur PEWA im Batch; hier: Vstatus '110'
      *
      *   Rev 2.0   Feb 27 1999 14:28:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   20 Oct 1997 17:29:22   T13EX22
      *AFPA0018 - Funktionen fuer List-OZP PEWA
      *
      *   Rev 1.0   Feb 19 1997 12:08:20   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0196  FUNKTIONEN LIST-OZP                *
      *                           PEWA TBPA0011                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPANNNN!30.01.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      * AFPA0018!07.08.97!    ! FUNKTION  DRPA0340        ! T. MUND  *
      * FAN00485!03.02.04!2.1 ! FUNKTION  DRPA1568        ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZXPA0011.
           05 PI-ZXPA0011-FUNKTION            PIC X(008).
              88 PI-ZXPA0011-DRPA0197             VALUE 'DRPA0197'.
              88 PI-ZXPA0011-DRPA0202             VALUE 'DRPA0202'.
              88 PI-ZXPA0011-DRPA0340             VALUE 'DRPA0340'.
030204        88 PI-ZXPA0011-DRPA1568             VALUE 'DRPA1568'.
