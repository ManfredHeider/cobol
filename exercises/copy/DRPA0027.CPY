      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0027.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:04:36   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:27:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:48:48   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0027  Funktionen Standard-OZP            *
      *                           PROD TBPA0004                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    14.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !14.08.96! 01 ! Neuerstellung             ! Priggen  *
      * APA00068!18.11.99!2.01!NEUE FUNKTION DRPA0922     ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0004.
           05 PI-ZTPA0004-FUNKTION            PIC X(008).
              88 PI-ZTPA0004-DRPA0042-INS         VALUE 'DRPA0042'.
              88 PI-ZTPA0004-DRPA0043-UPD         VALUE 'DRPA0043'.
              88 PI-ZTPA0004-DRPA0044-DEL         VALUE 'DRPA0044'.
              88 PI-ZTPA0004-DRPA0045-SEL-ALL     VALUE 'DRPA0045'.
181199        88 PI-ZTPA0004-DRPA0922             VALUE 'DRPA0922'.
