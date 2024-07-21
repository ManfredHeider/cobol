      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0236.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0236.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   08 Aug 1998 15:32:42   T13AE34
      *AAN00167 - wg. ZJPA0021
      *
      *   Rev 1.2   07 Aug 1998 11:30:32   T13AE34
      *AAN00167 - AAT00001
      *
      *   Rev 1.1   04 Aug 1997 10:09:06   T13EX22
      *AFPA0010 - Neuerstellung Funktionen ZJPA0021
      *
      *   Rev 1.0   Feb 19 1997 12:13:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0236  FUNKTION "PRODUKTGRUPPEN ERMITTELN"*
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    30.06.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!30.06.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      * AFPA0023!21.10.97! 01 ! FKT. DRPA0556             ! HEIDER   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZJPA0021.
           05 PI-ZJPA0021-FUNKTION            PIC X(008).
              88 PI-ZJPA0021-DRPA0249             VALUE 'DRPA0249'.
              88 PI-ZJPA0021-DRPA0250             VALUE 'DRPA0250'.
              88 PI-ZJPA0021-DRPA0251             VALUE 'DRPA0251'.
