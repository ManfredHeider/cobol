      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0590.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0590.CPv  $
      *
      *   Rev 2.1   15 Feb 2007 11:19:16   t13ae46
      *AAN00655: Erweiterung des PS auf 3 Stellen - JWA 
      *
      *   Rev 2.0   Feb 27 1999 14:29:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:04:28   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   22 Oct 1997 13:13:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0590  Funktionen Standard-OZP            *
      *                           MPIN TBPA0124                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: E.Schrieber                                     *
      *    DATUM:    03.11.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.11.97! 01 ! Neuerstellung             ! Schrieber*
      * AAN00655!09.02.07! 2.1! NEU DRPA1661              ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0124.
           05 PI-ZTPA0124-FUNKTION            PIC X(008).
              88 PI-ZTPA0124-DRPA0591-SEL-ALL     VALUE 'DRPA0591'.
090207        88 PI-ZTPA0124-DRPA1661-UPD         VALUE 'DRPA1661'.
