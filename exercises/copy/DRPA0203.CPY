      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0203.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0203.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   05 Jun 1997 08:02:24   T13AE31
      *AFPA0017 - Neue Funktion
      *
      *   Rev 1.1   Apr 09 1997 16:51:30   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:09:10   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0203                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0002 --> LIST-OZP-PEWP      *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BORK                                             *
      *    DATUM:    24.01.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.01.97! 01 ! NEU                       ! BORK     *
      *         !30.01.97! 01 ! FKT: DRPA0206   NEU       ! BOHLINGER*
      * AFPA0017!17.03.97! 01 ! FKT: DRPA0241   NEU       ! CECCONI  *
      *         !15.05.97!    ! FKT: DRPA0286   NEU       ! CECCONI  *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT NUR DIE FUNKTIONEN, DIE
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *
      ****************************************************************
      *
       01  PI-ZXPA0002.
           05  PI-ZXPA0002-FUNKTION            PIC  X(008).
               88 PI-ZXPA0002-DRPA0204              VALUE 'DRPA0204'.
               88 PI-ZXPA0002-DRPA0206              VALUE 'DRPA0206'.
               88 PI-ZXPA0002-DRPA0241              VALUE 'DRPA0241'.
               88 PI-ZXPA0002-DRPA0286              VALUE 'DRPA0286'.
      *
      ******* ENDE COPY-MEMBER **************************************
