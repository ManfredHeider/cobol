      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0034.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0034.CPv  $
      *
      *   Rev 2.1   07 Sep 2001 16:26:10   t13ex21
      *FPA00262 - Neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 14:27:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.6   05 Aug 1998 18:36:36   T13EX22
      *APAA0032 - PEW Release 2.0
      *
      *   Rev 1.5   20 Oct 1997 17:27:06   T13EX22
      *AFPA0018 - Funktionen Standard OZP PEWA
      *
      *   Rev 1.4   05 Jun 1997 08:01:44   T13AE31
      *AFPA0017 - Neue Funktion
      *
      *   Rev 1.3   Apr 11 1997 15:58:04   T13AE31
      *No change.
      *
      *   Rev 1.2   Apr 09 1997 16:47:06   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.1   Mar 06 1997 14:49:48   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    Copy-Member: DRPA0034  Funktionen Standard-OZP            *
      *                           PEWA TBPA0011                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !20.08.96! 01 ! Neuerstellung             ! Priggen  *
      * AFPA00..!30.01.97! 02 ! FKT DRPA0198-UPD-VSTAT    ! BOHLINGER*
      * AFPA0017!12.03.97! 03 ! FKT DRPA0237-UPD-VSTAT    ! A.CECCONI*
      *         !        !    ! (ANDERE FKT ALS DRPA0198!)!          *
      * AFPA0017!28.05.97! 04 ! FKT DRPA0302-UPD-WVDAT    ! A.CECCONI*
      * AFPA00..!05.08.97!    ! FKT DRPA0342-ENTSORGEN    ! T. MUND  *
      * AFPA0012!10.12.97!    ! FKT DRPA0670-UPD-ZUST     ! T. MUND  *
      * FPA00262!14.08.01!2.01! NEU: COMMIT-FUNKTION      ! STORCH   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0011.
           05 PI-ZTPA0011-FUNKTION            PIC X(008).
              88 PI-ZTPA0011-DRPA0070-INS         VALUE 'DRPA0070'.
              88 PI-ZTPA0011-DRPA0071-UPD         VALUE 'DRPA0071'.
              88 PI-ZTPA0011-DRPA0072-DEL         VALUE 'DRPA0072'.
              88 PI-ZTPA0011-DRPA0073-SEL-ALL     VALUE 'DRPA0073'.
              88 PI-ZTPA0011-DRPA0198-UPD-VST     VALUE 'DRPA0198'.
              88 PI-ZTPA0011-DRPA0238-UPD-VSTAT   VALUE 'DRPA0238'.
              88 PI-ZTPA0011-DRPA0302-UPD-WVDAT   VALUE 'DRPA0302'.
              88 PI-ZTPA0011-DRPA0342-ENTSORGEN   VALUE 'DRPA0342'.
              88 PI-ZTPA0011-DRPA0670-UPD-ZUST    VALUE 'DRPA0670'.
140801        88 PI-ZTPA0011-DRAT0228-COM         VALUE 'DRAT0228'.
