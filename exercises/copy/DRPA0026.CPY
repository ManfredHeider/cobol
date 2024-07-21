      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0026.CPv  $
      *
      *   Rev 2.1   07 Sep 2001 16:23:08   t13ex21
      *FPA00262 - Neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 14:27:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.4   05 Jun 1997 08:01:04   T13AE31
      *AFPA0017 - Neue Funktion
      *
      *   Rev 1.3   Apr 11 1997 15:57:26   T13AE31
      *No change.
      *
      *   Rev 1.2   Apr 09 1997 16:46:48   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.1   Mar 06 1997 14:48:42   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0026  FUNKTIONEN STANDARD-OZP            *
      *                           PEWP TBPA0002                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                         *
      *    DATUM:    14.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !14.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * AFPA0017!12.03.97! 02 ! FKT DRPA0239-UPD-VSTAT    ! CECCONI  *
      * AFPA0017!18.04.97! 03 ! FKT DRPA0257-UPD-RMELD    ! CECCONI  *
      * FPA00262!14.08.01!2.01! NEU: COMMIT-FUNKTION      ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0002.
           05 PI-ZTPA0002-FUNKTION            PIC X(008).
              88 PI-ZTPA0002-DRPA0023-INS         VALUE 'DRPA0023'.
              88 PI-ZTPA0002-DRPA0022-UPD         VALUE 'DRPA0022'.
              88 PI-ZTPA0002-DRPA0024-DEL         VALUE 'DRPA0024'.
              88 PI-ZTPA0002-DRPA0021-SEL-ALL     VALUE 'DRPA0021'.
              88 PI-ZTPA0002-DRPA0239-UPD-VSTAT   VALUE 'DRPA0239'.
              88 PI-ZTPA0002-DRPA0257-UPD-RMELD   VALUE 'DRPA0257'.
140801        88 PI-ZTPA0002-DRAT0228-COM         VALUE 'DRAT0228'.
