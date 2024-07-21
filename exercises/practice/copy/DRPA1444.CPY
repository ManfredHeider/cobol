      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1444.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1444.CPv  $
      *
      *   Rev 2.2   07 Sep 2001 16:28:30   t13ex21
      *FPA00262 - Neue Funktion
      *
      *   Rev 2.1   05 Feb 2001 10:19:56   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 47
      *(BfG ImmoInvest)
      *
      *   Rev 2.0   12 Jul 2000 12:33:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1433  FUNKTIONEN STANDARD-OZP            *
      *                           PMPPH TBPA0133                     *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: A. BRENNER                                      *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01! 01 ! NEUERSTELLUNG             ! BRENNER  *
      * FPA00262!14.08.01!2.02! NEU: COMMIT-FUNKTION      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0133.
           05 PI-ZTPA0133-FUNKTION            PIC X(008).
              88 PI-ZTPA0133-DRPA1445-INS         VALUE 'DRPA1445'.
              88 PI-ZTPA0133-DRPA1446-UPD         VALUE 'DRPA1446'.
              88 PI-ZTPA0133-DRPA1447-DEL         VALUE 'DRPA1447'.
              88 PI-ZTPA0133-DRPA1448-SEL-ALL     VALUE 'DRPA1448'.
140801        88 PI-ZTPA0133-DRAT0228-COM         VALUE 'DRAT0228'.
