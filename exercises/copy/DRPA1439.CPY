      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1439.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1439.CPv  $
      *
      *   Rev 2.2   07 Sep 2001 16:28:10   t13ex21
      *FPA00262 - Neue Funktion
      *
      *   Rev 2.1   05 Feb 2001 10:19:44   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 47
      *(BfG ImmoInvest)
      *
      *   Rev 2.0   12 Jul 2000 12:33:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1439  FUNKTIONEN STANDARD-OZP            *
      *                           PMPEH TBPA0132                     *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0132.
           05 PI-ZTPA0132-FUNKTION            PIC X(008).
              88 PI-ZTPA0132-DRPA1440-INS         VALUE 'DRPA1440'.
              88 PI-ZTPA0132-DRPA1441-UPD         VALUE 'DRPA1441'.
              88 PI-ZTPA0132-DRPA1442-DEL         VALUE 'DRPA1442'.
              88 PI-ZTPA0132-DRPA1443-SEL-ALL     VALUE 'DRPA1443'.
140801        88 PI-ZTPA0132-DRAT0228-COM         VALUE 'DRAT0228'.
