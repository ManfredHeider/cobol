      ****************************************************************
      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0247.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0247.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:12   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   03 Dec 1997 15:49:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0247  FUNKTIONEN                         *
      *                           RESTART-FUNKTIONEN                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    24.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!24.02.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DRAT0248                                                    *
      *  - PROGRAMM ANMELDEN                                         *
      *                                                              *
      *  DRAT0249                                                    *
      *  - PROGRAMM ABMELDEN                                         *
      *                                                              *
      *  DRAT0250                                                    *
      *  - VERARBEITUNGSBLOCK ABMELDEN                               *
      *                                                              *
      *  DRAT0251                                                    *
      *  - RESTARTINFORMATIONEN LESEN                                *
      *                                                              *
      *  DRAT0252                                                    *
      *  - RESTARTINFORMATIONEN SCHREIBEN                            *
      *                                                              *
      ****************************************************************
       01  PI-UPAT0022.
           05 PI-UPAT0022-FUNKTION            PIC X(008).
              88 PI-UPAT0022-DRAT0248-PGM-ANM     VALUE 'DRAT0248'.
              88 PI-UPAT0022-DRAT0249-PGM-ABM     VALUE 'DRAT0249'.
              88 PI-UPAT0022-DRAT0250-VER-ABM     VALUE 'DRAT0250'.
              88 PI-UPAT0022-DRAT0251-RSTI-L      VALUE 'DRAT0251'.
              88 PI-UPAT0022-DRAT0252-RSTI-S      VALUE 'DRAT0252'.
      *
      ****** ENDE COPYMEMBER *****************************************

