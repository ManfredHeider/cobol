      *---------------------------------------------------------------*
      *    Standard f³r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0253.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0253.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:24:22   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   27 Feb 1998 10:16:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0253  FUNKTIONEN                         *
      *                           ZTAT0024-MASSENDELETE              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    26.02.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!06.03.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DRAT0253                                                    *
      *  - MASSENDELETE DER RESTARTINFORMATIONEN                     *
      *                                                              *
      ****************************************************************
       01  PI-DRAT0253.
           05 PI-DRAT0253-KEY.
              10 PI-DRAT0253-JOBNAME       PIC X(008).
              10 PI-DRAT0253-PROGRAMMNAME  PIC X(008).
       01 PO-DRAT0253.
          05 PO-DRAT0253-ANZAHL-GEL-ZEILEN    PIC S9(009)  COMP.
          05 PO-DRAT0253-RC                   PIC  X(003).
             88 PO-DRAT0253-RC-OK                         VALUE 'OK '.
             88 PO-DRAT0253-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPYMEMBER *****************************************

