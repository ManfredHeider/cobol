      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0061.CPv  $
      *
      *   Rev 2.1   Aug 02 1999 10:43:44   T13AE34
      *AAT00010-Neukonzeption KS10
      *
      *   Rev 2.0   Feb 27 1999 11:57:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:46:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DRAT0061  SATZDEFINITION BENUTZERDATEI-2      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   2    26.08.94 16:38:40
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: WIECHERS                                         * 00060000
      *    DATUM:    26.08.1994                                       * 00070000
      *    AUFTRAG   AFRZ0013 - UMSTELLUNG KUS / RACF                 * 00080000
      *                                                               * 00080100
      ***************************************************************** 00084000
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00010!16.07.99!2.1 ! NEU: K22-BENUTZERPROFIL   ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  K22-AT0104S.
           02 K22-KEY.
              03 K22-USERID        PIC X(7).
              03 K22-TRANS         PIC X(6).
              03 K22-TRANS-R       REDEFINES K22-TRANS
                                   PIC 9(6).
           02 K22-VP1              PIC 999.
           02 K22-VP2              PIC X(26).
160799*    02 K22-RESERVE          PIC X(08).
160799     02 K22-RESERVE.
160799        03 K22-BENUTZERPROFIL   PIC X(01).
160799           88 K22-GUELTIGES-PROFIL VALUE 'A' THRU 'Z'.
160799        03 FILLER               PIC X(07).
