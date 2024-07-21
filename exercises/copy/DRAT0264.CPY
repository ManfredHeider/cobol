      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0264.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAT0264.CPv  $
      *
      *   Rev 2.1   Mar 09 1999 11:58:20   T13AE41
      *Daten recovert
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 08:38:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *
      ****************************************************************
      *    COPY-MEMBER: DRAT0264  KONSTANTEN FÜR ZUSTAND             *
      *                           EINHEITENMAXIMIERUNG               *
      *                           KTY-NR 154                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    02.09.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.09.98! 01 ! NEUERSTELLUNG             ! D.WILHELM*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-GRUNDZUSTAND       PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-EINH-MAX-DURCHF    PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-EINH-MAX-N-DURCHF  PIC S9(004) BINARY SYNC
                                                   VALUE  3.

