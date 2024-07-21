      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0222.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0222.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Nov 1998 09:07:30   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   03 Dec 1997 15:47:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0222  KONSTANTEN FÜR ÄNDERUNGSTYP        *
      *                           KTY-NR 151                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    30.01.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !30.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-ATYP-INSERT        PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-ATYP-UPDATE        PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-ATYP-DELETE        PIC S9(004) BINARY SYNC
                                                   VALUE  3.
