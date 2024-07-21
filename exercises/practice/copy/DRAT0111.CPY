      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0111.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0111.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Nov 1998 16:15:26   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 07 1997 09:17:40   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:17:58   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0111  KONSTANTEN FÜR DTA-ART    KTY = 101*
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    05.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !05.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * AFPA....!30.01.97! 02 ! K-KNR-DTA-ART-PEW-EXPORT  !BOHLINGER *
      * AFPA0012!13.08.97! 03 ! K-KNR-DTA-ART-MANUELL     !SCHRIEBER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-DTA-ART-ANTRAG-TDS PIC S9(004) BINARY SYNC
                                                   VALUE  1.
           05 K-KNR-DTA-ART-PROVISION  PIC S9(004) BINARY SYNC
                                                   VALUE  2.
           05 K-KNR-DTA-ART-BESTAND-PG PIC S9(004) BINARY SYNC
                                                   VALUE  3.
           05 K-KNR-DTA-ART-BESTAND-VG PIC S9(004) BINARY SYNC
                                                   VALUE  4.
           05 K-KNR-DTA-ART-PEW-EXPORT PIC S9(004) BINARY SYNC
                                                   VALUE  5.
           05 K-KNR-DTA-ART-MANUELL    PIC S9(004) BINARY SYNC
                                                   VALUE  6.
