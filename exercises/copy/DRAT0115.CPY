      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0115.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0115.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 07 1997 09:17:56   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 12:18:28   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0115  KONSTANTEN FÜR                     *
      *                           Konditionsregeln         KTY = 104 *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    10.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-REGEL-P-KEINE    PIC S9(004) BINARY SYNC VALUE 100.
           05 K-KNR-REGEL-P-VG-BTG   PIC S9(004) BINARY SYNC VALUE 101.
           05 K-KNR-REGEL-E-KEINE    PIC S9(004) BINARY SYNC VALUE 200.
           05 K-KNR-REGEL-E-GP-BEZUG PIC S9(004) BINARY SYNC VALUE 201.
           05 K-KNR-REGEL-E-GS-BEZUG PIC S9(004) BINARY SYNC VALUE 202.
           05 K-KNR-REGEL-K-KEINE    PIC S9(004) BINARY SYNC VALUE 300.
           05 K-KNR-REGEL-K-GP-BEZUG PIC S9(004) BINARY SYNC VALUE 301.
           05 K-KNR-REGEL-K-NP-BEZUG PIC S9(004) BINARY SYNC VALUE 302.
           05 K-KNR-REGEL-H-KEINE    PIC S9(004) BINARY SYNC VALUE 400.
