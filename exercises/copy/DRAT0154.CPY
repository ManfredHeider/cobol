      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0154.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0154.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:25:28   T13AE34
      *AAN00167 -> AAT00001
      *
      *   Rev 1.0   18 Aug 1997 10:27:14   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0154  KONSTANTEN FÜR     KTY = 127       *
      *                 PRODUKTGRUPPENART ZWEIGTEXT AUS KUNDENBEREICH*
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00001!22.10.97! 01 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  KNR  ZWEIGTEXT                                              *
      *  ----------------------------------                          *
      *    1  H    - HAFTPFLICHT                                     *
      *    2  U    - UNFALL                                          *
      *    3  S    - SACH                                            *
      *                                                              *
      ****************************************************************

       01  FILLER.
           05 K-KNR-ZWEIGTEXT-HAFT         PIC S9(004) BINARY VALUE  1.
           05 K-KNR-ZWEIGTEXT-UNFALL       PIC S9(004) BINARY VALUE  2.
           05 K-KNR-ZWEIGTEXT-SACH         PIC S9(004) BINARY VALUE  3.
