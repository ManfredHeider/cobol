      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0157.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0157.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 09:22:24   T13EX16
      *APA00032 - Umstellung Konditionskonzept
      *
      *   Rev 1.0   18 Aug 1997 10:27:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0157  KONSTANTEN FÜR     KTY = 121       *
      *                 Konditions-Regel Einheiten                   *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.09.97! 01 ! NEUERSTELLUNG             ! Auerbach *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *  KNR  REGEL                                                  *
      *  ----------------------------------                          *
      *    1  Keine Einheiten                                        *
      *    2  Grundprovision ist die Bezugsgröße                     *
      *    3  Gesamtprovision ist die Bezugsgröße                    *
      *                                                              *
      ****************************************************************

       01  FILLER.
           05 K-KNR-K-REGEL-E-KEINE          PIC S9(004) BINARY VALUE 1.
           05 K-KNR-K-REGEL-E-GRUND-P-BEZUG  PIC S9(004) BINARY VALUE 2.
           05 K-KNR-K-REGEL-E-GES-P-BEZUG    PIC S9(004) BINARY VALUE 3.
