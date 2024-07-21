      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0068.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0068.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:00:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:24   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:14:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    MITARBEITER                                                *
      *                                                               *
      *****************************************************************
      *    VERSION   1    31.03.94 18:12:47
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AKI00001!10.07.98!01.01! NEU                       !Zgur-Vujic
      * --------!--------!-----!---------------------------!---------*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
        05  WB-NI-MITARBEITER-TAB.
           10 WB-NI-MITARBEITER-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05  WB-NI-MITARBEITER REDEFINES WB-NI-MITARBEITER-TAB.
           10 WB-NI-MITARBPERSONENID      PIC S9(4) COMP.
           10 WB-NI-MITARBPERSROLLE       PIC S9(4) COMP.
           10 WB-NI-MITARBVORGESPERSID    PIC S9(4) COMP.
           10 WB-NI-MITARBVORGESROLLE     PIC S9(4) COMP.
           10 WB-NI-MITARBEITERNR         PIC S9(4) COMP.
        05  DCL-MITARBEITER.
           10 WB-MITARBPERSONENID         PIC X(13).
           10 WB-MITARBPERSROLLE          PIC X(01).
           10 WB-MITARBVORGESPERSID       PIC X(13).
           10 WB-MITARBVORGESROLLE        PIC X(01).
           10 WB-MITARBEITERNR            PIC S9(9) COMP.
      ******************************************************************
