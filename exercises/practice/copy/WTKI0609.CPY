      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0609.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0609.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:00:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:32   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:01:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    BENUTZER                                                   *
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
        05  WB-NI-BENUTZER-TAB.
           10 WB-NI-BENUTZER-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05  WB-NI-BENUTZER REDEFINES WB-NI-BENUTZER-TAB.
           10 WB-NI-BENMITAROLLENID       PIC S9(4) COMP.
           10 WB-NI-BENMITAROLLENKZ       PIC S9(4) COMP.
           10 WB-NI-RACFID                PIC S9(4) COMP.
           10 WB-NI-BENKENNWORT           PIC S9(4) COMP.
           10 WB-NI-BENART                PIC S9(4) COMP.
           10 WB-NI-LETZTEAENDERUNG       PIC S9(4) COMP.
        05  DCL-BENUTZER.
           10 WB-BENMITAROLLENID          PIC X(13).
           10 WB-BENMITAROLLENKZ          PIC X(01).
           10 WB-RACFID                   PIC X(8).
           10 WB-BENKENNWORT              PIC X(15).
           10 WB-BENART                   PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
