      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTKI0043.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTKI0043.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 13:00:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:56:14   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   18 Aug 1997 11:12:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE               *
      *                                                               *
      *    PERSON                                                     *
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
        05  WB-NI-PERSON-TAB.
           10 WB-NI-PERSON-ELE PIC S9(04) COMP OCCURS 14 TIMES.
        05  WB-NI-PERSON REDEFINES WB-NI-PERSON-TAB.
           10 WB-NI-PERSONENID            PIC S9(4) COMP.
           10 WB-NI-PERSONANREDEID        PIC S9(4) COMP.
           10 WB-NI-PERSONKUNDENART       PIC S9(4) COMP.
           10 WB-NI-PERSONANZNAME         PIC S9(4) COMP.
           10 WB-NI-PERSONANZVORNAME      PIC S9(4) COMP.
           10 WB-NI-PERSONSUCHNAME        PIC S9(4) COMP.
           10 WB-NI-PERSONSUCHVORNAME     PIC S9(4) COMP.
           10 WB-NI-PERSONNAMEPHONCODE PIC S9(4) COMP.
           10 WB-NI-PERSONGEBGRUENDDAT PIC S9(4) COMP.
           10 WB-NI-PERSONNOTIZID         PIC S9(4) COMP.
           10 WB-NI-PERSONEMPFOHLVON      PIC S9(4) COMP.
           10 WB-NI-PERSONSTEUERBERAT     PIC S9(4) COMP.
           10 WB-NI-PERSONSTEUERBTELEF PIC S9(4) COMP.
           10 WB-NI-LETZTEAENDERUNG       PIC S9(4) COMP.
        05  DCL-WB-PERSON.
           10 WB-PERSONENID               PIC X(13).
           10 WB-PERSONANREDEID           PIC S9(4) COMP.
           10 WB-PERSONKUNDENART          PIC X(01).
           10 WB-PERSONANZNAME            PIC X(30).
           10 WB-PERSONANZVORNAME         PIC X(15).
           10 WB-PERSONSUCHNAME           PIC X(15).
           10 WB-PERSONSUCHVORNAME        PIC X(15).
           10 WB-PERSONNAMEPHONCODE       PIC X(15).
           10 WB-PERSONGEBGRUENDDAT       PIC X(10).
           10 WB-PERSONNOTIZID            PIC X(13).
           10 WB-PERSONEMPFOHLVON.
              49 WB-PERSONEMPFOHLVON-LEN  PIC S9(4) COMP.
              49 WB-PERSONEMPFOHLVON-TEXT PIC X(50).
           10 WB-PERSONSTEUERBERAT.
              49 WB-PERSONSTEUERBERAT-LEN PIC S9(4) COMP.
              49 WB-PERSONSTEUERBERAT-TEXT PIC X(50).
           10 WB-PERSONSTEUERBTELEF.
              49 WB-PERSONSTEUERBTELEF-LEN PIC S9(4) COMP.
              49 WB-PERSONSTEUERBTELEF-TEXT PIC X(20).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
